//! Program status (OSC 7501) that outlives a client.
//!
//! A status report is not screen state, so the snapshot sent to an attaching
//! client has no place for it. The daemon keeps the latest report of each
//! record here and sends them again after the snapshot.
//!
//! It also answers the support query (`OSC 7501 ; ?`) while no terminal is
//! attached. A program asks once, when it starts. Without a reply it sends no
//! reports for the rest of its run, even after a terminal attaches.
//!
//! Records are dropped the way the terminal on the other end drops them, so a
//! replay never brings back a report that terminal has already let go of:
//! `clear` removes a record and those beneath it, a full reset removes all, a
//! new prompt (OSC 133;A) removes `working` and `blocked`, and input from the
//! user removes `done` and `error`.
//!
//! Specification: https://www.superlogical.com/rex/docs/build/program-status

const std = @import("std");

/// The longest OSC body kept while scanning, everything between `ESC ]` and
/// the terminator. The specification limits a whole 7501 sequence to 4096
/// bytes. Longer sequences are skipped.
const max_osc_bytes = 4096;

/// The most records kept. The oldest report goes first.
const max_records = 64;

const Terminator = enum {
    bel,
    st,

    fn string(self: Terminator) []const u8 {
        return switch (self) {
            .bel => "\x07",
            .st => "\x1b\\",
        };
    }
};

const Record = struct {
    /// The report as the program wrote it, without `7501;` or the terminator.
    body: []u8,
    /// Points into `body`. Empty for the root record.
    id: []const u8,
    state: State,
};

const State = enum { idle, working, done, blocked, @"error", clear };

pub const Memory = struct {
    records: std.ArrayList(Record) = .empty,

    scan: enum { ground, escape, osc, osc_escape } = .ground,
    osc_buf: [max_osc_bytes]u8 = undefined,
    osc_len: usize = 0,
    osc_overflow: bool = false,

    pub fn deinit(self: *Memory, alloc: std.mem.Allocator) void {
        self.removeAll(alloc);
        self.records.deinit(alloc);
    }

    /// Reads what the program wrote. `reply` receives the answer to a support
    /// query. Pass null when a terminal is attached, since it answers itself.
    pub fn feed(
        self: *Memory,
        alloc: std.mem.Allocator,
        data: []const u8,
        reply: ?*std.ArrayList(u8),
    ) void {
        for (data) |c| switch (self.scan) {
            .ground => if (c == 0x1b) {
                self.scan = .escape;
            },
            .escape => switch (c) {
                ']' => {
                    self.scan = .osc;
                    self.osc_len = 0;
                    self.osc_overflow = false;
                },
                // RIS
                'c' => {
                    self.removeAll(alloc);
                    self.scan = .ground;
                },
                0x1b => {},
                else => self.scan = .ground,
            },
            .osc => switch (c) {
                0x07 => self.endOsc(alloc, .bel, reply),
                0x1b => self.scan = .osc_escape,
                // CAN and SUB cancel the sequence.
                0x18, 0x1a => self.scan = .ground,
                else => if (self.osc_len < max_osc_bytes) {
                    self.osc_buf[self.osc_len] = c;
                    self.osc_len += 1;
                } else {
                    self.osc_overflow = true;
                },
            },
            .osc_escape => switch (c) {
                '\\' => self.endOsc(alloc, .st, reply),
                // Any other escape sequence cancels the OSC and starts here.
                ']' => {
                    self.scan = .osc;
                    self.osc_len = 0;
                    self.osc_overflow = false;
                },
                'c' => {
                    self.removeAll(alloc);
                    self.scan = .ground;
                },
                0x1b => self.scan = .escape,
                else => self.scan = .ground,
            },
        };
    }

    /// The user typed, clicked or looked: the terminal on the other end has
    /// let go of finished work by now.
    pub fn input(self: *Memory, alloc: std.mem.Allocator) void {
        self.removeWhere(alloc, struct {
            fn f(r: Record) bool {
                return r.state == .done or r.state == .@"error";
            }
        }.f);
    }

    /// Writes every kept report, oldest first, as the program would.
    pub fn replay(self: *const Memory, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        for (self.records.items) |r| {
            try writer.print("\x1b]7501;{s}\x1b\\", .{r.body});
        }
    }

    fn endOsc(
        self: *Memory,
        alloc: std.mem.Allocator,
        terminator: Terminator,
        reply: ?*std.ArrayList(u8),
    ) void {
        self.scan = .ground;
        if (self.osc_overflow) return;
        const osc = self.osc_buf[0..self.osc_len];

        if (std.mem.startsWith(u8, osc, "7501;")) {
            const body = osc["7501;".len..];
            if (std.mem.eql(u8, body, "?")) {
                const out = reply orelse return;
                out.appendSlice(alloc, "\x1b]7501;?") catch return;
                out.appendSlice(alloc, terminator.string()) catch {};
                return;
            }
            self.report(alloc, body);
            return;
        }

        // A new prompt: whatever was working or waiting is gone.
        if (std.mem.eql(u8, osc, "133;A") or std.mem.startsWith(u8, osc, "133;A;")) {
            self.removeWhere(alloc, struct {
                fn f(r: Record) bool {
                    return r.state == .working or r.state == .blocked;
                }
            }.f);
        }
    }

    fn report(self: *Memory, alloc: std.mem.Allocator, body: []const u8) void {
        var state: ?State = null;
        var id: []const u8 = "";
        var fields = std.mem.splitScalar(u8, body, ':');
        while (fields.next()) |field| {
            const eq = std.mem.indexOfScalar(u8, field, '=') orelse continue;
            const key = field[0..eq];
            const value = field[eq + 1 ..];
            if (std.mem.eql(u8, key, "state")) {
                state = std.meta.stringToEnum(State, value) orelse return;
            } else if (std.mem.eql(u8, key, "id")) {
                id = value;
            }
        }
        const new_state = state orelse return;

        if (new_state == .clear) {
            if (id.len == 0) return self.removeAll(alloc);
            var i: usize = 0;
            while (i < self.records.items.len) {
                if (covers(id, self.records.items[i].id)) {
                    self.removeAt(alloc, i);
                } else i += 1;
            }
            return;
        }

        // Each report replaces its record and becomes the newest.
        for (self.records.items, 0..) |r, i| {
            if (std.mem.eql(u8, r.id, id)) {
                self.removeAt(alloc, i);
                break;
            }
        }
        if (self.records.items.len >= max_records) self.removeAt(alloc, 0);

        const copy = alloc.dupe(u8, body) catch return;
        const id_start = @intFromPtr(id.ptr) -% @intFromPtr(body.ptr);
        self.records.append(alloc, .{
            .body = copy,
            .id = if (id.len == 0) "" else copy[id_start..][0..id.len],
            .state = new_state,
        }) catch alloc.free(copy);
    }

    /// Whether clearing `id` removes the record `other`: itself and anything
    /// beneath it. Ids are paths separated by slashes.
    fn covers(id: []const u8, other: []const u8) bool {
        if (!std.mem.startsWith(u8, other, id)) return false;
        return other.len == id.len or other[id.len] == '/';
    }

    fn removeAt(self: *Memory, alloc: std.mem.Allocator, i: usize) void {
        alloc.free(self.records.orderedRemove(i).body);
    }

    fn removeAll(self: *Memory, alloc: std.mem.Allocator) void {
        for (self.records.items) |r| alloc.free(r.body);
        self.records.clearRetainingCapacity();
    }

    fn removeWhere(self: *Memory, alloc: std.mem.Allocator, comptime pred: fn (Record) bool) void {
        var i: usize = 0;
        while (i < self.records.items.len) {
            if (pred(self.records.items[i])) {
                self.removeAt(alloc, i);
            } else i += 1;
        }
    }
};

fn expectReplay(memory: *const Memory, expected: []const u8) !void {
    var buf: [2048]u8 = undefined;
    var writer: std.Io.Writer = .fixed(&buf);
    try memory.replay(&writer);
    try std.testing.expectEqualStrings(expected, writer.buffered());
}

test "keeps the latest report of each record" {
    const alloc = std.testing.allocator;
    var m: Memory = .{};
    defer m.deinit(alloc);

    m.feed(alloc, "text\x1b]7501;state=working:app=cargo\x1b\\more", null);
    m.feed(alloc, "\x1b]7501;state=working:id=build/a\x07", null);
    m.feed(alloc, "\x1b]7501;state=blocked:kind=permission:app=cargo\x07", null);

    try expectReplay(
        &m,
        "\x1b]7501;state=working:id=build/a\x1b\\" ++
            "\x1b]7501;state=blocked:kind=permission:app=cargo\x1b\\",
    );
}

test "a report split across reads" {
    const alloc = std.testing.allocator;
    var m: Memory = .{};
    defer m.deinit(alloc);

    m.feed(alloc, "\x1b", null);
    m.feed(alloc, "]75", null);
    m.feed(alloc, "01;state=do", null);
    m.feed(alloc, "ne\x1b", null);
    try expectReplay(&m, "");
    m.feed(alloc, "\\", null);
    try expectReplay(&m, "\x1b]7501;state=done\x1b\\");
}

test "clear removes a record and those beneath it" {
    const alloc = std.testing.allocator;
    var m: Memory = .{};
    defer m.deinit(alloc);

    m.feed(alloc, "\x1b]7501;state=working\x07", null);
    m.feed(alloc, "\x1b]7501;state=working:id=a\x07", null);
    m.feed(alloc, "\x1b]7501;state=working:id=a/b\x07", null);
    m.feed(alloc, "\x1b]7501;state=working:id=ab\x07", null);
    m.feed(alloc, "\x1b]7501;state=clear:id=a\x07", null);
    try expectReplay(
        &m,
        "\x1b]7501;state=working\x1b\\\x1b]7501;state=working:id=ab\x1b\\",
    );

    m.feed(alloc, "\x1b]7501;state=clear\x07", null);
    try expectReplay(&m, "");
}

test "a new prompt drops working and blocked, input drops done and error" {
    const alloc = std.testing.allocator;
    var m: Memory = .{};
    defer m.deinit(alloc);

    m.feed(alloc, "\x1b]7501;state=working:id=w\x07", null);
    m.feed(alloc, "\x1b]7501;state=blocked:id=b\x07", null);
    m.feed(alloc, "\x1b]7501;state=done:id=d\x07", null);
    m.feed(alloc, "\x1b]7501;state=error:id=e\x07", null);
    m.feed(alloc, "\x1b]7501;state=idle:id=i\x07", null);

    m.feed(alloc, "\x1b]133;B\x07", null);
    try std.testing.expectEqual(5, m.records.items.len);

    m.feed(alloc, "\x1b]133;A;redraw=0\x07", null);
    try expectReplay(
        &m,
        "\x1b]7501;state=done:id=d\x1b\\" ++
            "\x1b]7501;state=error:id=e\x1b\\" ++
            "\x1b]7501;state=idle:id=i\x1b\\",
    );

    m.input(alloc);
    try expectReplay(&m, "\x1b]7501;state=idle:id=i\x1b\\");
}

test "a full reset drops everything" {
    const alloc = std.testing.allocator;
    var m: Memory = .{};
    defer m.deinit(alloc);

    m.feed(alloc, "\x1b]7501;state=done\x07", null);
    m.feed(alloc, "\x1bc", null);
    try expectReplay(&m, "");
}

test "answers the support query only when asked to" {
    const alloc = std.testing.allocator;
    var m: Memory = .{};
    defer m.deinit(alloc);
    var reply: std.ArrayList(u8) = .empty;
    defer reply.deinit(alloc);

    m.feed(alloc, "\x1b]7501;?\x1b\\", null);
    try std.testing.expectEqual(0, reply.items.len);

    m.feed(alloc, "\x1b]7501;?\x1b\\\x1b[c", &reply);
    try std.testing.expectEqualStrings("\x1b]7501;?\x1b\\", reply.items);

    reply.clearRetainingCapacity();
    m.feed(alloc, "\x1b]7501;?\x07", &reply);
    try std.testing.expectEqualStrings("\x1b]7501;?\x07", reply.items);
    try expectReplay(&m, "");
}

test "ignores what is not a report" {
    const alloc = std.testing.allocator;
    var m: Memory = .{};
    defer m.deinit(alloc);

    // No state, an unknown state, a cancelled sequence, another OSC, and one
    // that is too long.
    m.feed(alloc, "\x1b]7501;app=x\x07", null);
    m.feed(alloc, "\x1b]7501;state=sleeping\x07", null);
    m.feed(alloc, "\x1b]7501;state=done\x18\x07", null);
    m.feed(alloc, "\x1b]75010;state=done\x07", null);
    m.feed(alloc, "\x1b]7501;state=done:msg=", null);
    for (0..max_osc_bytes) |_| m.feed(alloc, "A", null);
    m.feed(alloc, "\x07", null);
    try expectReplay(&m, "");

    // An OSC cut short by a new one.
    m.feed(alloc, "\x1b]0;title\x1b]7501;state=done\x07", null);
    try expectReplay(&m, "\x1b]7501;state=done\x1b\\");
}

test "keeps a bounded number of records" {
    const alloc = std.testing.allocator;
    var m: Memory = .{};
    defer m.deinit(alloc);

    var buf: [64]u8 = undefined;
    for (0..max_records + 5) |i| {
        const seq = try std.fmt.bufPrint(&buf, "\x1b]7501;state=idle:id=r{d}\x07", .{i});
        m.feed(alloc, seq, null);
    }
    try std.testing.expectEqual(max_records, m.records.items.len);
    try std.testing.expectEqualStrings("r5", m.records.items[0].id);
}
