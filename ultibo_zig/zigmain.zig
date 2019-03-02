const std = @import("std");
const bufPrint = std.fmt.bufPrint;
const ultibo = @cImport({
    @cInclude("ultibo/platform.h");
});

export fn zigmain(argc: u32, argv: [*][*]u8) i32 {
    if (work(argc, argv)) {
        return 0;
    }
    else |err| {
        return @errorToInt(err);
    }
}

fn work(argc: u32, argv: [*][*]u8) !void {
    sleep(2*1000);
    for (argv[0..argc]) |arg, i| {
        warn("zig command line argument {} is {s}\x00", i + 1, arg);
    }
    var i: i32 = 1;
    while (true) {
        warn("zig clock message {}\x00", i);
        if (i == 3) {
            return error.SomeError;
        }
        sleep(1*1000);
        i += 1;
    }
}

var warnBuf: [1024]u8 = undefined;
fn warn(comptime fmt: []const u8, args: ...) void {
    if (std.fmt.bufPrint(&warnBuf, fmt, args)) |warning| {
        ultibo.logging_output(warning.ptr);
    }
    else |_| {
    }
}

fn sleep(milliseconds: u32) void {
    var start = ultibo.get_tick_count();
    while (ultibo.get_tick_count() -% start < milliseconds) {
    }
}
