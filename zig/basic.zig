const std = @import("std");
const common = @import("common.zig");

const Status = struct {
    min: i64,
    max: i64,
    total: i64,
    count: i64,
};

fn solution(path: []const u8) ![]const u8 {
    //   const stdout = std.io.getStdOut().writer();
    var allocator = std.heap.page_allocator;

    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;

    var hashmap = std.StringHashMap(Status).init(allocator);
    defer hashmap.deinit();

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var splitedIter = std.mem.split(u8, line, ";");

        var cityName: []const u8 = splitedIter.next().?;
        var measurement: i64 = try std.fmt.parseInt(i32, splitedIter.next().?, 10);

        const cityCopy = try allocator.alloc(u8, cityName.len);
        @memcpy(cityCopy, cityName);

        //  try stdout.print("{s} {d}\n", .{ cityName, measurement });

        if (hashmap.get(cityCopy)) |value| {
            var min = value.min;
            if (measurement < min) {
                min = measurement;
            }

            var max = value.max;
            if (measurement > max) {
                max = measurement;
            }

            var total = value.total + measurement;

            try hashmap.put(cityCopy, Status{
                .min = min,
                .max = max,
                .total = total,
                .count = value.count + 1,
            });
        } else {
            try hashmap.put(cityCopy, Status{
                .min = measurement,
                .max = measurement,
                .total = measurement,
                .count = 1,
            });
        }
    }

    var iterator = hashmap.iterator();

    var cityNames = std.ArrayList(*[]const u8).init(allocator);
    defer cityNames.deinit();

    while (iterator.next()) |entry| {
        var cityName = entry.key_ptr;

        try cityNames.append(cityName);
    }

    // sort
    //
    //

    return "asdf";
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    var file = try std.fs.cwd().openFile(common.OUTPUT_PATH, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buffer: [10024]u8 = undefined;

    var size = try in_stream.readAll(&buffer);
    var expectOutputs = buffer[0..size];

    const got = try solution(common.MEASUREMENTS_PATH);

    if (std.mem.eql(u8, got, expectOutputs)) {
        try stdout.print("Test passed\n", .{});
    } else {
        try stdout.print("Test failed\n", .{});
        try stdout.print("Expected\n{s}", .{expectOutputs});
        try stdout.print("Got\n{s}", .{got});
    }
    //    var expectOutputs = try file.readAllAlloc(allocator, 0);
}
