const std = @import("std");
const common = @import("common.zig");

const Status = struct {
    min: i64,
    max: i64,
    total: i64,
    count: i64,
};

fn compareStrings(_: void, lhs: *[]const u8, rhs: *[]const u8) bool {
    return std.mem.order(u8, lhs.*, rhs.*).compare(std.math.CompareOperator.lt);
}

fn solution(path: []const u8) ![]const u8 {
    var allocator = std.heap.page_allocator;

    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buffer: [256]u8 = undefined;

    var hashmap = std.StringHashMap(Status).init(allocator);
    defer hashmap.deinit();

    while (try in_stream.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        // ;를 기준으로 분할
        var splitedIter = std.mem.split(u8, line, ";");

        const cityName: []const u8 = splitedIter.next().?;
        const measurement: i64 = try std.fmt.parseInt(i32, splitedIter.next().?, 10);

        if (hashmap.getPtr(cityName)) |value| {
            var min = value.min;
            if (measurement < min) {
                min = measurement;
            }

            var max = value.max;
            if (measurement > max) {
                max = measurement;
            }

            const total = value.total + measurement;

            value.* = Status{
                .min = min,
                .max = max,
                .total = total,
                .count = value.count + 1,
            };
        } else {
            const cityCopy = try allocator.alloc(u8, cityName.len);
            @memcpy(cityCopy, cityName);

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

    // 키 추출
    while (iterator.next()) |entry| {
        const cityName = entry.key_ptr;

        try cityNames.append(cityName);
    }
    const cityNamesSlice = try cityNames.toOwnedSlice();

    // 키를 오름차순으로 정렬
    std.mem.sort(*[]const u8, cityNamesSlice, {}, compareStrings);

    // 키 순서대로 조회하면서 결과 생성
    var result = try allocator.alloc(u8, 100);
    result = "";
    for (cityNamesSlice) |cityName| {
        if (hashmap.get(cityName.*)) |status| {
            const avg = @divTrunc(status.total, status.count);

            const line = try std.fmt.allocPrint(
                allocator,
                "{s}={d};{d};{d}({d}/{d})\n",
                .{ cityName.*, status.min, status.max, avg, status.total, status.count },
            );

            const toFree = result;
            result = try std.mem.concat(allocator, u8, &[_][]const u8{ result, line });
            allocator.free(toFree);
            allocator.free(line);
        }
    }

    return result;
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    var file = try std.fs.cwd().openFile(common.OUTPUT_PATH, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buffer: [8192]u8 = undefined;

    const size = try in_stream.readAll(&buffer);
    const expectOutputs = buffer[0..size];

    var timer = try std.time.Timer.start();
    const got = try solution(common.MEASUREMENTS_PATH);
    const elapsedNano = timer.read();
    const elapsedMilli = elapsedNano / 1_000_000;

    try stdout.print("elapsed: {}ms\n", .{elapsedMilli});

    if (std.mem.eql(u8, got, expectOutputs)) {
        try stdout.print("Test passed\n", .{});
    } else {
        try stdout.print("Test failed\n", .{});
        try stdout.print("Expected\n{s}", .{expectOutputs});
        try stdout.print("Got\n{s}", .{got});
    }
}
