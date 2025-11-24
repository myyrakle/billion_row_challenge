const std = @import("std");
const common = @import("common.zig");

const Status = struct {
    min: i64,
    max: i64,
    total: i64,
    count: i64,
};

fn compareStrings(_: void, lhs: []const u8, rhs: []const u8) bool {
    return std.mem.order(u8, lhs, rhs) == .lt;
}

fn parseValue(line: []const u8, separator_pos: usize) ?i64 {
    const value_str = line[separator_pos + 1 ..];
    return std.fmt.parseInt(i64, value_str, 10) catch null;
}

fn solution(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    var hashmap = std.StringHashMap(Status).init(allocator);
    defer {
        var it = hashmap.keyIterator();
        while (it.next()) |key| {
            allocator.free(key.*);
        }
        hashmap.deinit();
    }

    // Read entire file into memory for faster processing
    const file_size = try file.getEndPos();
    const buffer = try allocator.alloc(u8, file_size);
    defer allocator.free(buffer);

    const bytes_read = try file.readAll(buffer);
    if (bytes_read != file_size) {
        return error.IncompleteRead;
    }

    // Process lines
    var line_start: usize = 0;
    var i: usize = 0;

    while (i < buffer.len) {
        if (buffer[i] == '\n') {
            var line_end = i;

            // Remove carriage return if present
            if (line_end > 0 and buffer[line_end - 1] == '\r') {
                line_end -= 1;
            }

            if (line_end > line_start) {
                const line = buffer[line_start..line_end];

                // Find separator
                if (std.mem.indexOfScalar(u8, line, ';')) |sep_pos| {
                    const city_name = line[0..sep_pos];
                    if (parseValue(line, sep_pos)) |measurement| {
                        // Update or insert in hashmap
                        if (hashmap.getPtr(city_name)) |entry| {
                            entry.min = @min(entry.min, measurement);
                            entry.max = @max(entry.max, measurement);
                            entry.total += measurement;
                            entry.count += 1;
                        } else {
                            const city_copy = try allocator.dupe(u8, city_name);
                            try hashmap.put(city_copy, Status{
                                .min = measurement,
                                .max = measurement,
                                .total = measurement,
                                .count = 1,
                            });
                        }
                    }
                }
            }

            line_start = i + 1;
        }

        i += 1;
    }

    // Collect and sort city names
    var city_names: std.ArrayList([]const u8) = .{};
    defer city_names.deinit(allocator);

    var iterator = hashmap.keyIterator();
    while (iterator.next()) |city_ptr| {
        try city_names.append(allocator, city_ptr.*);
    }

    std.mem.sort([]const u8, city_names.items, {}, compareStrings);

    // Generate output
    var output: std.ArrayList(u8) = .{};
    defer output.deinit(allocator);

    var buf: [256]u8 = undefined;

    for (city_names.items) |city_name| {
        if (hashmap.get(city_name)) |status| {
            const avg = @divTrunc(status.total, status.count);
            const line_str = try std.fmt.bufPrint(
                &buf,
                "{s}={d};{d};{d}({d}/{d})\n",
                .{ city_name, status.min, status.max, avg, status.total, status.count },
            );
            try output.appendSlice(allocator, line_str);
        }
    }

    return output.toOwnedSlice(allocator);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read expected output
    var output_file = try std.fs.cwd().openFile(common.OUTPUT_PATH, .{});
    defer output_file.close();

    const expected_output = try output_file.readToEndAlloc(allocator, 1024 * 1024 * 10);
    defer allocator.free(expected_output);

    // Time the solution
    var timer = try std.time.Timer.start();
    const got = try solution(allocator, common.MEASUREMENTS_PATH);
    defer allocator.free(got);
    const elapsed_nanos = timer.read();
    const elapsed_millis = elapsed_nanos / 1_000_000;

    // Output results
    _ = try std.posix.write(std.posix.STDOUT_FILENO, "Elapsed: ");
    var buf: [32]u8 = undefined;
    const time_str = try std.fmt.bufPrint(&buf, "{}ms\n", .{elapsed_millis});
    _ = try std.posix.write(std.posix.STDOUT_FILENO, time_str);

    if (std.mem.eql(u8, got, expected_output)) {
        _ = try std.posix.write(std.posix.STDOUT_FILENO, "Test passed\n");
    } else {
        _ = try std.posix.write(std.posix.STDOUT_FILENO, "Test failed\n");
        _ = try std.posix.write(std.posix.STDOUT_FILENO, "Expected:\n");
        _ = try std.posix.write(std.posix.STDOUT_FILENO, expected_output);
        _ = try std.posix.write(std.posix.STDOUT_FILENO, "\nGot:\n");
        _ = try std.posix.write(std.posix.STDOUT_FILENO, got);
    }
}
