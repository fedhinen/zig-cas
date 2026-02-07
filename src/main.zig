const std = @import("std");
const Io = std.Io;

const cas = @import("cas");
const expr = @import("./ops/expr.zig");

const Expr = expr.Expr;

pub fn main(init: std.process.Init) !void {
    // This is appropriate for anything that lives as long as the process.
    const arena: std.mem.Allocator = init.arena.allocator();

    // Accessing command line arguments:
    const args = try init.minimal.args.toSlice(arena);
    for (args) |arg| {
        std.log.info("arg: {s}", .{arg});
    }

    // In order to do I/O operations need an `Io` instance.
    const io = init.io;

    // Stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_file_writer: Io.File.Writer = .init(.stdout(), io, &stdout_buffer);
    const stdout_writer = &stdout_file_writer.interface;

    const five_expr = try Expr.createLiteral(arena, 5);
    const twenty_expr = try Expr.createLiteral(arena, 20);
    const x_expr = try Expr.createVar(arena, 'x');

    const first_expr = try Expr.createMul(arena, five_expr, x_expr);
    const second_expr = try Expr.createMul(arena, twenty_expr, x_expr);

    const full_expr = try Expr.createMul(arena, first_expr, second_expr);

    const test_derivative = try full_expr.derivative('x', arena);
    const test_derivative_str = try test_derivative.string(arena);

    try stdout_writer.print("Test derivative, {s}\n", .{test_derivative_str});

    try stdout_writer.flush();
}
