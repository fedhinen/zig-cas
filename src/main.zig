const std = @import("std");
const cas = @import("cas");
const expr = @import("./ops/expr.zig");

const Io = std.Io;
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

    std.log.info("Hello, world!", .{});

    try stdout_writer.flush();
}

test "d(c)/dx = 0 --- (5)' = 0" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator: std.mem.Allocator = arena.allocator();

    const five_expr = try Expr.createLiteral(allocator, 5);
    const derivative = try five_expr.derivative('x', allocator);
    const simplified = try derivative.simplify(allocator);

    try std.testing.expect(simplified.isConstantValue(0));
}

test "d(x)/dx = 1" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator: std.mem.Allocator = arena.allocator();

    const x_expr = try Expr.createVar(allocator, 'x');
    const derivative = try x_expr.derivative('x', allocator);
    const simplified = try derivative.simplify(allocator);

    try std.testing.expect(simplified.isConstantValue(1));
}

test "d(x)/dy = 0" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator: std.mem.Allocator = arena.allocator();

    const x_expr = try Expr.createVar(allocator, 'x');
    const derivative = try x_expr.derivative('y', allocator);
    const simplified = try derivative.simplify(allocator);

    try std.testing.expect(simplified.isConstantValue(0));
}

// d(cx)/dx = c * d(x)/dx = c * 1 = c
test "d(5x)/dx = 5" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator: std.mem.Allocator = arena.allocator();

    const five_expr = try Expr.createLiteral(allocator, 5);
    const x_expr = try Expr.createVar(allocator, 'x');
    const expr_test = try Expr.createMul(allocator, five_expr, x_expr);

    const derivative = try expr_test.derivative('x', allocator);
    const simplified = try derivative.simplify(allocator);

    try std.testing.expect(simplified.isConstantValue(5));
}

// d(x^n)/dx = n * x^(n-1)
test "d(x^4)/dx = (4 * (x ^ 3))" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator: std.mem.Allocator = arena.allocator();

    const x_expr = try Expr.createVar(allocator, 'x');
    const four_expr = try Expr.createLiteral(allocator, 4);
    const expr_test = try Expr.createPow(allocator, x_expr, four_expr);

    const derivative = try expr_test.derivative('x', allocator);
    const simplified = try derivative.simplify(allocator);

    const stringified = try simplified.string(allocator);
    const expected = "(4 * (x ^ 3))";
    const is_equal = std.mem.eql(u8, stringified, expected);
    try std.testing.expect(is_equal);
}

// d(f(x) ^ n)/dx = n * (f(x) ^ (n-1)) * f'(x)
test "d((5x + 1) ^ 3)/dx = (3 * ((5x + 1) ^ 2) * 5) = (15 * (1 + (5 * x)) ^ 2))" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator: std.mem.Allocator = arena.allocator();

    const five_expr = try Expr.createLiteral(allocator, 5);
    const x_expr = try Expr.createVar(allocator, 'x');
    const one_expr = try Expr.createLiteral(allocator, 1);
    const five_x_expr = try Expr.createMul(allocator, five_expr, x_expr);
    const inner_expr = try Expr.createAdd(allocator, five_x_expr, one_expr);
    const three_expr = try Expr.createLiteral(allocator, 3);
    const expr_test = try Expr.createPow(allocator, inner_expr, three_expr);

    const derivative = try expr_test.derivative('x', allocator);
    const simplified = try derivative.simplify(allocator);

    const stringified = try simplified.string(allocator);
    const expected = "(15 * ((1 + (5 * x)) ^ 2))";
    const is_equal = std.mem.eql(u8, stringified, expected);
    try std.testing.expect(is_equal);
}

// d(f(x) * g(x))/dx = f'(x) * g(x) + f(x) * g'(x)
// Se puede simplificar aun mas, 5x^3 + 3x^2 + 15x^3 = 20x^3 + 3x^2
test "d((5x + 1) * (x^3))/dx = ((5 * (x ^ 3)) + ((1 + (5 * x)) * (3 * (x ^ 2))))" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator: std.mem.Allocator = arena.allocator();
    const five_expr = try Expr.createLiteral(allocator, 5);
    const x_expr = try Expr.createVar(allocator, 'x');
    const one_expr = try Expr.createLiteral(allocator, 1);
    const five_x_expr = try Expr.createMul(allocator, five_expr, x_expr);
    const inner_expr = try Expr.createAdd(allocator, five_x_expr, one_expr);
    const three_expr = try Expr.createLiteral(allocator, 3);
    const x_pow_3_expr = try Expr.createPow(allocator, x_expr, three_expr);
    const expr_test = try Expr.createMul(allocator, inner_expr, x_pow_3_expr);

    const derivative = try expr_test.derivative('x', allocator);
    const simplified = try derivative.simplify(allocator);

    const stringified = try simplified.string(allocator);
    const expected = "((5 * (x ^ 3)) + ((1 + (5 * x)) * (3 * (x ^ 2))))";
    const is_equal = std.mem.eql(u8, stringified, expected);
    try std.testing.expect(is_equal);
}
