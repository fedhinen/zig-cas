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

    const expected = try Expr.createLiteral(allocator, 1);

    try std.testing.expect(simplified.isEqual(expected));
}

test "d(x)/dy = 0" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator: std.mem.Allocator = arena.allocator();

    const x_expr = try Expr.createVar(allocator, 'x');
    const derivative = try x_expr.derivative('y', allocator);
    const simplified = try derivative.simplify(allocator);

    const expected = try Expr.createLiteral(allocator, 0);

    try std.testing.expect(simplified.isEqual(expected));
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

    const expected = try Expr.createLiteral(allocator, 5);

    try std.testing.expect(simplified.isEqual(expected));
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

    const three_expr = try Expr.createLiteral(allocator, 3);
    const inner_pow = try Expr.createPow(allocator, x_expr, three_expr);
    const expected_expr = try Expr.createMul(allocator, four_expr, inner_pow);

    try std.testing.expect(simplified.isEqual(expected_expr));
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

    const fiveteen_expr = try Expr.createLiteral(allocator, 15);
    const inner_base = try Expr.createAdd(allocator, one_expr, five_x_expr);
    const two_expr = try Expr.createLiteral(allocator, 2);
    const inner_pow = try Expr.createPow(allocator, inner_base, two_expr);
    const expected_expr = try Expr.createMul(allocator, fiveteen_expr, inner_pow);

    try std.testing.expect(simplified.isEqual(expected_expr));
}

// d(f(x) * g(x))/dx = f'(x) * g(x) + f(x) * g'(x)
// Se puede simplificar aun mas, 5x^3 + 3x^2 + 15x^3 = 20x^3 + 3x^2
// TODO: Implementar la simplificacion de terminos semejantes para que el resultado sea el mas simple posible
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

    const two_expr = try Expr.createLiteral(allocator, 2);
    const x_pow_2_expr = try Expr.createPow(allocator, x_expr, two_expr);
    const three_x_pow_2_expr = try Expr.createMul(allocator, three_expr, x_pow_2_expr);
    const one_plus_5x_expr = try Expr.createAdd(allocator, one_expr, five_x_expr);
    const right_term_expr = try Expr.createMul(allocator, one_plus_5x_expr, three_x_pow_2_expr);
    const five_x_pow_3_expr = try Expr.createMul(allocator, five_expr, x_pow_3_expr);
    const expected_expr = try Expr.createAdd(allocator, five_x_pow_3_expr, right_term_expr);

    try std.testing.expect(simplified.isEqual(expected_expr));
}
test "Expr.is" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator: std.mem.Allocator = arena.allocator();

    const five_expr = try Expr.createLiteral(allocator, 5);
    const x_expr = try Expr.createVar(allocator, 'x');
    const mul_expr = try Expr.createMul(allocator, five_expr, x_expr);
    const pow_expr = try Expr.createPow(allocator, five_expr, x_expr);
    const add_expr = try Expr.createAdd(allocator, five_expr, x_expr);
    const sub_expr = try Expr.createSub(allocator, five_expr, x_expr);
    const div_expr = try Expr.createDiv(allocator, five_expr, x_expr);

    try std.testing.expect(five_expr.is(.Literal));
    try std.testing.expect(x_expr.is(.Variable));
    try std.testing.expect(mul_expr.is(.Multiply));
    try std.testing.expect(pow_expr.is(.Pow));
    try std.testing.expect(add_expr.is(.Add));
    try std.testing.expect(sub_expr.is(.Substract));
    try std.testing.expect(div_expr.is(.Divide));
}

test "Expr.isEqual: 5 * x" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator: std.mem.Allocator = arena.allocator();

    const five_expr = try Expr.createLiteral(allocator, 5);
    const x_expr = try Expr.createVar(allocator, 'x');
    const expr1 = try Expr.createMul(allocator, five_expr, x_expr);
    const expr2 = try Expr.createMul(allocator, five_expr, x_expr);

    try std.testing.expect(expr1.isEqual(expr2));
}

test "Expr.isEqual: 5 * x != 5 * y" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator: std.mem.Allocator = arena.allocator();

    const five_expr = try Expr.createLiteral(allocator, 5);
    const x_expr = try Expr.createVar(allocator, 'x');
    const y_expr = try Expr.createVar(allocator, 'y');
    const expr1 = try Expr.createMul(allocator, five_expr, x_expr);
    const expr2 = try Expr.createMul(allocator, five_expr, y_expr);

    try std.testing.expect(!expr1.isEqual(expr2));
}

test "Expr.isEqual: (((5 + x) * (2 + y)) ^ (10 + w))" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator: std.mem.Allocator = arena.allocator();

    const five_expr = try Expr.createLiteral(allocator, 5);
    const x_expr = try Expr.createVar(allocator, 'x');
    const two_expr = try Expr.createLiteral(allocator, 2);
    const y_expr = try Expr.createVar(allocator, 'y');
    const ten_expr = try Expr.createLiteral(allocator, 10);
    const w_expr = try Expr.createVar(allocator, 'w');

    const inner_add1 = try Expr.createAdd(allocator, five_expr, x_expr);
    const inner_add2 = try Expr.createAdd(allocator, two_expr, y_expr);
    const inner_pow = try Expr.createAdd(allocator, ten_expr, w_expr);
    const mul_expr = try Expr.createMul(allocator, inner_add1, inner_add2);
    const pow_expr1 = try Expr.createPow(allocator, mul_expr, inner_pow);
    const pow_expr2 = try Expr.createPow(allocator, mul_expr, inner_pow);

    try std.testing.expect(pow_expr1.isEqual(pow_expr2));
}

test "Expr.isEqual: f(x)/f(x) == 1" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator: std.mem.Allocator = arena.allocator();

    const x_expr = try Expr.createVar(allocator, 'x');
    const f_x_expr = try Expr.createAdd(allocator, x_expr, x_expr); // f(x) = 2x
    const div_expr = try Expr.createDiv(allocator, f_x_expr, f_x_expr);
    const div_simplified = try div_expr.simplify(allocator);

    const expected = try Expr.createLiteral(allocator, 1);

    try std.testing.expect(div_simplified.isEqual(expected));
}

test "Simplify (5 * x) * (3 * x) => 15 * x^2" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator: std.mem.Allocator = arena.allocator();

    const five_expr = try Expr.createLiteral(allocator, 5);
    const three_expr = try Expr.createLiteral(allocator, 3);
    const x_expr = try Expr.createVar(allocator, 'x');
    const five_x_expr = try Expr.createMul(allocator, five_expr, x_expr);
    const three_x_expr = try Expr.createMul(allocator, three_expr, x_expr);
    const mul_expr = try Expr.createMul(allocator, five_x_expr, three_x_expr);

    const simplified = try mul_expr.simplify(allocator);

    const fifteen_expr = try Expr.createLiteral(allocator, 15);
    const two_expr = try Expr.createLiteral(allocator, 2);
    const x_pow_2_expr = try Expr.createPow(allocator, x_expr, two_expr);
    const expected = try Expr.createMul(allocator, fifteen_expr, x_pow_2_expr);

    try std.testing.expect(simplified.isEqual(expected));
}

test "Simplify (3 * x) + (5 * x) => 8x" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator: std.mem.Allocator = arena.allocator();

    const three_expr = try Expr.createLiteral(allocator, 3);
    const five_expr = try Expr.createLiteral(allocator, 5);
    const x_expr = try Expr.createVar(allocator, 'x');
    const three_x_expr = try Expr.createMul(allocator, three_expr, x_expr);
    const five_x_expr = try Expr.createMul(allocator, five_expr, x_expr);
    const sum_expr = try Expr.createAdd(allocator, three_x_expr, five_x_expr);

    const simplified = try sum_expr.simplify(allocator);

    const eight_expr = try Expr.createLiteral(allocator, 8);
    const expected = try Expr.createMul(allocator, eight_expr, x_expr);

    try std.testing.expect(simplified.isEqual(expected));
}

test "Simplify (5 + x) * (2 + y) => 10 + 5y + 2x + xy" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator: std.mem.Allocator = arena.allocator();

    const five_expr = try Expr.createLiteral(allocator, 5);
    const two_expr = try Expr.createLiteral(allocator, 2);
    const x_expr = try Expr.createVar(allocator, 'x');
    const y_expr = try Expr.createVar(allocator, 'y');
    const add1_expr = try Expr.createAdd(allocator, five_expr, x_expr);
    const add2_expr = try Expr.createAdd(allocator, two_expr, y_expr);
    const mul_expr = try Expr.createMul(allocator, add1_expr, add2_expr);

    const simplified = try mul_expr.simplify(allocator);

    const ten_expr = try Expr.createLiteral(allocator, 10);
    const five_y_expr = try Expr.createMul(allocator, five_expr, y_expr);
    const two_x_expr = try Expr.createMul(allocator, two_expr, x_expr);
    const xy_expr = try Expr.createMul(allocator, x_expr, y_expr);
    const first_sum = try Expr.createAdd(allocator, ten_expr, five_y_expr);
    const second_sum = try Expr.createAdd(allocator, two_x_expr, xy_expr);
    const expected = try Expr.createAdd(allocator, first_sum, second_sum);

    try std.testing.expect(simplified.isEqual(expected));
}

test "Simplify (10 * x) - (10 * x) => 0" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator: std.mem.Allocator = arena.allocator();

    const ten_expr = try Expr.createLiteral(allocator, 10);
    const x_expr = try Expr.createVar(allocator, 'x');
    const ten_x_expr = try Expr.createMul(allocator, ten_expr, x_expr);
    const sub_expr = try Expr.createSub(allocator, ten_x_expr, ten_x_expr);

    const simplified = try sub_expr.simplify(allocator);

    const expected = try Expr.createLiteral(allocator, 0);

    try std.testing.expect(simplified.isEqual(expected));
}

test "Simplify (x^2) * (x^3) => x^5" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator: std.mem.Allocator = arena.allocator();

    const x_expr = try Expr.createVar(allocator, 'x');
    const two_expr = try Expr.createLiteral(allocator, 2);
    const three_expr = try Expr.createLiteral(allocator, 3);
    const x_pow_2_expr = try Expr.createPow(allocator, x_expr, two_expr);
    const x_pow_3_expr = try Expr.createPow(allocator, x_expr, three_expr);
    const mul_expr = try Expr.createMul(allocator, x_pow_2_expr, x_pow_3_expr);

    const simplified = try mul_expr.simplify(allocator);

    const five_expr = try Expr.createLiteral(allocator, 5);
    const expected = try Expr.createPow(allocator, x_expr, five_expr);

    try std.testing.expect(simplified.isEqual(expected));
}
