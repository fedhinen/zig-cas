const std = @import("std");
const Io = std.Io;

const cas = @import("cas");

const BinOp = enum {
    Add,
    Subtract,
    Multiply,
    Divide,
    // Power, // For future extension
};

const ExprTy = enum {
    Variable,
    Literal,
    Binary,
};

const BinaryExpr = struct {
    left: *const Expr,
    right: *const Expr,
    op: BinOp,
};

const Expr = union(ExprTy) {
    Variable: u8,
    Literal: i32,
    Binary: BinaryExpr,
    fn eval(self: *const Expr, vars: *const std.AutoHashMap(u8, i32)) isize {
        return switch (self.*) {
            Expr.Variable => |v| {
                const var_name = v;
                const value = vars.get(var_name);
                if (value) |val| {
                    return val;
                } else {
                    std.debug.panic("Undefined variable: {d}", .{var_name});
                }
            },
            Expr.Literal => |l| l,
            Expr.Binary => |b| {
                const left_val = b.left.eval(vars);
                const right_val = b.right.eval(vars);
                return switch (b.op) {
                    BinOp.Add => left_val + right_val,
                    BinOp.Subtract => left_val - right_val,
                    BinOp.Multiply => left_val * right_val,
                    BinOp.Divide => @divFloor(left_val, right_val),
                };
            },
        };
    }
    fn derive(self: *const Expr, vars: u8, alloc: std.mem.Allocator) !Expr {
        return switch (self.*) {
            Expr.Variable => |v| {
                if (v == vars) {
                    return Expr{ .Literal = 1 };
                } else {
                    return Expr{ .Literal = 0 };
                }
            },
            Expr.Literal => Expr{ .Literal = 0 },
            Expr.Binary => |b| {
                const ld = try b.left.derive(vars, alloc);
                const rd = try b.right.derive(vars, alloc);

                const left_deriv = try alloc.create(Expr);
                left_deriv.* = ld;
                const right_deriv = try alloc.create(Expr);
                right_deriv.* = rd;

                return switch (b.op) {
                    // (f + g)' = f' + g'
                    BinOp.Add => Expr{ .Binary = BinaryExpr{ .op = BinOp.Add, .left = left_deriv, .right = right_deriv } },
                    // (f - g)' = f' - g'
                    BinOp.Subtract => Expr{ .Binary = BinaryExpr{ .op = BinOp.Subtract, .left = left_deriv, .right = right_deriv } },
                    BinOp.Multiply => {
                        // Necesitamos crear los nodos para (f' * g) y (f * g') en el heap
                        const left_node = try alloc.create(Expr);
                        left_node.* = Expr{ .Binary = .{ .op = .Multiply, .left = left_deriv, .right = b.right } };

                        const right_node = try alloc.create(Expr);
                        right_node.* = Expr{ .Binary = .{ .op = .Multiply, .left = b.left, .right = right_deriv } };

                        return Expr{ .Binary = .{ .op = .Add, .left = left_node, .right = right_node } };
                    },

                    BinOp.Divide => {
                        // Numerador parte 1: (f' * g)
                        const n1 = try alloc.create(Expr);
                        n1.* = Expr{ .Binary = .{ .op = .Multiply, .left = left_deriv, .right = b.right } };

                        // Numerador parte 2: (f * g')
                        const n2 = try alloc.create(Expr);
                        n2.* = Expr{ .Binary = .{ .op = .Multiply, .left = b.left, .right = right_deriv } };

                        // Numerador completo: (f' * g - f * g')
                        const numerator = try alloc.create(Expr);
                        numerator.* = Expr{ .Binary = .{ .op = .Subtract, .left = n1, .right = n2 } };

                        // Denominador: (g * g)
                        const denominator = try alloc.create(Expr);
                        denominator.* = Expr{ .Binary = .{ .op = .Multiply, .left = b.right, .right = b.right } };

                        return Expr{ .Binary = .{ .op = .Divide, .left = numerator, .right = denominator } };
                    },
                };
            },
        };
    }
    fn stringify(self: *const Expr, alloc: std.mem.Allocator) ![]const u8 {
        return switch (self.*) {
            Expr.Variable => |v| try std.fmt.allocPrint(alloc, "{c}", .{v}),
            Expr.Literal => |l| try std.fmt.allocPrint(alloc, "{d}", .{l}),
            Expr.Binary => |b| {
                const left_str = try b.left.stringify(alloc);
                const right_str = try b.right.stringify(alloc);

                const op_str = switch (b.op) {
                    BinOp.Add => "+",
                    BinOp.Subtract => "-",
                    BinOp.Multiply => "*",
                    BinOp.Divide => "/",
                };
                return try std.fmt.allocPrint(alloc, "({s} {s} {s})", .{ left_str, op_str, right_str });
            },
        };
    }
};

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

    var vars: std.AutoHashMap(u8, i32) = .init(arena);
    defer vars.deinit();
    try vars.put('x', 10);
    try vars.put('y', 20);

    const expr_with_vars = Expr{ .Binary = BinaryExpr{
        .op = BinOp.Add,
        .left = &Expr{ .Variable = 'x' },
        .right = &Expr{ .Variable = 'y' },
    } };

    try stdout_writer
        .print("Result with variables: {d}\n", .{expr_with_vars
        .eval(&vars)});

    const derivative_expr = try expr_with_vars.derive('x', arena); // 1 + 0
    const derivative_str = try derivative_expr.stringify(arena);
    try stdout_writer.print("Result with variables, derived: {s}\n", .{derivative_str});

    const to_derivative_expr = Expr{ .Binary = BinaryExpr{ .op = BinOp.Multiply, .left = &Expr{ .Binary = BinaryExpr{ .op = BinOp.Multiply, .left = &Expr{ .Literal = 5 }, .right = &Expr{ .Variable = 'x' } } }, .right = &Expr{ .Binary = BinaryExpr{ .op = BinOp.Multiply, .left = &Expr{ .Literal = 20 }, .right = &Expr{ .Variable = 'x' } } } } };
    const to_derivative = try to_derivative_expr.derive('x', arena);
    const to_derivative_str = try to_derivative.stringify(arena);
    try stdout_writer.print("Result derivative, {s}\n", .{to_derivative_str});

    try stdout_writer.flush();
}
