const std = @import("std");
const Io = std.Io;

const cas = @import("cas");

const BinOp = enum {
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
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
                    BinOp.Power => std.math.pow(i32, left_val, right_val),
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
                    BinOp.Add => Expr{ .Binary = .{ .op = .Add, .left = left_deriv, .right = right_deriv } },
                    // (f - g)' = f' - g'
                    BinOp.Subtract => Expr{ .Binary = .{ .op = .Subtract, .left = left_deriv, .right = right_deriv } },
                    // (f * g)' = f' * g + f * g'
                    BinOp.Multiply => {
                        // Necesitamos crear los nodos para (f' * g) y (f * g') en el heap
                        const left_node = try alloc.create(Expr);
                        left_node.* = Expr{ .Binary = .{ .op = .Multiply, .left = left_deriv, .right = b.right } };

                        const right_node = try alloc.create(Expr);
                        right_node.* = Expr{ .Binary = .{ .op = .Multiply, .left = b.left, .right = right_deriv } };

                        return Expr{ .Binary = .{ .op = .Add, .left = left_node, .right = right_node } };
                    },
                    // (f / g)' = (f' * g - f * g') / (g * g)
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
                    // (f ^ n)' = n * f ^ ( n - 1 ) * f' --- f is left, n is right
                    BinOp.Power => {
                        // n
                        const n_value = try alloc.create(Expr);
                        n_value.* = Expr{ .Literal = b.right.Literal };

                        // n - 1
                        const n_one = try alloc.create(Expr);
                        n_one.* = Expr{ .Literal = 1 };

                        const exp_value = try alloc.create(Expr);
                        exp_value.* = Expr{ .Binary = .{ .op = .Subtract, .left = b.right, .right = n_one } };

                        // f ^ ( n - 1 )
                        const base = try alloc.create(Expr);
                        base.* = Expr{ .Binary = .{ .op = .Power, .left = b.left, .right = exp_value } };

                        // n * f ^ ( n - 1 )
                        const power_expr = try alloc.create(Expr);
                        power_expr.* = Expr{ .Binary = .{ .op = .Multiply, .left = n_value, .right = base } };

                        // n * f ^ ( n - 1 ) * f'
                        return Expr{ .Binary = .{ .op = .Multiply, .left = power_expr, .right = left_deriv } };
                    },
                };
            },
        };
    }
    fn isLiteral(self: *const Expr, val: i32) bool {
        return switch (self.*) {
            Expr.Literal => |l| l == val,
            else => false,
        };
    }
    fn simplify(self: *const Expr, alloc: std.mem.Allocator) !Expr {
        return switch (self.*) {
            Expr.Variable, Expr.Literal => self.*,
            Expr.Binary => |b| {
                const left_expr = try b.left.simplify(alloc);
                const right_expr = try b.right.simplify(alloc);

                // constantes
                if (left_expr == .Literal and right_expr == .Literal) {
                    const left_val = left_expr.Literal;
                    const right_val = right_expr.Literal;

                    const val = switch (b.op) {
                        BinOp.Add => left_val + right_val,
                        BinOp.Subtract => left_val - right_val,
                        BinOp.Multiply => left_val * right_val,
                        BinOp.Divide => @divFloor(left_val, right_val),
                        BinOp.Power => std.math.pow(i32, left_val, right_val),
                    };

                    return Expr{ .Literal = val };
                }

                // identidades
                switch (b.op) {
                    BinOp.Add => {
                        // x + 0 = x
                        if (right_expr.isLiteral(0)) return left_expr;
                        // 0 + x = x
                        if (left_expr.isLiteral(0)) return right_expr;
                    },
                    BinOp.Subtract => {
                        // x - 0 = x
                        if (right_expr.isLiteral(0)) return left_expr;
                        // 0 - x = -x

                        if (left_expr.isLiteral(0)) {
                            const neg_one = try alloc.create(Expr);
                            neg_one.* = Expr{ .Literal = -1 };

                            const right_expr_neg = try alloc.create(Expr);
                            right_expr_neg.* = right_expr;

                            return Expr{ .Binary = .{ .op = .Multiply, .left = neg_one, .right = right_expr_neg } };
                        }

                        // x - x = 0 comparar arboles completos TODO
                    },
                    BinOp.Multiply => {
                        if (left_expr.isLiteral(0) or right_expr.isLiteral(0)) return Expr{ .Literal = 0 };

                        if (left_expr.isLiteral(1)) return right_expr;

                        if (right_expr.isLiteral(1)) return left_expr;
                    },
                    BinOp.Divide => {
                        if (right_expr.isLiteral(1)) return left_expr;

                        // que sucede con x/0, panic?
                        // que sucede con x/x = 1, comparacion de expr pendiente
                    },
                    BinOp.Power => {
                        // x ^ 0 = 1
                        if (right_expr.isLiteral(0)) return Expr{ .Literal = 1 };
                        // x ^ 1 = x
                        if (right_expr.isLiteral(1)) return left_expr;
                    },
                }

                const left_ptr = try alloc.create(Expr);
                left_ptr.* = left_expr;

                const right_ptr = try alloc.create(Expr);
                right_ptr.* = right_expr;

                return Expr{ .Binary = BinaryExpr{ .left = left_ptr, .right = right_ptr, .op = b.op } };
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
                    BinOp.Power => "^",
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

    const to_derivative_expr = Expr{ .Binary = BinaryExpr{ .op = BinOp.Multiply, .left = &Expr{ .Binary = BinaryExpr{ .op = BinOp.Multiply, .left = &Expr{ .Literal = 5 }, .right = &Expr{ .Variable = 'x' } } }, .right = &Expr{ .Binary = BinaryExpr{ .op = BinOp.Multiply, .left = &Expr{ .Literal = 20 }, .right = &Expr{ .Variable = 'x' } } } } };
    const to_derivative = try to_derivative_expr.derive('x', arena);
    const simplified_derivative = try to_derivative.simplify(arena);
    const to_derivative_str = try simplified_derivative.stringify(arena);
    try stdout_writer.print("Result derivative, {s}\n", .{to_derivative_str});

    try stdout_writer.flush();
}
