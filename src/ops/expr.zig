const std = @import("std");

const add = @import("./add.zig");
const sub = @import("./sub.zig");
const mul = @import("./mul.zig");
const div = @import("./div.zig");
const pow = @import("./pow.zig");

const ExprTy = enum {
    Variable,
    Literal,
    Add,
    Substract,
    Multiply,
    Divide,
    Pow,
};

pub const Expr = union(ExprTy) {
    Variable: u8,
    Literal: i32,
    Add: *add.AddExpression,
    Substract: *sub.SubstractExpression,
    Multiply: *mul.MultiplyExpression,
    Divide: *div.DivideExpression,
    Pow: *pow.PowExpression,
    pub fn eval(self: *const Expr, vars: *const std.AutoHashMap(u8, i32)) isize {
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
            Expr.Add => |expr| expr.eval(vars),
            Expr.Substract => |expr| expr.eval(vars),
            Expr.Multiply => |expr| expr.eval(vars),
            Expr.Divide => |expr| expr.eval(vars),
            Expr.Pow => |expr| expr.eval(vars),
        };
    }
    pub fn derivative(self: *const Expr, vars: u8, alloc: std.mem.Allocator) std.mem.Allocator.Error!*Expr {
        return switch (self.*) {
            Expr.Variable => |v| {
                if (v == vars) {
                    return try Expr.createLiteral(alloc, 1);
                } else {
                    return try Expr.createLiteral(alloc, 0);
                }
            },
            Expr.Literal => try Expr.createLiteral(alloc, 0),
            Expr.Add => |expr| try expr.derivative(vars, alloc),
            Expr.Substract => |expr| try expr.derivative(vars, alloc),
            Expr.Multiply => |expr| try expr.derivative(vars, alloc),
            Expr.Divide => |expr| try expr.derivative(vars, alloc),
            Expr.Pow => |expr| try expr.derivative(vars, alloc),
        };
    }
    pub fn string(self: *const Expr, alloc: std.mem.Allocator) std.mem.Allocator.Error![]const u8 {
        return switch (self.*) {
            Expr.Variable => |v| try std.fmt.allocPrint(alloc, "{c}", .{v}),
            Expr.Literal => |l| try std.fmt.allocPrint(alloc, "{d}", .{l}),
            Expr.Add => |e| try e.string(alloc),
            Expr.Substract => |e| try e.string(alloc),
            Expr.Multiply => |e| try e.string(alloc),
            Expr.Divide => |e| try e.string(alloc),
            Expr.Pow => |e| try e.string(alloc),
        };
    }

    // helpers
    pub fn createLiteral(alloc: std.mem.Allocator, val: i32) !*Expr {
        const e = try alloc.create(Expr);
        e.* = Expr{ .Literal = val };
        return e;
    }

    pub fn createVar(alloc: std.mem.Allocator, name: u8) !*Expr {
        const e = try alloc.create(Expr);
        e.* = Expr{ .Variable = name };
        return e;
    }

    pub fn createMul(alloc: std.mem.Allocator, lhs: *Expr, rhs: *Expr) !*Expr {
        const m = try alloc.create(mul.MultiplyExpression);
        m.* = .{ .lhs = lhs, .rhs = rhs };

        const e = try alloc.create(Expr);
        e.* = Expr{ .Multiply = m };
        return e;
    }

    pub fn createPow(alloc: std.mem.Allocator, base: *Expr, exponent: *Expr) !*Expr {
        const p = try alloc.create(pow.PowExpression);
        p.* = .{ .base = base, .exponent = exponent };

        const e = try alloc.create(Expr);
        e.* = Expr{ .Pow = p };
        return e;
    }

    pub fn createAdd(alloc: std.mem.Allocator, lhs: *Expr, rhs: *Expr) !*Expr {
        const a = try alloc.create(add.AddExpression);
        a.* = .{ .lhs = lhs, .rhs = rhs };

        const e = try alloc.create(Expr);
        e.* = Expr{ .Add = a };
        return e;
    }

    pub fn createSub(alloc: std.mem.Allocator, lhs: *Expr, rhs: *Expr) !*Expr {
        const s = try alloc.create(sub.SubstractExpression);
        s.* = .{ .lhs = lhs, .rhs = rhs };

        const e = try alloc.create(Expr);
        e.* = Expr{ .Substract = s };
        return e;
    }

    pub fn createDiv(alloc: std.mem.Allocator, lhs: *Expr, rhs: *Expr) !*Expr {
        const d = try alloc.create(div.DivideExpression);
        d.* = .{ .lhs = lhs, .rhs = rhs };

        const e = try alloc.create(Expr);
        e.* = Expr{ .Divide = d };
        return e;
    }
};

// fn isLiteral(self: *const Expr, val: i32) bool {
//     return switch (self.*) {
//         Expr.Literal => |l| l == val,
//         else => false,
//     };
// }
// fn simplify(self: *const Expr, alloc: std.mem.Allocator) !Expr {
//     return switch (self.*) {
//         Expr.Variable, Expr.Literal => self.*,
//         Expr.Binary => |b| {
//             const left_expr = try b.left.simplify(alloc);
//             const right_expr = try b.right.simplify(alloc);
//
//             // constantes
//             if (left_expr == .Literal and right_expr == .Literal) {
//                 const left_val = left_expr.Literal;
//                 const right_val = right_expr.Literal;
//
//                 const val = switch (b.op) {
//                     BinOp.Add => left_val + right_val,
//                     BinOp.Subtract => left_val - right_val,
//                     BinOp.Multiply => left_val * right_val,
//                     BinOp.Divide => @divFloor(left_val, right_val),
//                     BinOp.Power => std.math.pow(i32, left_val, right_val),
//                 };
//
//                 return Expr{ .Literal = val };
//             }
//
//             // identidades
//             switch (b.op) {
//                 BinOp.Add => {
//                     // x + 0 = x
//                     if (right_expr.isLiteral(0)) return left_expr;
//                     // 0 + x = x
//                     if (left_expr.isLiteral(0)) return right_expr;
//                 },
//                 BinOp.Subtract => {
//                     // x - 0 = x
//                     if (right_expr.isLiteral(0)) return left_expr;
//                     // 0 - x = -x
//
//                     if (left_expr.isLiteral(0)) {
//                         const neg_one = try alloc.create(Expr);
//                         neg_one.* = Expr{ .Literal = -1 };
//
//                         const right_expr_neg = try alloc.create(Expr);
//                         right_expr_neg.* = right_expr;
//
//                         return Expr{ .Binary = .{ .op = .Multiply, .left = neg_one, .right = right_expr_neg } };
//                     }
//
//                     // x - x = 0 comparar arboles completos TODO
//                 },
//                 BinOp.Multiply => {
//                     if (left_expr.isLiteral(0) or right_expr.isLiteral(0)) return Expr{ .Literal = 0 };
//
//                     if (left_expr.isLiteral(1)) return right_expr;
//
//                     if (right_expr.isLiteral(1)) return left_expr;
//                 },
//                 BinOp.Divide => {
//                     if (right_expr.isLiteral(1)) return left_expr;
//
//                     // que sucede con x/0, panic?
//                     // que sucede con x/x = 1, comparacion de expr pendiente
//                 },
//                 BinOp.Power => {
//                     // x ^ 0 = 1
//                     if (right_expr.isLiteral(0)) return Expr{ .Literal = 1 };
//                     // x ^ 1 = x
//                     if (right_expr.isLiteral(1)) return left_expr;
//                 },
//             }
//
//             const left_ptr = try alloc.create(Expr);
//             left_ptr.* = left_expr;
//
//             const right_ptr = try alloc.create(Expr);
//             right_ptr.* = right_expr;
//
//             return Expr{ .Binary = BinaryExpr{ .left = left_ptr, .right = right_ptr, .op = b.op } };
//         },
//     };
// }
