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

    pub fn simplify(self: *const Expr, alloc: std.mem.Allocator) std.mem.Allocator.Error!*Expr {
        return switch (self.*) {
            Expr.Variable => try createVar(alloc, self.Variable),
            Expr.Literal => try createLiteral(alloc, self.Literal),
            Expr.Add => |e| try e.simplify(alloc),
            Expr.Substract => |e| try e.simplify(alloc),
            Expr.Multiply => |e| try e.simplify(alloc),
            Expr.Divide => |e| try e.simplify(alloc),
            Expr.Pow => |e| try e.simplify(alloc),
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
    pub fn swap(lhs: *Expr, rhs: *Expr) struct { lhs: *Expr, rhs: *Expr } {
        if (rhs.isConstant() and !lhs.isConstant()) {
            return .{ .lhs = rhs, .rhs = lhs };
        } else {
            return .{ .lhs = lhs, .rhs = rhs };
        }
    }
    pub fn createMul(alloc: std.mem.Allocator, lhs: *Expr, rhs: *Expr) !*Expr {
        const swapped = swap(lhs, rhs);

        const m = try alloc.create(mul.MultiplyExpression);
        m.* = .{ .lhs = swapped.lhs, .rhs = swapped.rhs };

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
        const swapped = swap(lhs, rhs);

        const a = try alloc.create(add.AddExpression);
        a.* = .{ .lhs = swapped.lhs, .rhs = swapped.rhs };

        const e = try alloc.create(Expr);
        e.* = Expr{ .Add = a };
        return e;
    }

    pub fn createSub(alloc: std.mem.Allocator, lhs: *Expr, rhs: *Expr) !*Expr {
        const s = try alloc.create(sub.SubstractExpression);
        s.* = .{ .lhs = lhs, .rhs = rhs };

        // x - 2 => (-2) + x
        if (rhs.isConstant() and !lhs.isConstant()) {
            const minus_one = try Expr.createLiteral(alloc, -1);
            const neg_rhs = try Expr.createMul(alloc, minus_one, rhs);

            const add_expr = try alloc.create(add.AddExpression);
            add_expr.* = .{ .lhs = neg_rhs, .rhs = lhs };

            const e = try alloc.create(Expr);
            e.* = Expr{ .Add = add_expr };
            return e;
        }

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

    // is?
    pub fn isConstant(self: *const Expr) bool {
        return switch (self.*) {
            Expr.Literal => true,
            else => false,
        };
    }
    pub fn isConstantValue(self: *const Expr, val: i32) bool {
        return switch (self.*) {
            Expr.Literal => |l| l == val,
            else => false,
        };
    }
    pub fn isLeftConstant(self: *const Expr) bool {
        return switch (self.*) {
            Expr.Multiply => |m| m.lhs.isConstant(),
            Expr.Add => |a| a.lhs.isConstant(),
            else => false,
        };
    }
};
