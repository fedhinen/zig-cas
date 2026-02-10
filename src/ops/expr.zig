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

    pub fn isEqual(self: *const Expr, other: *const Expr) bool {
        // Comparar si son literales
        if (self.is(.Literal) and other.is(.Literal)) {
            return self.Literal == other.Literal;
        }
        // Comparar si son variables
        if (self.is(.Variable) and other.is(.Variable)) {
            return self.Variable == other.Variable;
        }

        if (self.is(.Pow) and other.is(.Pow)) {
            const self_pow = self.Pow;
            const other_pow = other.Pow;
            return self_pow.base.isEqual(other_pow.base) and self_pow.exponent.isEqual(other_pow.exponent);
        }

        // comparar
        if (self.is(.Add) and other.is(.Add)) {
            const self_add = self.Add;
            const other_add = other.Add;
            return self_add.lhs.isEqual(other_add.lhs) and self_add.rhs.isEqual(other_add.rhs);
        }

        if (self.is(.Substract) and other.is(.Substract)) {
            const self_sub = self.Substract;
            const other_sub = other.Substract;
            return self_sub.lhs.isEqual(other_sub.lhs) and self_sub.rhs.isEqual(other_sub.rhs);
        }

        if (self.is(.Multiply) and other.is(.Multiply)) {
            const self_mul = self.Multiply;
            const other_mul = other.Multiply;
            return self_mul.lhs.isEqual(other_mul.lhs) and self_mul.rhs.isEqual(other_mul.rhs);
        }

        if (self.is(.Divide) and other.is(.Divide)) {
            const self_div = self.Divide;
            const other_div = other.Divide;
            return self_div.numerator.isEqual(other_div.numerator) and self_div.denominator.isEqual(other_div.denominator);
        }

        return false;
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
        d.* = .{ .numerator = lhs, .denominator = rhs };

        const e = try alloc.create(Expr);
        e.* = Expr{ .Divide = d };
        return e;
    }

    // is?
    pub fn is(self: *const Expr, ty: ExprTy) bool {
        return switch (self.*) {
            Expr.Variable => ty == .Variable,
            Expr.Literal => ty == .Literal,
            Expr.Add => ty == .Add,
            Expr.Substract => ty == .Substract,
            Expr.Multiply => ty == .Multiply,
            Expr.Divide => ty == .Divide,
            Expr.Pow => ty == .Pow,
        };
    }

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
