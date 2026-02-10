const std = @import("std");
const expr = @import("./expr.zig");

pub const AddExpression = struct {
    lhs: *expr.Expr,
    rhs: *expr.Expr,

    pub fn eval(self: *const AddExpression, vars: *const std.AutoHashMap(u8, i32)) i32 {
        const left_val = self.lhs.eval(vars);
        const right_val = self.rhs.eval(vars);

        return left_val + right_val;
    }
    pub fn derivative(self: *const AddExpression, vars: u8, alloc: std.mem.Allocator) !*expr.Expr {
        const left_deriv = try self.lhs.derivative(vars, alloc);
        const right_deriv = try self.rhs.derivative(vars, alloc);

        return try expr.Expr.createAdd(alloc, left_deriv, right_deriv);
    }
    pub fn string(self: *const AddExpression, alloc: std.mem.Allocator) ![]const u8 {
        const left_str = try self.lhs.string(alloc);
        const right_str = try self.rhs.string(alloc);

        return try std.fmt.allocPrint(alloc, "({s} + {s})", .{ left_str, right_str });
    }
    pub fn simplify(self: *const AddExpression, alloc: std.mem.Allocator) !*expr.Expr {
        const left_simp = try self.lhs.simplify(alloc);
        const right_simp = try self.rhs.simplify(alloc);

        const swapped = expr.Expr.swap(left_simp, right_simp);
        const lhs = swapped.lhs;
        const rhs = swapped.rhs;

        // Constant folding
        if (lhs.isConstant() and rhs.isConstant()) {
            const left_val = lhs.Literal;
            const right_val = rhs.Literal;
            return try expr.Expr.createLiteral(alloc, left_val + right_val);
        }

        // x + 0 => x and 0 + x => x
        if (lhs.isConstantValue(0)) return rhs;
        if (rhs.isConstantValue(0)) return lhs;

        // (A * f(x)) + (B * f(x)) => (A + B) * f(x)
        if (lhs.isLeftConstant() and rhs.isLeftConstant()) {
            if (!lhs.is(.Multiply) or !rhs.is(.Multiply))
                return try expr.Expr.createAdd(alloc, lhs, rhs);

            const left_mul = lhs.Multiply;
            const right_mul = rhs.Multiply;

            if (!left_mul.rhs.isEqual(right_mul.rhs))
                return try expr.Expr.createAdd(alloc, lhs, rhs);

            const left_const = lhs.Multiply.lhs.Literal;
            const right_const = rhs.Multiply.lhs.Literal;
            const new_const = try expr.Expr.createLiteral(alloc, left_const + right_const);

            const expr_assoc = try expr.Expr.createMul(alloc, new_const, lhs.Multiply.rhs);
            return try expr_assoc.simplify(alloc);
        }

        return try expr.Expr.createAdd(alloc, lhs, rhs);
    }
};
