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
        if (lhs.is(.Literal) and rhs.is(.Literal)) {
            const left_val = lhs.Literal;
            const right_val = rhs.Literal;
            return try expr.Expr.createLiteral(alloc, left_val + right_val);
        }

        // x + 0 => x and 0 + x => x
        if (lhs.isConstantValue(0)) return rhs;
        if (rhs.isConstantValue(0)) return lhs;

        if (lhs.isEqual(rhs)) {
            const two = try expr.Expr.createLiteral(alloc, 2);

            return try expr.Expr.createMul(alloc, two, lhs);
        }

        // (A * f(x)) + (B * f(x)) => (A + B) * f(x)
        if (lhs.is(.Multiply) and rhs.is(.Multiply)) {
            const left_mul = lhs.Multiply;
            const right_mul = rhs.Multiply;

            const same_rhs = left_mul.rhs.isEqual(right_mul.rhs);
            const rhs_is_constant = left_mul.lhs.is(.Literal) and right_mul.lhs.is(.Literal);

            if (same_rhs and rhs_is_constant) {
                const new_const = try expr.Expr.createLiteral(alloc, left_mul.lhs.Literal + right_mul.lhs.Literal);

                const new_expr = try expr.Expr.createMul(alloc, new_const, left_mul.rhs);

                return try new_expr.simplify(alloc);
            }
        }

        return try expr.Expr.createAdd(alloc, lhs, rhs);
    }
};
