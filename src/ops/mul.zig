const std = @import("std");
const expr = @import("./expr.zig");

pub const MultiplyExpression = struct {
    lhs: *expr.Expr,
    rhs: *expr.Expr,

    pub fn eval(self: *const MultiplyExpression, vars: *const std.AutoHashMap(u8, i32)) i32 {
        const left_val = self.lhs.eval(vars);
        const right_val = self.rhs.eval(vars);

        return left_val * right_val;
    }
    pub fn derivative(self: *const MultiplyExpression, vars: u8, alloc: std.mem.Allocator) !*expr.Expr {
        const left_deriv = try self.lhs.derivative(vars, alloc);
        const right_deriv = try self.rhs.derivative(vars, alloc);

        const left_mul = try expr.Expr.createMul(alloc, left_deriv, self.rhs);
        const right_mul = try expr.Expr.createMul(alloc, self.lhs, right_deriv);
        const add_expr = try expr.Expr.createAdd(alloc, left_mul, right_mul);

        return add_expr;
    }
    pub fn string(self: *const MultiplyExpression, alloc: std.mem.Allocator) ![]const u8 {
        const left_str = try self.lhs.string(alloc);
        const right_str = try self.rhs.string(alloc);

        return try std.fmt.allocPrint(alloc, "({s} * {s})", .{ left_str, right_str });
    }
    pub fn simplify(self: *const MultiplyExpression, alloc: std.mem.Allocator) !*expr.Expr {
        const left_simp = try self.lhs.simplify(alloc);
        const right_simp = try self.rhs.simplify(alloc);

        // Constant folding
        if (left_simp.isConstant() and right_simp.isConstant()) {
            const left_val = left_simp.Literal;
            const right_val = right_simp.Literal;
            return try expr.Expr.createLiteral(alloc, left_val * right_val);
        }

        // x + 0 => x and 0 + x => x
        if (left_simp.isConstantValue(0) or right_simp.isConstantValue(0)) return try expr.Expr.createLiteral(alloc, 0);
        if (left_simp.isConstantValue(1)) return right_simp;
        if (right_simp.isConstantValue(1)) return left_simp;

        // Combine like terms: x * x => x ^ 2 TODO

        // terminos semejantes: 2*x * 3*x => 6x^2 TODO

        return try expr.Expr.createMul(alloc, left_simp, right_simp);
    }
};
