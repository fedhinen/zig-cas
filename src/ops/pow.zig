const std = @import("std");
const expr = @import("./expr.zig");

pub const PowExpression = struct {
    base: *expr.Expr,
    exponent: *expr.Expr,

    pub fn eval(self: *const PowExpression, vars: *const std.AutoHashMap(u8, i32)) i32 {
        const base_val = self.base.eval(vars);
        const exponent_val = self.exponent.eval(vars);

        return std.math.pow(i32, base_val, exponent_val);
    }
    pub fn derivative(self: *const PowExpression, vars: u8, alloc: std.mem.Allocator) !*expr.Expr {
        const base_deriv = try self.base.derivative(vars, alloc);

        const minus_one = try expr.Expr.createLiteral(alloc, 1);
        const new_exponent = try expr.Expr.createSub(alloc, self.exponent, minus_one);
        const pow_expr = try expr.Expr.createPow(alloc, self.base, new_exponent);
        const base_deriv_mul = try expr.Expr.createMul(alloc, self.exponent, pow_expr);
        const final_expr = try expr.Expr.createMul(alloc, base_deriv_mul, base_deriv);

        return final_expr;
    }
    pub fn string(self: *const PowExpression, alloc: std.mem.Allocator) ![]const u8 {
        const base_str = try self.base.string(alloc);
        const exponent_str = try self.exponent.string(alloc);

        return try std.fmt.allocPrint(alloc, "({s} ^ {s})", .{ base_str, exponent_str });
    }
    pub fn simplify(self: *const PowExpression, alloc: std.mem.Allocator) !*expr.Expr {
        const base_simp = try self.base.simplify(alloc);
        const pow_simp = try self.exponent.simplify(alloc);

        // Constant folding
        if (base_simp.isConstant() and pow_simp.isConstant()) {
            const left_val = base_simp.Literal;
            const right_val = pow_simp.Literal;
            return try expr.Expr.createLiteral(alloc, std.math.pow(i32, left_val, right_val));
        }

        if (pow_simp.isConstantValue(0)) return try expr.Expr.createLiteral(alloc, 1);
        if (pow_simp.isConstantValue(1)) return base_simp;

        return try expr.Expr.createPow(alloc, base_simp, pow_simp);
    }
};
