const std = @import("std");
const expr = @import("./expr.zig");

pub const DivideExpression = struct {
    numerator: *expr.Expr,
    denominator: *expr.Expr,

    pub fn eval(self: *const DivideExpression, vars: *const std.AutoHashMap(u8, i32)) i32 {
        const numerator_val = self.numerator.eval(vars);
        const denominator_val = self.denominator.eval(vars);

        return numerator_val / denominator_val;
    }
    pub fn derivative(self: *const DivideExpression, vars: u8, alloc: std.mem.Allocator) !*expr.Expr {
        const numerator_deriv = try self.numerator.derivative(vars, alloc);
        const denominator_deriv = try self.denominator.derivative(vars, alloc);

        const n1_expr = try expr.Expr.createMul(alloc, numerator_deriv, self.denominator);
        const n2_expr = try expr.Expr.createMul(alloc, self.numerator, denominator_deriv);
        const numerator_expr = try expr.Expr.createSub(alloc, n1_expr, n2_expr);
        const denominator_expr = try expr.Expr.createMul(alloc, self.denominator, self.denominator);
        const div_expr = try expr.Expr.createDiv(alloc, numerator_expr, denominator_expr);

        return div_expr;
    }
    pub fn string(self: *const DivideExpression, alloc: std.mem.Allocator) ![]const u8 {
        const numerator_str = try self.numerator.string(alloc);
        const denominator_str = try self.denominator.string(alloc);

        return try std.fmt.allocPrint(alloc, "({s} / {s})", .{ numerator_str, denominator_str });
    }
    pub fn simplify(self: *const DivideExpression, alloc: std.mem.Allocator) !*expr.Expr {
        const numerator_simp = try self.numerator.simplify(alloc);
        const denominator_simp = try self.denominator.simplify(alloc);

        // Constant folding
        if (numerator_simp.isConstant() and denominator_simp.isConstant()) {
            const numerator_val = numerator_simp.Literal;
            const denominator_val = denominator_simp.Literal;
            return try expr.Expr.createLiteral(alloc, @divFloor(numerator_val, denominator_val));
        }

        //if (denominator_simp.isConstantValue(0)) return right_simp; // TODO error
        if (denominator_simp.isConstantValue(1)) return numerator_simp;

        // Combine like terms: x / x => 1 TODO
        if (numerator_simp.isEqual(denominator_simp)) {
            return try expr.Expr.createLiteral(alloc, 1);
        }

        // terminos semejantes: 2*x + 3*x => 5*x TODO

        return try expr.Expr.createDiv(alloc, numerator_simp, denominator_simp);
    }
};
