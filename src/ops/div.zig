const std = @import("std");
const expr = @import("./expr.zig");

pub const DivideExpression = struct {
    lhs: *expr.Expr,
    rhs: *expr.Expr,

    pub fn eval(self: *const DivideExpression, vars: *const std.AutoHashMap(u8, i32)) i32 {
        const left_val = self.lhs.eval(vars);
        const right_val = self.rhs.eval(vars);

        return left_val / right_val;
    }
    pub fn derivative(self: *const DivideExpression, vars: u8, alloc: std.mem.Allocator) !*expr.Expr {
        const left_deriv = try self.lhs.derivative(vars, alloc);
        const right_deriv = try self.rhs.derivative(vars, alloc);

        const n1_expr = try expr.Expr.createMul(alloc, left_deriv, self.rhs);
        const n2_expr = try expr.Expr.createMul(alloc, self.lhs, right_deriv);
        const numerator_expr = try expr.Expr.createSub(alloc, n1_expr, n2_expr);
        const denominator_expr = try expr.Expr.createMul(alloc, self.rhs, self.rhs);
        const div_expr = try expr.Expr.createDiv(alloc, numerator_expr, denominator_expr);

        return div_expr;
    }
    pub fn string(self: *const DivideExpression, alloc: std.mem.Allocator) ![]const u8 {
        const left_str = try self.lhs.string(alloc);
        const right_str = try self.rhs.string(alloc);

        return try std.fmt.allocPrint(alloc, "({s} / {s})", .{ left_str, right_str });
    }
    pub fn simplify(self: *const DivideExpression, alloc: std.mem.Allocator) !*expr.Expr {
        const left_simp = try self.lhs.simplify(alloc);
        const right_simp = try self.rhs.simplify(alloc);

        // Constant folding
        if (left_simp.isConstant() and right_simp.isConstant()) {
            const left_val = left_simp.Literal;
            const right_val = right_simp.Literal;
            return try expr.Expr.createLiteral(alloc, @divFloor(left_val, right_val));
        }

        //if (right_simp.isConstantValue(0)) return right_simp; // TODO error
        if (right_simp.isConstantValue(1)) return left_simp;

        // Combine like terms: x / x => 1 TODO

        // terminos semejantes: 2*x + 3*x => 5*x TODO

        return try expr.Expr.createDiv(alloc, left_simp, right_simp);
    }
};
