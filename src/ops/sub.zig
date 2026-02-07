const std = @import("std");
const expr = @import("./expr.zig");

pub const SubstractExpression = struct {
    lhs: *expr.Expr,
    rhs: *expr.Expr,

    pub fn eval(self: *const SubstractExpression, vars: *const std.AutoHashMap(u8, i32)) i32 {
        const left_val = self.lhs.eval(vars);
        const right_val = self.rhs.eval(vars);

        return left_val - right_val;
    }
    pub fn derivative(self: *const SubstractExpression, vars: u8, alloc: std.mem.Allocator) !*expr.Expr {
        const left_deriv = try self.lhs.derivative(vars, alloc);
        const right_deriv = try self.rhs.derivative(vars, alloc);

        return try expr.Expr.createSub(alloc, left_deriv, right_deriv);
    }
    pub fn string(self: *const SubstractExpression, alloc: std.mem.Allocator) ![]const u8 {
        const left_str = try self.lhs.string(alloc);
        const right_str = try self.rhs.string(alloc);

        return try std.fmt.allocPrint(alloc, "({s} - {s})", .{ left_str, right_str });
    }
    pub fn simplify(self: *const SubstractExpression, alloc: std.mem.Allocator) !*expr.Expr {
        const left_simp = try self.lhs.simplify(alloc);
        const right_simp = try self.rhs.simplify(alloc);

        // Constant folding
        if (left_simp.isConstant() and right_simp.isConstant()) {
            const left_val = left_simp.Literal;
            const right_val = right_simp.Literal;
            return try expr.Expr.createLiteral(alloc, left_val - right_val);
        }

        // x + 0 => x and 0 + x => x
        if (right_simp.isConstantValue(0)) return left_simp;
        if (left_simp.isConstantValue(0)) {
            const minus_one = try expr.Expr.createLiteral(alloc, -1);
            const neg_rhs = try expr.Expr.createMul(alloc, minus_one, right_simp);
            return neg_rhs;
        }

        // Combine like terms: x - x => 0 TODO

        // terminos semejantes: 2*x - 3*x => -1 * x TODO

        return try expr.Expr.createSub(alloc, left_simp, right_simp);
    }
};
