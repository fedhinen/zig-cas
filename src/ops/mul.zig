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

        const swapped = expr.Expr.swap(left_simp, right_simp);
        const lhs = swapped.lhs;
        const rhs = swapped.rhs;

        // Constant folding
        if (lhs.isConstant() and rhs.isConstant()) {
            const left_val = lhs.Literal;
            const right_val = rhs.Literal;
            return try expr.Expr.createLiteral(alloc, left_val * right_val);
        }

        // x + 0 => x and 0 + x => x
        if (lhs.isConstantValue(0) or rhs.isConstantValue(0)) return try expr.Expr.createLiteral(alloc, 0);
        if (lhs.isConstantValue(1)) return rhs;
        if (rhs.isConstantValue(1)) return lhs;

        // A * (B * x) => (A * B) * x, A is lhs, B is rhs
        if (lhs.isConstant() and rhs.isLeftConstant()) {
            const left_val = lhs.Literal;

            if (!rhs.is(.Multiply))
                return try expr.Expr.createMul(alloc, lhs, rhs);

            const right_const = rhs.Multiply;

            const new_const = try expr.Expr.createLiteral(alloc, left_val * right_const.lhs.Literal);

            const expr_assoc = try expr.Expr.createMul(alloc, new_const, right_const.rhs);

            return try expr_assoc.simplify(alloc);
        }

        // Terminos semejantes: 2 * x * 3 => 6 * x
        // (A * f(x)) * (B * f(x)) => (A * B) * f(x)^2
        if (lhs.is(.Multiply) and rhs.is(.Multiply)) {
            if (!lhs.Multiply.lhs.isConstant() or !rhs.Multiply.lhs.isConstant())
                return try expr.Expr.createMul(alloc, lhs, rhs);

            if (!lhs.Multiply.rhs.isEqual(rhs.Multiply.rhs))
                return try expr.Expr.createMul(alloc, lhs, rhs);

            if (!lhs.Multiply.rhs.isEqual(rhs.Multiply.rhs))
                return try expr.Expr.createMul(alloc, lhs, rhs);

            const left_const = lhs.Multiply.lhs.Literal;
            const right_const = rhs.Multiply.lhs.Literal;
            const new_const = try expr.Expr.createLiteral(alloc, left_const * right_const);

            const two = try expr.Expr.createLiteral(alloc, 2);
            const pow_expr = try expr.Expr.createPow(alloc, lhs.Multiply.rhs, two);
            const expr_assoc = try expr.Expr.createMul(alloc, new_const, pow_expr);
            return try expr_assoc.simplify(alloc);
        }

        // Distributive property: (A + B) * (C + D) => (A * (C + D)) + (B * (C + D)) => (A * C) + (A * D) + (B * C) + (B * D)
        if (lhs.is(.Add) and rhs.is(.Add)) {
            const a = lhs.Add.lhs;
            const b = lhs.Add.rhs;
            const c = rhs.Add.lhs;
            const d = rhs.Add.rhs;

            const ac = try expr.Expr.createMul(alloc, a, c);
            const ad = try expr.Expr.createMul(alloc, a, d);
            const bc = try expr.Expr.createMul(alloc, b, c);
            const bd = try expr.Expr.createMul(alloc, b, d);

            const ac_ad = try expr.Expr.createAdd(alloc, ac, ad);
            const bc_bd = try expr.Expr.createAdd(alloc, bc, bd);

            const ac_ad_bc_bd = try expr.Expr.createAdd(alloc, ac_ad, bc_bd);

            return try ac_ad_bc_bd.simplify(alloc);
        }
        return try expr.Expr.createMul(alloc, lhs, rhs);
    }
};
