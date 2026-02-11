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

        // f(x) * f(x) = f(x)^2
        const same_expr = lhs.isEqual(rhs);
        if (same_expr) {
            const two = try expr.Expr.createLiteral(alloc, 2);
            return try expr.Expr.createPow(alloc, two, lhs);
        }

        if (lhs.is(.Pow) and rhs.is(.Pow)) {
            const same_base = lhs.Pow.base.isEqual(rhs.Pow.base);

            if (same_base) {
                const left_exponent = lhs.Pow.exponent;
                const right_exponent = rhs.Pow.exponent;

                const new_exponent = try expr.Expr.createAdd(alloc, left_exponent, right_exponent);
                const simplified = try new_exponent.simplify(alloc);

                return try expr.Expr.createPow(alloc, lhs.Pow.base, simplified);
            }
        }

        const left_start_with_const = lhs.is(.Multiply) and lhs.Multiply.lhs.is(.Literal);
        const right_start_with_const = rhs.is(.Multiply) and rhs.Multiply.lhs.is(.Literal);
        if (left_start_with_const and right_start_with_const) {
            const lhs_right_term = lhs.Multiply.rhs;
            const rhs_right_term = rhs.Multiply.rhs;
            const lhs_const = lhs.Multiply.lhs;
            const rhs_const = rhs.Multiply.lhs;

            const same_right_term = lhs_right_term.isEqual(rhs_right_term);

            // (A * f(x)) * (B * f(x)) = (A * B)f(x)^2
            if (same_right_term) {
                const new_const = try expr.Expr.createLiteral(alloc, lhs_const.Literal * rhs_const.Literal);

                const two_literal = try expr.Expr.createLiteral(alloc, 2);
                const pow_expr = try expr.Expr.createPow(alloc, lhs_right_term, two_literal);
                return try expr.Expr.createMul(alloc, new_const, pow_expr);
            }

            const are_pow = lhs_right_term.is(.Pow) and rhs_right_term.is(.Pow);
            const same_base = lhs_right_term.Pow.base.isEqual(rhs_right_term.Pow.base);

            // (A * f(x) ^ n) * (B * f(x) ^ m) = (A * B) f(x) ^ (n + m)
            if (are_pow and same_base) {
                const new_const = try expr.Expr.createLiteral(alloc, lhs_const.Literal * rhs_const.Literal);

                const new_exponent = try expr.Expr.createAdd(alloc, lhs_right_term.Pow.exponent, rhs_right_term.Pow.exponent);
                const simplified_exponent = try new_exponent.simplify(alloc);

                const new_pow = try expr.Expr.createPow(alloc, lhs_right_term.Pow.base, simplified_exponent);
                return try expr.Expr.createMul(alloc, new_const, new_pow);
            }
        }

        // A * (B * x) => (A * B) * x, A is lhs, B is rhs
        const left_is_const = lhs.is(.Literal);

        if (left_is_const and right_start_with_const) {
            const left_const = lhs.Literal;
            const right_mul = rhs.Multiply;
            const right_const = right_mul.lhs.Literal;
            const right_fn = right_mul.rhs;

            const new_const = try expr.Expr.createLiteral(alloc, left_const * right_const);
            return try expr.Expr.createMul(alloc, new_const, right_fn);
        }

        // (A + B) * (C + D) => (A * (C + D)) + (B * (C + D)) => (A * C) + (A * D) + (B * C) + (B * D)
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
