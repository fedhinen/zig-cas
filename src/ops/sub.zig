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
};
