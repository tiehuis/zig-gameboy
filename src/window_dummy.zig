const std = @import("std");
const Mmu = @import("mmu.zig").Mmu;

pub const NoError = error{None};

pub const Window = struct {
    pixels: [140][166]u32,

    pub fn init(mmu: *Mmu) NoError!Window {
        return Window{ .pixels = undefined };
    }

    pub fn deinit(w: *Window) void {}

    pub fn render(w: *Window) void {}

    pub fn handleEvents(w: *Window) NoError!void {}
};
