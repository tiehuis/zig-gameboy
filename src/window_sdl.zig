const std = @import("std");
const immu = @import("mmu.zig");
const Mmu = immu.Mmu;
const A = immu.addresses;

const c = @cImport({
    @cInclude("SDL2/SDL.h");
});

pub const Window = struct {
    renderer: ?*c.SDL_Renderer,
    window: ?*c.SDL_Window,

    pixels: [144][160]u32,
    mmu: *Mmu,

    pub fn init(mmu: *Mmu) !Window {
        var w: Window = undefined;
        w.mmu = mmu;

        if (c.SDL_Init(c.SDL_INIT_VIDEO | c.SDL_INIT_AUDIO) != 0) {
            return error.FailedToInitSDL;
        }
        errdefer c.SDL_Quit();

        if (c.SDL_CreateWindowAndRenderer(160 * 2, 144 * 2, c.SDL_WINDOW_SHOWN | c.SDL_WINDOW_ALLOW_HIGHDPI, &w.window, &w.renderer) != 0) {
            return error.FailedToInitWindowAndRenderer;
        }
        errdefer c.SDL_DestroyWindow(w.window);

        c.SDL_SetWindowResizable(w.window, c.SDL_bool.SDL_FALSE);

        c.SDL_SetWindowTitle(w.window, c"zig-gameboy");
        _ = c.SDL_SetRenderDrawColor(w.renderer, 255, 255, 255, 255);
        _ = c.SDL_RenderClear(w.renderer);
        _ = c.SDL_RenderPresent(w.renderer);

        return w;
    }

    pub fn deinit(w: *Window) void {
        c.SDL_DestroyWindow(w.window);
        c.SDL_Quit();
    }

    pub fn render(w: *Window) void {
        var y: usize = 0;
        while (y < 144) : (y += 1) {
            var x: usize = 0;
            while (x < 160) : (x += 1) {
                // TODO: Use surfaces instead and draw directly
                _ = c.SDL_SetRenderDrawColor(
                    w.renderer,
                    @truncate(u8, w.pixels[y][x] >> 24),
                    @truncate(u8, w.pixels[y][x] >> 16),
                    @truncate(u8, w.pixels[y][x] >> 8),
                    255,
                );

                _ = c.SDL_RenderDrawPoint(w.renderer, @intCast(c_int, x), @intCast(c_int, y));
            }
        }

        _ = c.SDL_RenderPresent(w.renderer);
    }

    pub fn handleEvents(w: *Window) !void {
        var ev: c.SDL_Event = undefined;
        while (c.SDL_PollEvent(&ev) != 0) {
            switch (ev.type) {
                c.SDL_QUIT => {
                    return error.Quit;
                },
                c.SDL_KEYDOWN => switch (ev.key.keysym.sym) {
                    c.SDLK_RETURN => w.mmu.joyp_bit[0] |= 0x8,
                    c.SDLK_SPACE => w.mmu.joyp_bit[0] |= 0x4,
                    c.SDLK_x => w.mmu.joyp_bit[0] |= 0x2,
                    c.SDLK_z => w.mmu.joyp_bit[0] |= 0x1,
                    c.SDLK_DOWN => w.mmu.joyp_bit[1] |= 0x8,
                    c.SDLK_UP => w.mmu.joyp_bit[1] |= 0x4,
                    c.SDLK_LEFT => w.mmu.joyp_bit[1] |= 0x2,
                    c.SDLK_RIGHT => w.mmu.joyp_bit[1] |= 0x1,
                    else => {},
                },
                c.SDL_KEYUP => switch (ev.key.keysym.sym) {
                    c.SDLK_RETURN => w.mmu.joyp_bit[0] &= 0xe,
                    c.SDLK_SPACE => w.mmu.joyp_bit[0] &= 0xd,
                    c.SDLK_x => w.mmu.joyp_bit[0] &= 0xb,
                    c.SDLK_z => w.mmu.joyp_bit[0] &= 0x7,
                    c.SDLK_DOWN => w.mmu.joyp_bit[1] &= 0xe,
                    c.SDLK_UP => w.mmu.joyp_bit[1] &= 0xd,
                    c.SDLK_LEFT => w.mmu.joyp_bit[1] &= 0xb,
                    c.SDLK_RIGHT => w.mmu.joyp_bit[1] &= 0x7,
                    else => {},
                },
                else => {},
            }

            // Update active keys in joyp on any key change
            w.mmu.mem[A.JOYP] = w.mmu.joyp_bit[w.mmu.joyp_active];
        }
    }
};
