const std = @import("std");
const immu = @import("mmu.zig");
const Mmu = immu.Mmu;
const A = immu.addresses;

use @import("zig-sdl2");

pub const Window = struct {
    renderer: *SDL_Renderer,
    window: *SDL_Window,

    pixels: [144][160]u32,
    mmu: *Mmu,

    pub fn init(mmu: *Mmu) !Window {
        var w: Window = undefined;
        w.mmu = mmu;

        if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO) != 0) {
            return error.FailedToInitSDL;
        }
        errdefer SDL_Quit();

        if (SDL_CreateWindowAndRenderer(160 * 2, 144 * 2, SDL_WINDOW_SHOWN | SDL_WINDOW_ALLOW_HIGHDPI, &w.window, &w.renderer) != 0) {
            return error.FailedToInitWindowAndRenderer;
        }
        errdefer SDL_DestroyWindow(w.window);

        SDL_SetWindowResizable(w.window, SDL_bool.SDL_FALSE);

        SDL_SetWindowTitle(w.window, c"zig-gameboy");
        _ = SDL_SetRenderDrawColor(w.renderer, 255, 255, 255, 255);
        _ = SDL_RenderClear(w.renderer);
        _ = SDL_RenderPresent(w.renderer);

        return w;
    }

    pub fn deinit(w: *Window) void {
        SDL_DestroyWindow(w.window);
        SDL_Quit();
    }

    pub fn render(w: *Window) void {
        var y: usize = 0;
        while (y < 144) : (y += 1) {
            var x: usize = 0;
            while (x < 160) : (x += 1) {
                // TODO: Use surfaces instead and draw directly
                _ = SDL_SetRenderDrawColor(
                    w.renderer,
                    @truncate(u8, w.pixels[y][x] >> 24),
                    @truncate(u8, w.pixels[y][x] >> 16),
                    @truncate(u8, w.pixels[y][x] >> 8),
                    255,
                );

                _ = SDL_RenderDrawPoint(w.renderer, @intCast(c_int, x), @intCast(c_int, y));
            }
        }

        _ = SDL_RenderPresent(w.renderer);
    }

    pub fn handleEvents(w: *Window) !void {
        var ev: SDL_Event = undefined;
        while (SDL_PollEvent(&ev) != 0) {
            switch (ev.type) {
                SDL_QUIT => {
                    return error.Quit;
                },
                SDL_KEYDOWN => switch (ev.key.keysym.sym) {
                    SDLK_RETURN => w.mmu.joyp_bit[0] |= 0x8,
                    SDLK_SPACE => w.mmu.joyp_bit[0] |= 0x4,
                    SDLK_x => w.mmu.joyp_bit[0] |= 0x2,
                    SDLK_z => w.mmu.joyp_bit[0] |= 0x1,
                    SDLK_DOWN => w.mmu.joyp_bit[1] |= 0x8,
                    SDLK_UP => w.mmu.joyp_bit[1] |= 0x4,
                    SDLK_LEFT => w.mmu.joyp_bit[1] |= 0x2,
                    SDLK_RIGHT => w.mmu.joyp_bit[1] |= 0x1,
                    else => {},
                },
                SDL_KEYUP => switch (ev.key.keysym.sym) {
                    SDLK_RETURN => w.mmu.joyp_bit[0] &= 0xe,
                    SDLK_SPACE => w.mmu.joyp_bit[0] &= 0xd,
                    SDLK_x => w.mmu.joyp_bit[0] &= 0xb,
                    SDLK_z => w.mmu.joyp_bit[0] &= 0x7,
                    SDLK_DOWN => w.mmu.joyp_bit[1] &= 0xe,
                    SDLK_UP => w.mmu.joyp_bit[1] &= 0xd,
                    SDLK_LEFT => w.mmu.joyp_bit[1] &= 0xb,
                    SDLK_RIGHT => w.mmu.joyp_bit[1] &= 0x7,
                    else => {},
                },
                else => {},
            }

            // Update active keys in joyp on any key change
            w.mmu.mem[A.JOYP] = w.mmu.joyp_bit[w.mmu.joyp_active];
        }
    }
};
