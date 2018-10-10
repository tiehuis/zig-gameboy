const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();

    var exe = b.addExecutable("gameboy", "src/main.zig");
    exe.setBuildMode(mode);
    exe.addPackagePath("zig-sdl2", "lib/zig-sdl2/src/index.zig");
    exe.linkSystemLibrary("c");
    exe.linkSystemLibrary("SDL2");
    exe.setOutputPath("./gameboy");

    b.default_step.dependOn(&exe.step);
    b.installArtifact(exe);
}
