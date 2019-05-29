const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();

    var exe = b.addExecutable("gameboy", "src/main.zig");
    exe.setBuildMode(mode);

    b.detectNativeSystemPaths();

    exe.linkSystemLibrary("c");
    exe.linkSystemLibrary("SDL2");
    exe.setOutputDir(".");

    b.default_step.dependOn(&exe.step);
    b.installArtifact(exe);
}
