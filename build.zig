const Builder = @import("std").build.Builder;

pub fn build(b: &Builder) {
    const mode = b.standardReleaseOptions();

    var exe = b.addExecutable("gameboy", "src/main.zig");
    exe.setBuildMode(mode);

    b.default_step.dependOn(&exe.step);
    b.installArtifact(exe);

    const play = b.step("run", "Run the emulator");
    const run = b.addCommand(".", b.env_map, [][]const u8{ exe.getOutputPath() });
    play.dependOn(&run.step);
    run.step.dependOn(&exe.step);
}
