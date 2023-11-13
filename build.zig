const std = @import("std");
const zigbo = @import("src/main.zig");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const install_test_option = b.option(bool, "install-test", "When running install steps, also run respective test steps") orelse false;
    const zigbo_steps_option = b.option(bool, "zigbo", "Enable zigbo steps") orelse true;

    const test_step = b.step("test", "Run all the tests");

    const install_lib_step = b.step("install-lib", "Copy library build artifacts to prefix path");
    const lib = b.addStaticLibrary(.{
        .name = "zigbo",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    install_lib_step.dependOn(&lib.step);
    b.getInstallStep().dependOn(install_lib_step);

    const test_lib_step = b.step("test-lib", "Run the library tests");
    if (install_test_option) install_lib_step.dependOn(test_lib_step);
    test_step.dependOn(test_lib_step);
    const lib_tests = b.addTest(.{
        .name = "test-lib",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    test_lib_step.dependOn(&lib_tests.step);

    if (zigbo_steps_option) {
        const stdout = std.io.getStdOut().writer();
        const build_graph = zigbo.addGraphOutput(b, @TypeOf(stdout), .{ .writer = stdout });
        build_graph.setCustomStepCallback(customCallback);

        const build_graph_step = b.step("graph", "Output the build graph as a mermaid diagram");
        build_graph_step.dependOn(&build_graph.step);
    }
}

fn customCallback(step: *std.Build.Step, writer: anytype) !?zigbo.Step.GraphOutputWriteFnInstruction {
    _ = writer;
    if (std.mem.eql(u8, step.name, "foobarbaz\x00" ** 23)) return null;
    return .use_default_implementation;
}
