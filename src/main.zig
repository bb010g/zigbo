const std = @import("std");
const zigbo = @This();

pub fn addGraphOutput(b: *std.Build, comptime Writer: type, options: Step.GraphOutput(Writer).Options) *Step.GraphOutput(Writer) {
    return Step.GraphOutput(Writer).create(b, options);
}

pub const Step = struct {
    /// Outputs a mermaid diagram describing the build graph
    pub fn GraphOutput(comptime WriterT: type) type {
        return struct {
            const GraphOutputStep = @This();
            pub const Writer = WriterT;

            step: std.Build.Step,
            name: []const u8,
            writer: Writer,
            custom_step_callback: *const WriteFn = defaultCustomStepCallback,

            pub const Options = struct {
                name: []const u8 = "zigbo graph",
                writer: Writer,
            };

            pub fn create(owner: *std.Build, options: Options) *GraphOutputStep {
                const self = owner.allocator.create(GraphOutputStep) catch @panic("OOM");
                const name = owner.dupe(options.name);
                self.* = .{
                    .name = name,
                    .step = std.Build.Step.init(.{
                        .id = .custom,
                        .name = "zigbo graph",
                        .owner = owner,
                        .makeFn = GraphOutputStep.make,
                    }),
                    .writer = options.writer,
                };
                return self;
            }

            pub fn defaultCustomStepCallback(step: *std.Build.Step, writer: Writer) Writer.Error!?WriteFnInstruction {
                _ = step;
                _ = writer;
                return .use_default_implementation;
            }

            pub fn setCustomStepCallback(self: *GraphOutputStep, comptime callback: anytype) void {
                switch (@typeInfo(@TypeOf(callback))) {
                    .Fn => {},
                    else => @compileError("Expected function, got " ++ @typeName(@TypeOf(callback))),
                }

                const gen = struct {
                    fn callbackWrapper(step: *std.Build.Step, writer: Writer) Writer.Error!?WriteFnInstruction {
                        return @call(.always_inline, callback, .{ step, writer });
                    }
                };

                self.custom_step_callback = gen.callbackWrapper;
            }

            pub const AnnotatedBuiltinStep = struct {
                step: *std.Build.Step,
                graph_output: *GraphOutputStep,

                pub fn format(
                    annotated_builtin_step: AnnotatedBuiltinStep,
                    comptime fmt: []const u8,
                    options: std.fmt.FormatOptions,
                    writer: Writer,
                ) !void {
                    _ = fmt;
                    _ = options;

                    return switch (annotated_builtin_step.step.id) {
                        .top_level => step_inspection_functions.annotateTopLevelStep(annotated_builtin_step.step, writer),
                        .install_artifact => step_inspection_functions.annotateInstallArtifact(annotated_builtin_step.step, writer),
                        .compile => step_inspection_functions.annotateCompileStep(annotated_builtin_step.step, writer, .{
                            .print_newlines = true,
                            .print_in_between_quotes = true,
                        }),
                        .run => step_inspection_functions.annotateRunStep(annotated_builtin_step.step, writer),
                        .custom => blk: {
                            const customFormatFn = annotated_builtin_step.graph_output.custom_step_callback;
                            const maybe_instruction = try customFormatFn(annotated_builtin_step.step, writer);
                            const instruction = maybe_instruction orelse break :blk;
                            switch (instruction) {
                                .use_default_implementation => try writer.print("\"{s} (Custom)\"", .{annotated_builtin_step.step.name}),
                            }
                        },

                        // TODO(haze): better messages for these
                        .options => try writer.writeAll("Options step"),
                        .objcopy => try writer.writeAll("ObjCopy step"),
                        .config_header => try writer.writeAll("Configure Header step"),
                        .check_object => try writer.writeAll("CHeck Object step"),
                        .check_file => try writer.writeAll("Check File step"),
                        .write_file => try writer.writeAll("Write File step"),
                        .translate_c => try writer.writeAll("Translate-C step"),
                        .fmt => try writer.writeAll("Format step"),
                        .remove_dir => try writer.writeAll("Remove Directory step"),
                        .install_dir => try writer.writeAll("Install Directory step"),
                        .install_file => try writer.writeAll("Install File step"),
                    };
                }
            };

            pub const Header = struct {
                direction: Direction,

                pub const Direction = enum {
                    top_to_bottom,
                    top_down,
                    bottom_to_top,
                    right_to_left,
                    left_to_right,

                    pub fn format(
                        direction: Direction,
                        comptime fmt: []const u8,
                        options: std.fmt.FormatOptions,
                        writer: Writer,
                    ) !void {
                        _ = options;
                        _ = fmt;
                        try writer.writeAll(switch (direction) {
                            .top_to_bottom => "TB",
                            .top_down => "TD",
                            .bottom_to_top => "BT",
                            .right_to_left => "RL",
                            .left_to_right => "LR",
                        });
                    }
                };

                pub fn format(
                    header: Header,
                    comptime fmt: []const u8,
                    options: std.fmt.FormatOptions,
                    writer: Writer,
                ) !void {
                    _ = options;
                    _ = fmt;
                    try writer.print("flowchart {}", .{header.direction});
                }
            };

            pub const Subgraph = struct {
                pub const Start = struct {
                    id: usize,
                    label: []const u8,

                    pub fn format(
                        start: Start,
                        comptime fmt: []const u8,
                        options: std.fmt.FormatOptions,
                        writer: Writer,
                    ) !void {
                        _ = options;
                        _ = fmt;
                        try writer.print("subgraph tls_{} [\"{s}\"]", .{ start.id, start.label });
                    }
                };
                pub const End = struct {
                    pub fn format(
                        end: End,
                        comptime fmt: []const u8,
                        options: std.fmt.FormatOptions,
                        writer: Writer,
                    ) !void {
                        _ = end;
                        _ = options;
                        _ = fmt;
                        try writer.writeAll("end");
                    }
                };
            };

            pub const Edge = struct {
                parent: *std.Build.Step,
                parent_id: usize,
                include_parent_description: bool,

                child: *std.Build.Step,
                child_id: usize,
                include_child_description: bool,

                graph_output: *GraphOutputStep,

                pub fn format(
                    edge: Edge,
                    comptime fmt: []const u8,
                    options: std.fmt.FormatOptions,
                    writer: Writer,
                ) !void {
                    _ = options;
                    _ = fmt;
                    try writer.print("step_{}", .{
                        edge.parent_id,
                    });
                    if (edge.include_parent_description) {
                        try writer.print("[{}]", .{
                            AnnotatedBuiltinStep{
                                .step = edge.parent,
                                .graph_output = edge.graph_output,
                            },
                        });
                    }
                    try writer.writeAll(" --> ");
                    try writer.print("step_{}", .{
                        edge.child_id,
                    });
                    if (edge.include_child_description) {
                        try writer.print("[{}]", .{
                            AnnotatedBuiltinStep{
                                .step = edge.child,
                                .graph_output = edge.graph_output,
                            },
                        });
                    }
                }
            };

            pub const Node = struct {
                item: *std.Build.Step,
                id: usize,
                include_description: bool,
                graph_output: *GraphOutputStep,

                pub fn format(
                    node: Node,
                    comptime fmt: []const u8,
                    options: std.fmt.FormatOptions,
                    writer: Writer,
                ) !void {
                    _ = options;
                    _ = fmt;
                    try writer.print("step_{}", .{
                        node.id,
                    });
                    if (node.include_description) {
                        try writer.print("[{}]", .{
                            AnnotatedBuiltinStep{
                                .step = node.item,
                                .graph_output = node.graph_output,
                            },
                        });
                    }
                }
            };

            pub const WriteFn = fn (step: *std.Build.Step, writer: Writer) Writer.Error!?WriteFnInstruction;
            pub const WriteFnInstruction = GraphOutputWriteFnInstruction;

            fn make(type_erased_graph_output: *std.Build.Step, prog_node: *std.Progress.Node) anyerror!void {
                _ = prog_node;
                const graph_output = @fieldParentPtr(GraphOutputStep, "step", type_erased_graph_output);
                const b = graph_output.step.owner;
                const gpa = b.allocator;
                try graph_output.writer.print("{}\n", .{Header{ .direction = .top_down }});
                var subgraph_counter: usize = 0;

                var step_stack = std.AutoArrayHashMapUnmanaged(*std.Build.Step, void){};
                defer step_stack.deinit(gpa);

                var top_level_step_stack_map = std.AutoArrayHashMapUnmanaged(*std.Build.Step, std.ArrayListUnmanaged(*std.Build.Step)){};
                defer for (top_level_step_stack_map.values()) |*top_level_step_stack| {
                    top_level_step_stack.deinit(gpa);
                };
                defer top_level_step_stack_map.deinit(gpa);
                try top_level_step_stack_map.ensureUnusedCapacity(gpa, b.top_level_steps.values().len);
                for (b.top_level_steps.values()) |top_level| {
                    var top_level_step_stack = std.ArrayListUnmanaged(*std.Build.Step){};
                    try top_level_step_stack.ensureTotalCapacity(gpa, top_level.step.dependencies.items.len);
                    top_level_step_stack_map.putAssumeCapacity(&top_level.step, top_level_step_stack);
                }

                var step_parents_map = std.AutoHashMapUnmanaged(*std.Build.Step, std.AutoArrayHashMapUnmanaged(*std.Build.Step, void)){};
                defer {
                    var step_parents_map_it = step_parents_map.valueIterator();
                    while (step_parents_map_it.next()) |*step_parents| {
                        step_parents.*.deinit(gpa);
                    }
                }
                defer step_parents_map.deinit(gpa);

                for (top_level_step_stack_map.keys()) |starting_step| {
                    checkForDependencyLoop(b, starting_step, starting_step, starting_step, &step_stack, &step_parents_map, &top_level_step_stack_map) catch |err| switch (err) {
                        error.DependencyLoopDetected => return error.UncleanExit,
                        else => |e| return e,
                    };
                }

                var running_step_id: usize = step_stack.entries.len;

                var step_visited_map = std.AutoHashMapUnmanaged(*std.Build.Step, void){};
                defer step_visited_map.deinit(gpa);

                var top_level_step_stack_it = top_level_step_stack_map.iterator();
                while (top_level_step_stack_it.next()) |top_level_step_stack_entry| {
                    const top_level_step = top_level_step_stack_entry.key_ptr.*;
                    const top_level_step_stack = top_level_step_stack_entry.value_ptr.*;
                    try graph_output.writer.print("    {}\n", .{Subgraph.Start{
                        .id = subgraph_counter,
                        .label = try std.fmt.allocPrint(gpa, "{s} (Top Level)", .{top_level_step.name}),
                    }});

                    for (top_level_step_stack.items) |step| {
                        const step_id = step_stack.getIndex(step).?;

                        const step_visited = try step_visited_map.getOrPut(gpa, step);
                        if (!step_visited.found_existing) {
                            const step_parents = step_parents_map.get(step).?;
                            if (step_parents.count() > 0) (for (step_parents.keys()) |parent_step| {
                                try graph_output.writer.print("    {}\n", .{Edge{
                                    .parent = parent_step,
                                    .parent_id = step_stack.getIndex(parent_step).?,
                                    .include_parent_description = false,
                                    .child = step,
                                    .child_id = step_id,
                                    .include_child_description = true,
                                    .graph_output = graph_output,
                                }});
                            }) else {
                                try graph_output.writer.print("    {}\n", .{Node{
                                    .item = step,
                                    .id = step_id,
                                    .include_description = true,
                                    .graph_output = graph_output,
                                }});
                            }
                            step_visited.value_ptr.* = {};
                        } else {
                            const alternate_step_id = running_step_id;
                            running_step_id += 1;
                            try graph_output.writer.print("    {}\n", .{Edge{
                                .parent = step,
                                .parent_id = alternate_step_id,
                                .include_parent_description = true,
                                .child = step,
                                .child_id = step_id,
                                .include_child_description = false,
                                .graph_output = graph_output,
                            }});
                        }
                    }

                    try graph_output.writer.print("    {}\n", .{Subgraph.End{}});

                    subgraph_counter += 1;
                }
            }
        };
    }
    pub const GraphOutputWriteFnInstruction = enum {
        /// Simply print out "Opaque Custom Step (step_name)"
        use_default_implementation,
    };
};

const step_inspection_functions = struct {
    pub fn annotateTopLevelStep(step: *std.Build.Step, writer: anytype) !void {
        // TODO: This is a hack around the fact that `std.Build.TopLevelStep` is a private decl.
        // Look into whether it could be made public upstream.
        const TopLevelStep = std.meta.FieldType(std.Build, .install_tls);
        if (step.cast(TopLevelStep)) |top_level| {
            try writer.print("\"{s} (Top Level)\\n{s}\"", .{ step.name, top_level.description });
        } else std.debug.panic("not a TopLevelStep", .{});
    }

    pub fn annotateCompileStep(step: *std.Build.Step, writer: anytype, options: struct {
        print_newlines: bool,
        print_in_between_quotes: bool,
    }) !void {
        const compile = @fieldParentPtr(std.Build.Step.Compile, "step", step);
        if (options.print_in_between_quotes) {
            try writer.writeByte('"');
        }

        try writer.print("{s} (Compile)", .{compile.name});

        if (options.print_newlines) {
            try writer.writeAll("\\n");
        } else {
            try writer.writeAll(", ");
        }

        try writer.print("kind: {s}", .{@tagName(compile.kind)});

        if (options.print_newlines) {
            try writer.writeAll("\\n");
        } else {
            try writer.writeAll(", ");
        }
        try writer.print("mode: {s}", .{@tagName(compile.optimize)});

        if (compile.linkage) |linkage| {
            if (options.print_newlines) {
                try writer.writeAll("\\n");
            } else {
                try writer.writeAll(", ");
            }
            try writer.print("linkage: {s}", .{@tagName(linkage)});
        }

        if (compile.root_src) |root_src| {
            if (options.print_newlines) {
                try writer.writeAll("\\n");
            } else {
                try writer.writeAll(", ");
            }
            try writer.print("root_src: '{}'", .{formatting.fmtLazyPath(compile.step.owner, root_src)});
        }

        if (compile.version) |version| {
            if (options.print_newlines) {
                try writer.writeAll("\\n");
            } else {
                try writer.writeAll(", ");
            }
            try writer.print("version: {}", .{version});
        }

        if (options.print_in_between_quotes) {
            try writer.writeByte('"');
        } else {
            _ = '{'; // lol vim
            try writer.writeByte('}');
        }
    }

    // TODO(haze): annotate envmap
    pub fn annotateRunStep(step: *std.Build.Step, writer: anytype) !void {
        const run = @fieldParentPtr(std.Build.Step.Run, "step", step);

        try writer.writeAll("\"Run\\nargv: [");
        for (run.argv.items, 0..) |arg, index| {
            try writer.print("{}", .{formatting.RunStepArg{ .arg = arg, .owner = run.step.owner }});
            if (index != run.argv.items.len - 1) {
                try writer.writeAll(", ");
            }
        }
        try writer.writeAll("]");

        if (run.cwd) |cwd| {
            try writer.print("\\ncwd: '{s}'", .{cwd});
        }

        switch (run.stdio) {
            .check => |checks| for (checks.items) |check| switch (check) {
                .expect_term => |expected_term| switch (expected_term) {
                    .Exited => |exit_code| try writer.print("\\nexpecting exit code: {}", .{exit_code}),
                    .Signal => |signal_code| try writer.print("\\nexpecting signal: {}", .{signal_code}),
                    .Stopped => |stop_code| try writer.print("\\nexpecting stop code: {}", .{stop_code}),
                    .Unknown => |unknown_code| try writer.print("\\nexpecting unknown code: {}", .{unknown_code}),
                },
                else => {},
            },
            else => {},
        }

        try writer.writeAll("\"");
    }

    pub fn annotateInstallArtifact(step: *std.Build.Step, writer: anytype) !void {
        const install_artifact = @fieldParentPtr(std.Build.Step.InstallArtifact, "step", step);
        const b = install_artifact.step.owner;

        if (install_artifact.dest_dir) |dest_dir| {
            try writer.print(
                "\"{s} (Install Artifact)\\ndestination: '{}'\"",
                .{ install_artifact.step.name, formatting.fmtInstallDir(b, dest_dir) },
            );
        } else {
            try writer.print(
                "\"{s} (Install Artifact)\\ndestination: null\"",
                .{install_artifact.step.name},
            );
        }

        if (install_artifact.pdb_dir) |pdb_directory| {
            try writer.print("\\nPDB directory: '{}'", .{formatting.fmtInstallDir(b, pdb_directory)});
        }

        if (install_artifact.pdb_dir) |header_directory| {
            try writer.print("\\nHeader directory: '{}'", .{formatting.fmtInstallDir(b, header_directory)});
        }
    }
};

const formatting = struct {
    pub const CompileStep = struct {
        compile: *std.Build.Step.Compile,

        pub fn format(
            formatter: CompileStep,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;
            try step_inspection_functions.annotateCompileStep(&formatter.compile.step, writer, .{
                .print_newlines = false,
                .print_in_between_quotes = false,
            });
        }
    };

    pub const JsonStringChars = struct {
        string: []const u8,
        options: std.json.StringifyOptions = .{},

        pub fn format(
            formatter: JsonStringChars,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;
            return std.json.encodeJsonStringChars(formatter.string, formatter.options, writer);
        }
    };
    pub fn fmtJsonStringChars(string: []const u8, options: std.json.StringifyOptions) JsonStringChars {
        return .{ .string = string, .options = options };
    }

    pub const JsonString = struct {
        string: []const u8,
        options: std.json.StringifyOptions = .{},

        pub fn format(
            formatter: JsonString,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;
            return std.json.encodeJsonString(formatter.string, formatter.options, writer);
        }
    };
    pub fn fmtJsonString(string: []const u8, options: std.json.StringifyOptions) JsonString {
        return .{ .string = string, .options = options };
    }

    pub const LazyPath = struct {
        lazy_path: std.Build.LazyPath,
        src_builder: *std.Build,

        pub fn format(
            formatter: LazyPath,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;
            const path = formatter.lazy_path.getPath2(formatter.src_builder, null);
            const relative_path = formatter.src_builder.build_root.join(formatter.src_builder.allocator, &.{path}) catch unreachable;
            defer formatter.src_builder.allocator.free(relative_path); // there's a chance this could get freed, with it (presumably) being the latest allocation.
            try writer.writeAll(relative_path);
        }
    };
    pub fn fmtLazyPath(src_builder: *std.Build, lazy_path: std.Build.LazyPath) LazyPath {
        return .{
            .src_builder = src_builder,
            .lazy_path = lazy_path,
        };
    }

    pub const InstallDir = struct {
        owner: *std.Build,
        install_dir: std.Build.InstallDir,

        pub fn format(
            formatter: InstallDir,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;

            const owner = formatter.owner;
            const install_path = owner.getInstallPath(formatter.install_dir, ".");

            const dst_rel_path = owner.build_root.join(owner.allocator, &.{install_path}) catch unreachable;
            defer owner.allocator.free(dst_rel_path); // there's a chance this could get freed, with it (presumably) being the latest allocation.

            try writer.writeAll(dst_rel_path);
        }
    };
    pub fn fmtInstallDir(b: *std.Build, install_dir: std.Build.InstallDir) InstallDir {
        return .{
            .owner = b,
            .install_dir = install_dir,
        };
    }

    pub const RunStepArg = struct {
        arg: std.Build.RunStep.Arg,
        owner: *std.Build,

        pub fn format(
            run_step_arg: RunStepArg,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = options;
            _ = fmt;
            switch (run_step_arg.arg) {
                .bytes => |bytes| try writer.print("&quot;{s}&quot;", .{bytes}),
                .lazy_path => |lazy_path| try writer.print("{s}{}", .{ lazy_path.prefix, fmtLazyPath(run_step_arg.owner, lazy_path.lazy_path) }),
                .directory_source => |directory_source| try writer.print("{s}{}", .{ directory_source.prefix, fmtLazyPath(run_step_arg.owner, directory_source.lazy_path) }),
                .artifact => |artifact| try writer.print("{}", .{formatting.CompileStep{ .compile = artifact }}),
                .output => |out| try writer.print("{s}", .{out.basename}),
            }
        }
    };
};

fn checkForDependencyLoop(
    b: *std.Build,
    s: *std.Build.Step,
    s_parent: *std.Build.Step,
    top_level_s: *std.Build.Step,
    step_stack: *std.AutoArrayHashMapUnmanaged(*std.Build.Step, void),
    step_parents_map: *std.AutoHashMapUnmanaged(*std.Build.Step, std.AutoArrayHashMapUnmanaged(*std.Build.Step, void)),
    top_level_step_stack_map: *std.AutoArrayHashMapUnmanaged(*std.Build.Step, std.ArrayListUnmanaged(*std.Build.Step)),
) !void {
    if (s != top_level_s and top_level_step_stack_map.contains(s)) return checkForDependencyLoop(b, s, s_parent, s, step_stack, step_parents_map, top_level_step_stack_map);
    var top_level_s_stack = top_level_step_stack_map.getPtr(top_level_s).?;
    try top_level_s_stack.append(b.allocator, s);
    if (step_stack.contains(s)) return;
    const s_parents_result = try step_parents_map.getOrPut(b.allocator, s);
    const s_parents = s_parents_result.value_ptr;
    if (!s_parents_result.found_existing) s_parents.* = .{};
    const s_parent_result = try s_parents.getOrPut(b.allocator, s_parent);
    if (s_parent_result.found_existing) {
        std.debug.print("dependency loop detected:\n  {s}\n", .{s.name});
        return error.DependencyLoopDetected;
    } else {
        s_parent_result.value_ptr.* = {};
    }

    const primary_top_level_s = if (top_level_step_stack_map.getPtr(s)) |s_stack| blk: {
        top_level_s_stack = s_stack;
        break :blk s;
    } else top_level_s;

    try step_stack.ensureUnusedCapacity(b.allocator, s.dependencies.items.len);
    for (s.dependencies.items) |dep| {
        checkForDependencyLoop(b, dep, s, primary_top_level_s, step_stack, step_parents_map, top_level_step_stack_map) catch |err| {
            if (err == error.DependencyLoopDetected) {
                std.debug.print("  {s}\n", .{s.name});
            }
            return err;
        };
    }

    if (s_parent == s) _ = s_parents.swapRemove(s);
    if (top_level_s != primary_top_level_s) try top_level_s_stack.append(b.allocator, s);
    try step_stack.put(b.allocator, s, {});
    return;
}

test {
    std.testing.refAllDecls(@This());
    const GraphOutputStepStdout = zigbo.Step.GraphOutput(@TypeOf(std.io.getStdOut().writer()));
    std.testing.refAllDecls(GraphOutputStepStdout);
}
