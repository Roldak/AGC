# AGC ![example workflow](https://github.com/roldak/agc/actions/workflows/main.yml/badge.svg) 

AGC adds a garbage collector to your Ada programs.

## Usage

1. In order to benefit from garbage collection, the sources of your project must be instrumented by AGC as a pre-processing
   step before you invoke gprbuild to build your application. Assuming AGC's `bin/` directory containing the `agc` executable
   is included in your `$PATH`, you can run:
   ```
   agc -P <project_file.gpr> --output-dir <instr-dir> [--jobs|-j JOBS] [--optimize]
   ```
   * A new set of sources will be generated in the directory specified by `<instr-dir>`.
   * Using `-j0` allows AGC to treat several source files in parallel, which can heavily speed up this part.
   * AGC will try to generate optimized code when using `--optimize`, but will take more time.
   * AGC works incrementally by re-instrumenting only units which have been impacted by a change since the last run.
   
   Run `agc --help` to find out all the available options.
   
   *Note: if AGC is invoked on a project (through `-P`), the path given to `--output-dir` is treated as relative to the
   project's object directory, unless it is an absolute path.*
   
2. We now have to tell gprbuild to consider this set of files as a substitute for the original sources,
   but also to tell it that we depend on a new library, AGC's runtime, which implements the actual garbage
   collection routines that will run during your program's execution. First, make sure AGC's `lib/` directory
   containing the `agc_runtime.gpr` project file is included in your `$GPR_PROJECT_PATH`, and then:
   
   - The simplest way is to use gprbuild's newly added ``--src-subdirs`` and ``--implicit-with`` switches, which were
     in fact implemented to handle instrumenting tools (such as gnatcoverage). Building your application might then look like:
     ```
     gprbuild -P <project_file.gpr> --src-subdirs <instr-dir> --implicit-with=agc_runtime.gpr
     ```
     *Note: just like AGC's ``--output-dir`` switch, ``--src-subdirs`` takes a path relative to the project's object
     directory. This implies that you can use the exact same value for both.*
   
   - Another way is to create a copy of your existing project file and replace its `Source_Dirs` value so that it points
     to `<instr-dir>` instead. You should also add a top level `with "agc_runtime.gpr"` clause.
     This approach is less convenient but more flexible, as it allows you to configure a totally different build process
     when targetting AGC.
   
3. That's it! The built binary will behave as your original program, but will benefit from garbage collection!

*Warning: this is still in prototype phase. A lot of features are missing and a lot of Ada constructs are not supported yet.
Check out the tests and benchmarks to have an idea of what is currently supported.*

## Configuration

It is possible to configure the runtime behavior of AGC by choosing which storage pool it will use internally. This can be done either at compile-time by passing the scenario variable `-XAGC_POOL=<POOL>`, or dynamically by first compiling with `-XAGC_POOL=dynamic` and then running your final executable with the environment variable `AGC_POOL` set to the desired pool identifier. Possible values for `AGC_POOL` are:
* `malloc_free`: The storage pool is managed by the system using malloc/free.
* `free_list`: The storage pool is managed by AGC which allocates a big chunk of memory and manages all allocations using a free-list based mechanism.

## Performance

Although the main goal of AGC is to alleviate Ada programmers from memory management, it's very important to keep performance of resulting binaries reasonable. To keep track of this work, a set of benchmarks will be maintained in the `benchmarks` directory (there are only two of them for now).

To have an idea of the performance, single-run results for the `binary_tree` benchmark on my machine give:

* **Raw** (no deallocation):                           `0,18s user 0,09s system 98% cpu 0,269 total`
* **Manual** (user-managed):                           `0,22s user 0,00s system 99% cpu 0,227 total`
* **Controlled** (ref-counted using controlled types): `1,04s user 0,00s system 99% cpu 1,044 total`
* **AGC** with `AGC_POOL=MALLOC_FREE`:                 `0,21s user 0,01s system 94% cpu 0,235 total`
* **AGC** with `AGC_POOL=FREE_LIST`:                   `0,15s user 0,00s system 99% cpu 0,158 total`

As you can see the AGC version that uses its own free-list based storage pool performs approximately 30% better than the version with manually managed memory, while the malloc/free-based storage pool yields roughly equivalent results. In both cases, performance is much better (more than 6x faster) than a controlled-types based implementation, without having to write any memory-management code.

## Setup

**Build requirements**:
 - A relatively recent GNAT
 - Libadalang from [here](https://github.com/Roldak/libadalang/tree/topic/env_rework_agc_2),
   generated from [this](https://github.com/Roldak/langkit/tree/topic/env_rework_agc_2) Langkit.

Run `gprbuild agc.gpr -p -j0` to produce AGC's frontend binary in the `bin/` directory.

For **x86_64-linux** machines, the [`Release`](https://github.com/Roldak/AGC/actions/workflows/release.yml) github workflow produces an `AGC` binary artifact, which you can download directly from github. Simply follow the link, click on the most recent run and scroll all the way down to the *Artifacts* section.

**Test requirements**:
 - Python3
 - AGC's frontend available in the `$PATH`
 - AGC's runtime project file available in `$GPR_PROJECT_PATH` (simply add the `lib/` directory)
 
Run `python3 testsuite/run_testsuite.py`.

## Internals

The implementation is similar to any garbage collector:
1. Keep track of reachable memory locations
2. Keep track of heap allocated locations
3. Free heap allocated locations when they are not reachable

Unfortunately, 1. and 2. are not easily extractable from arbitrary Ada programs. Therefore, AGC chooses not to work with the original Ada code, but transforms it into a representation that allows it perform those two actions. AGC's frontend first transforms a source file of the following form:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer_Access is access all Integer;

   X : Integer_Access := null;
begin
   for I in 1 .. 10_000 loop
      X := new Integer'(42);
   end loop;
end Test;
```

Into this:

```ada
with AGC;
with AGC.Storage;
with System;
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   pragma Default_Storage_Pool (AGC.Storage.Pool);
   AGC_Base_Root_Count : Natural := AGC.Root_Count;
   
   type Integer_Access is access all Integer;
   procedure AGC_Visit_Test_Integer_Access is new AGC.Visit_Access_Type
     (Integer, Integer_Access, AGC.No_Op);
     
   X : aliased Integer_Access := null;
begin
   AGC.Push_Root (X'Address, AGC_Visit_Test_Integer_Access'Address);
   for I in 1 .. 10_000 loop
      X := new Integer'(42);
   end loop;
   AGC.Pop_Roots (AGC_Base_Root_Count);
end Test;

```

As you can see, roots are explicited to the garbage collector through calls to `AGC.Push_Root`. The example above shows it working for stack-allocated variables, but this must also include global variables. Note that all stack-allocated variables are now marked `aliased`: this is because we are taking their address which in Ada is only necessarily meaningful for aliased objects (see RM 13.3.16).

In some cases, AGC must perform heavy manipulations to be able to track all reachable locations. In particular, temporary results such as values returned from function calls must be registered in the garbage collector although they are not reachable by a source code variable. This is done by reshaping the code to store them explicitly in temporary variables. For example:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer_Access is access all Integer;

   function Incr (X : Integer_Access) return Integer_Access is
   begin
     return new Integer'(X.all + 1);
   end Incr;
begin
   Put_Line (Incr (new Integer'(1)).all'Image);
end Test;
```

Is expanded to:

```ada
with AGC;
with AGC.Storage;
with System;
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   pragma Default_Storage_Pool (AGC.Storage.Pool);
   AGC_Base_Root_Count : Natural := AGC.Root_Count;
   
   type Integer_Access is access all Integer;
   procedure AGC_Visit_Test_Integer_Access is new AGC.Visit_Access_Type
     (Integer, Integer_Access, AGC.No_Op);
     
   function Incr (X : Integer_Access) return Integer_Access is
   begin
      return new Integer'(X.all + 1);
   end Incr;
begin
   declare
      AGC_Temp_1 : aliased Test.Integer_Access := new Integer'(1);
   begin
      AGC.Push_Root (AGC_Temp_1'Address, AGC_Visit_Test_Integer_Access'Address);
      declare
         AGC_Temp_0 : aliased Test.Integer_Access := Incr (AGC_Temp_1);
      begin
         AGC.Push_Root
           (AGC_Temp_0'Address, AGC_Visit_Test_Integer_Access'Address);
         Put_Line (AGC_Temp_0.all'Image);
      end;
   end;
   AGC.Pop_Roots (AGC_Base_Root_Count);
end Test;
```

To see why this is necessary, assume for a moment that the GC was running on the original program. If we suppose that the allocation inside `Incr` triggers a garbage collection, then the temporary value `new Integer'(1)` allocated in `Test`'s body would be collected because it's not reachable by any root, neither directly nor indirectly. Thanks to the transformation, a temporary variable `AGC_Temp_1` is generated and holds a reference to it while `Incr` is being called.

Additionally, you may have observed the presence of `AGC_Visit_[...]` procedures in the generated code. Those are automatically derived for types that the GC must be aware of, which are basically all types that are either access types or than contain access types directly or indirectly. These are used by AGC's runtime to browse the entire space of reachable locations starting from the program's roots whenever a collection is triggered. This allows keeping alive all memory that is reachable by a root of the program and freeing the rest of it.
