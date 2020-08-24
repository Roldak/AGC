# AGC
AGC adds a garbage collector to your Ada programs.

## Usage

1. The sources of your project must be processed by AGC as a first step before
   you invoke gprbuild. Run
   ```
   agc -P <project_file.gpr> --output-dir <new-project-dir> [--jobs|-j JOBS] [--optimize]
   ```
   * A new set of sources will be generated in the directory specified by `<new-project-dir>`.
   * Using `-j0` allows AGC to treat several source files in parallel, which can heavily speed up this part.
   * AGC will try to generate optimized code when using `--optimize`, but will take more time.
   
   Run `agc --help` to find out all the available options.
   
2. Then, you must write a new project file that can compile the set of generated sources.
   Typically (but not necessarily), this project file will be similar to that of your original project,
   but must include the AGC's runtime:
   ```
   with "<path-to-AGC>/lib/agc_runtime.gpr";
   ```
   Don't forget to rectify the relative paths appearing in the project file.
   
3. You can now invoke `gprbuild` on this new project file. The built binary will behave as your original program, but will benefit from garbage collection!

Note that it is possible to configure the runtime behavior of AGC. For now, this is done using environment variables and the only configurable part is the kind of storage pool to use: simply set the `AGC_POOL` variable to either:
* `MALLOC_FREE`: The storage pool is managed by the system using malloc/free.
* `FREE_LIST`: The storage pool is managed by AGC which allocates a big chunk of memory and manages all allocations using a free-list based mechanism.

*Warning: this is still in prototype phase. A lot of features are missing and a lot of Ada constructs are not supported yet. Check out the tests and benchmarks to have an idea of what is currently supported.*

## Performance

Although the main goal of AGC is to alleviate Ada programmers from memory management, it's very important to keep performance of resulting binaries reasonable. To keep track of this work, a set of benchmarks will be maintained in the `benchmarks` directory (there are only two of them for now).

To have an idea of the performance, single-run results for the `binary_tree` benchmark on my machine give:

* **Raw** (no deallocation):                              `0,18s user 0,09s system 98% cpu 0,269 total`
* **Manual** (user-managed):                              `0,23s user 0,00s system 99% cpu 0,230 total`
* **Controlled** (ref-counted using controlled types):    `1,04s user 0,00s system 99% cpu 1,044 total`
* **AGC** with `AGC_POOL=MALLOC_FREE`:                    `0,30s user 0,00s system 96% cpu 0,318 total`
* **AGC** with `AGC_POOL=FREE_LIST`:                      `0,21s user 0,01s system 94% cpu 0,235 total`

As you can see AGC's performance using its own free-list based storage pool gives results on-par with manually managed memory, and even the malloc/free-based storage pool yields better performance (more than 3x faster) than a controlled-types based implementation, without having to write any additional memory-management code.

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

As you can see, roots are explicited to the garbage collector through calls to `AGC.Push_Root`. The example above shows it working for stack-allocated variables, but this must also include global variables (_not yet implemented_). Note that all stack-allocated variables are now marked `aliased`: this is because we are taking their address which in Ada is only necessarily meaningful for aliased objects (see RM 13.3.16).

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
