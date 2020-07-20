# AGC
AGC adds a garbage collector to your Ada program.

## Usage

1. The sources of your project must be processed by AGC as a first step before
   you invoke gprbuild. Run
   ```
   agc -P <project_file.gpr> <new-project-dir>
   ```
   This will generate a new set of sources in the directory specified by `<new-project-dir>`.
   
2. Then, you must write a new project file that can compile the set of generated sources.
   This project file will be similar to that of your original project, but must include the AGC's runtime:
   ```
   with "AGC/lib/agc_runtime.gpr";
   ```
   
3. You can now invoke `gprbuild` on this new project file.

## Internals

AGC's frontend first transforms a source file of the following form:

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

into this:

```ada
with GC;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer_Access is access all Integer;
   
   X : aliased Integer_Access := null;
begin
   GC.Push_Reachable (X'Address);
   for I in 1 .. 10_000 loop
      X := GC.Register (0, new Integer'(42));
      GC.Untemp (0);
   end loop;
   GC.Pop_Reachable (1);
end Test;
```

At runtime, calls made to the `GC` package keep track of reachable addresses and memory allocated on the heap and is implemented like a classical mark & sweep GC.

_more to come_
