# Verimacro

A small macro expander written in haskell. 
Translates a file into a corresponding .v file of the same name.
(e.g. "control.abcde" -> "control.v")

CURRENTLY SUPPORTED MACROS:
```
mod {} => module; endmodule
case {} => case; endcase
comb {} => always_comb begin end
if () {} => if () begin end
i => input
o => output
l => logic
r => reg
-> => assign
ff () {} => always_ff @() {}
def>> => default: 
[x] => [x:0]
```

To use, install the binary `build/vmacro` to somewhere on your path, like `/usr/bin`. Then, run `vmacro /path/to/macro/file.vm`.
