# Test Suite

## Outline

The Test Suite is currently broken down into files by Operation type, i.e. Logical Operations, Store Operations, Stack Operations, etc. are all grouped together.
Since there is no way to read 6502 assembly source code, much of the testing is done using Utility functions derived in `TestUtils.hs`. 
Testing (in most cases) leverages the logging capabilities of the Emulator system, this helps enable debugging through any failed tests.

Each operation is tested based on all possible addressing modes. As of now each addressing mode is usually only tested once. 
Since most operations have more than one addressing mode, we can provide different "corner case" values to test on for each mode.
However, that being said, there has been some testing with binary operations (shifting  to be exact) using QuickCheck to test the entire range of 8-bit numbers. 
Unfortunately, there is no good way of testing the Flags in these types of operations, as at that point we'd be re-implementing the flag logic found in the actual code we are testing.
Also, for many operations there is no need to test the full 8-bit range (store operations or increment/decrement).

There are plans to overhaul the testing system to use an eventual "assembler" to construct the base CPU. 
For example, as it stands now there are numerous helper functions that combine to create a test machine that does what we need it to do.
There are many ways this can fail and cause headaches (copying the wrong Op Code from test to test for example).

## Zero Page Testing

Of note, the zero page for the emulator is the only portion of the machine that contains actual information during construction. Essentially, for every address from `0x0` to `0xFF`, there is value equivalent to `0xFF - address`. For example, at address `0xF`, the value `0xF0`can be found. Almost all other memory locations are filled with `0x0` and as a result, there are certain tests where we refrain from using the number `0x0` as an operand. Again, an example would be loading a byte from a memory address. If it were `0x0` to begin with and we expect `0x0`, we may not know if that's because something went wrong or it's actually correct.
