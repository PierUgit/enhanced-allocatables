# Enhanced Fortran allocatable arrays
Extending the allocatable arrays to be dynamically reallocatable/resizable

**FOR DEMONSTRATION PURPOSE ONLY**

Objectives:
- Following the C++ vector class principle, resize an already allocated array, keeping or not the existing content, without necessarily freeing/rellocating the memory
- modifying the bounds of an allocated array

It it was integrated to the langage, the proposal:
- would add a *capacity* specifier to the `allocatable` statement

`allocate( a([lb:]ub) [, capacity=c] )`
- would add a `resize` statement with the `keep`, `capacity`, `container`, `extend`, `mold`, `source` specifiers

`resize( a([lb]:[ub]) [, keep=k] [, capacity=c] [, container=con] [, extend=b], [, source=s] [, mold=m])`
- would add an `append` statement that would be syntatically equivalent to `a = [a, b]`

`append(a, b)`

In the demonstration code we have however to simulate averything with subroutines.

Limitations of the demonstration code:
- default real kind only
- rank-1 only at the moment
- size limited to default integer values
- size=0 not yet managed
- not thread-safe
- The reallocatable/resizable features are achieved through directly manipulating the C array descriptors in the framework of the C interoperability: this is non standard, and therefore not guaranteed to be portable ! 

## Specifications

- Similarly to C++ vectors, the enhanced Fortran allocatable arrays have a *capacity* property, corresponding to the actually allocated memory volume. The capacity is larger or equal to the size, and the difference `capacity - size` is the overprovisioning
- When resizing an array, no actual free/malloc is needed as long as the new size does not exceed the current capacity, and the existing content does not have to be copied if one wants to keep it
- An efficient allocation strategy aims at reducing the occurences of actual free/malloc
-- the capacity is (possibly iteratively) doubled if the new size exceeds the capacity
-- optionaly, the capacity is (possibly iteratively) halved if the new size gets below a percentage of the capacity
-- the compiler may cap the overprovisioning (possibly as a function of the available physical memory)

## Documentation

