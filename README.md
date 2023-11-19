# Enhanced Fortran allocatable arrays
Extending the allocatable arrays to be dynamically reallocatable/resizable

**FOR DEMONSTRATION PURPOSE ONLY**

Objectives:
- Following the C++ vector class principle, resize an already allocated array, keeping or not the existing content, without necessarily freeing/rellocating the memory
- modifying the bounds of an allocated array

It it was integrated to the langage, the proposal would add:
- a *capacity* specifier to the `allocatable` statement

`allocate( array([lb:]ub) [, capacity=c] )`
- a `resize` statement with the `keep`, `capacity`, `container`, `extend`, `mold`, `source` specifiers

`resize( array([lb]:[ub]) [, keep=k] [, capacity=c] [, container=con] [, extend=b], [, source=s] [, mold=m])`

- a `capa( a )` integer function to inquire the capacity of an array

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
- the user can overide the default strategy by using the `capacity` specifier

## Documentation

### `call eallocate(array , lb , ub [,capacity=c] )`

Wrapper to the standard `allocate` statement. The capacity can be set, otherwise it is set the requested size. 

### `call resize( array [, lb=l] [, ub=u] [,keep=k] [,capacity=c | ,container=con] [,extend=e | ,drop=d] )

- `l` and `u` are the new lower and upper bounds.
-- if none is coded, either `capacity=c` or `container='fit'` must be coded
-- if only one is coded the bounds are updated and the size does not change
-- if both are coded the size is potentially modified, and so is the capacity
- `k` is a logical scalar that is `.false.` by default. If `.true.`, the content of the array is kept whenever the array has te be reallocated under the hood
- `c` is an integer scalar, used to to force the number of elements actually allocated under the hood
- `con` is a character(*) scalar.
-- `'grow'`: the capacity can only increase
-- `'any'`: the capacity can increase or decrease
-- `'fit'`: the capacity is set to the size
- `e` is a rank-1 array that is appended to `array`
-- the size of `array` is increased accordindly
-- `lb`and `ub` must not be coded together in this case
- `d` is an integer that tells to drop the `k` last elements of `array`
-- `lb`and `ub` must not be coded together in this case

### `call edeallocate(array)`

Wrapper to the standard `deallocate` statement. 

## ToDo

- manage the size=0 case
- refine some default values (keep=.true. should be implied with extend= and drop=, and container='any' may be implied with drop=)
- add more tests to for arguments checking
- write the routines for the 2D case (rank=2 or more arrays have some specificities compared to rank=1)
