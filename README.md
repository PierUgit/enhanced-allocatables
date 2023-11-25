# Enhanced Fortran allocatable arrays
Extending the allocatable arrays to be dynamically reallocatable/resizable

**FOR DEMONSTRATION PURPOSE ONLY**

Objectives:
- Following the C++ vector class principle, resize an already allocated array, keeping or not the existing content, without necessarily freeing/rellocating the memory
- modifying the bounds of an allocated array

It it was integrated to the langage, the proposal would add:
- a *capacity* specifier to the `allocatable` statement

`allocate( array([lb:]ub) [,capacity=c] )`
- a `resize` statement with the `keep`, `capacity`, `container`, `extend`, `mold`, `source` specifiers

`resize( array([lb]:[ub]) [,keep=k] [,capacity=c] [,container=con] [,extend=e] [,drop=d] [,mold=m] [,source=s])`

- a `capa( a )` integer function to inquire the capacity of an array

In the demonstration code we have however to simulate averything with subroutines.

Limitations of the demonstration code:
- default real kind only
- rank-1 only at the moment
- size limited to default integer values
- not thread-safe
- The reallocatable/resizable features are achieved by directly manipulating the C array descriptors in the framework of the C interoperability: this is non standard, and therefore not guaranteed to be portable ! 

TESTED SUCCESSFULLY WITH:
- gfortran/g++ 13 on macOS 10.13
- gfortran/g++ 10 on Linux db11

**DOES NOT WORK WITH:**
- ifort/icpc 21 on Linux db11

## compilation

```
gfortran -c -O3 enhanced_allocatables.f90 eatest.f90                            && \
g++ -O3 -lgfortran enhanced_allocatables.o eatest.o enhanced_allocatables_c.cpp
```

## Specifications

- Similarly to C++ vectors, the enhanced Fortran allocatable arrays have a *capacity* property, corresponding to the actually allocated memory volume. The capacity is larger or equal to the size, and the difference `capacity - size` is the overprovisioning
- When resizing an array, no actual free/malloc is needed as long as the new size does not exceed the current capacity, and the existing content does not have to be copied if one wants to keep it
- An efficient allocation strategy aims at reducing the occurences of actual free/malloc
-- the capacity is (possibly iteratively) doubled if the new size exceeds the capacity
-- optionaly, the capacity is (possibly iteratively) halved if the new size gets below a percentage of the capacity
-- the compiler may cap the overprovisioning (possibly as a function of the available physical memory)
- the user can overide the default strategy by using the `capacity` specifier (this is an important point)

## Documentation

### eallocate()
`call eallocate(array , lb , ub [,capacity=c] )`

Wrapper to the standard `allocate` statement. The capacity can be specified, otherwise it is set to the requested size. 

### resize
`call resize( array [,lb=l] [,ub=u] [,keep=k] [,capacity=c |,container=con] [,extend=e | ,drop=d] [,mold=m | ,source=s] )`

`lb=l` , `ub=u` 
- `l` and `u` are the new lower and upper bounds.
- if none is coded, either `capacity=c` or `container='fit'` must be coded
- if only one is coded, the bounds are updated and the size does not change
- if both are coded the size is potentially modified, and so is the capacity

`keep=k`
- `k` is a logical scalar that is `.false.` by default. 
- If `.true.`, the content of the array is kept whenever the array has te be reallocated under the hood

`capacity=c`
- `c` is an integer scalar, used to to force the number of elements actually allocated under the hood

`container=con`
- `con` is a character(*) scalar:
  - `'grow'`: the capacity can only increase
  - `'any'`: the capacity can increase or decrease
  - `'fit'`: the capacity is set to the size
- cannot be coded together with `capacity=`

`extend=e`
- `e` is a rank-1 array that is appended to `array`
- the size of `array` is increased accordindly
- `lb=` and `ub=` must not be coded together in this case
- cannot be coded together with `drop=`

`drop=d`
- `d` is an integer scaler
- drops the `d` last elements of `array`
- `lb=` and `ub=` must not be coded together in this case
- cannot be coded together with `extend=`

`mold=m`
- works exactly like in the current `allocate` statement
- `m` is a rank 1 array that determines the new lower and upper bounds of `array`
- `lb=`, `ub=`, `extend=`, and `drop=` must not be coded in this case 

`source=s`
- works *almost* exactly like in the current `allocate` statement
- if `s` is a scalar
  - `s` is used to fill the entire resized array if keep=.false.
  - `s` is used to fill the new part of the resized array if enlarged if `keep=.true`
- if `s` is a rank 1 array
  - `s` determines the new lower and upper bounds of `array`
  - the content of `s` is copied to `array`
  - `keep=` must not be coded in this case

### edeallocate()
`call edeallocate(array)`

Wrapper to the standard `deallocate` statement. Is needed to update the hidden capacity table.

