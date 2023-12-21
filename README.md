# Enhanced Fortran allocatable arrays
Extending the allocatable arrays to be dynamically reallocatable/resizable

## Proposal

Objectives:
- Following (and somehow extending) the C++ vector class principle, resize an already allocated array, keeping or not the existing content, without necessarily freeing/rellocating the memory
- dynamically modifying the bounds of an already allocated array

### Specifications

- Similarly to C++ vectors, the enhanced Fortran allocatable arrays have a *capacity* property, corresponding to the actually allocated memory volume. The capacity is larger or equal to the size, and the difference `capacity - size` is the overprovisioning
- When resizing an array, no actual free/malloc is needed as long as the new size does not exceed the current capacity, and the existing content does not have to be copied if one wants to keep it
- An efficient allocation strategy aims at reducing the occurences of actual free/malloc
  - the capacity is (possibly iteratively) doubled if the new size exceeds the capacity
  - optionaly, the capacity is (possibly iteratively) halved if the new size gets below a percentage of the capacity (33% in the demonstration code).
  - the compiler may cap the overprovisioning (possibly as a function of the available physical memory)
- the user can overide the default strategy by using the `capacity` specifier (**this is an important point**)
- the compiler is free to internally adjust the capacity to always be a multiple of some bytes (16 bytes in the demonstration code)

### Statements

It it was integrated to the langage, the proposal would add:

- a `resize` statement with the `keep`, `capacity`, `container`, `extend`, `drop`, `mold`, and `source` specifiers  
`resize( array[(array-bounds-list)] [,mold=m] [,source=s] [,keep=k] [,capacity=c] [,container=con] [,extend=e] [,drop=d] )`
  - bounds:
    - `array` or `array(:)` : the lower bound is unmodified; the upper bound is modified if the size is modified by the other specifiers
    - `array(lb: )` : specifying a new lower bound; the upper bound is modified accordingly (also as a function of the new size depending on the other specifiers)
    - `array( :ub)` : specifying a new upper bound; the lower bound is modified accordingly (also as a function of the new size depending on the other specifiers)
    - `array([lb:]ub)` : specifying a new size; the new lower bound is 1 if unspecified 
    - all the above can be combined for the different dimension, e.g. `array(5: ,:,100)` 
  - Please refer to the demonstration code documentation below to get a full description of the specifiers.
  - instead of a new `resize` statement, all these new features could alternatively be included in the existing `allocate` statement

- a `capacity( array )` integer function to inquire the capacity of an array

## Demonstration code

**FOR DEMONSTRATION PURPOSE ONLY**

In the demonstration code we have to simulate the new statement with subroutines.

Limitations:
- default real kind only (the generalization to any kind is trivial)
- rank-1 and rank-2 only (the generalization to any rank is trivial)
- size and capacity limited to default integer values
- `mold=` and `source=` limited to allocatable argument, as this is the only way to get lower bounds /= 1
- not thread-safe
- The reallocatable/resizable features are achieved by directly manipulating the C array descriptors in the framework of the C interoperability: **this is a violation of the API, and therefore not guaranteed to be portable !** 

TESTED SUCCESSFULLY WITH:
- gfortran/g++ 13 on macOS 10.13
- gfortran/g++ 10 on Linux db11
- ifort/icpc 21 on Linux db11

**DOES NOT WORK WITH:**
- ...

### compilation

```
gfortran -c -O3 enhanced_allocatables.F90 eatest.f90                            && \
g++ -O3 -lgfortran enhanced_allocatables.o eatest.o enhanced_allocatables_c.cpp && \
a.out
```

### resize
`call resize( array [,lb=l] [,ub=u] [,keep=k] [,capacity=c |,container=con] [,extend=e | ,drop=d] [,mold=m | ,source=s] )`

`call resize( array [,lb1=l1] [,ub1=u1] [,lb2=l2] [,ub2=u2] [,keep=k] [,capacity=c |,container=con] [,extend=e | ,drop=d] [,mold=m | ,source=s] )` 

`array`
- a rank-1 or rank-2 `REAL` array
- can be already allocated or not on input
- if the array has been previously allocated with the standard `allocate` statement, the first call to `resize` afterwards will always generate a free/malloc behavior even if not necessary. 
  - This a limitation of the demonstration code because there's no way to know the capacity of an array that is allocate with `allocate`.
  - a standard implementation of the proposal would not have this limitation
  - the recommandation with the demonstration code is to use `resize` even for the initial allocation

`lb=l` , `ub=u` 
- `l` and `u` are the new lower and upper bounds.
- if none is coded, either `capacity=c` or `container='fit'` must be coded
- if only one is coded, the bounds are updated and the size does not change
- if both are coded the size is potentially modified, and so is the capacity

`keep=k`
- `k` is a logical scalar that is `.true` by default if `extend=` or `drop=` are coded, and `.false.` otherwise 
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
- `e` is appended to `array`
  - if `array` is rank-1, `e` is a scalar or a rank-1 array
  - if `array` is rank-2, `e` is a rank-1 or rank-2 array
    - `size(e,1)` must be equal to `size(array,1)`
- the size of `array` is increased accordindly
  - if `array` is rank-1, its size is increasing by `1` or by `size(e)`
  - if `array` is rank-2, `size(array,2)` is increasing by `1` or by `size(e,2)`
- `lb=` and `ub=` must not be coded together in this case
- cannot be coded together with `drop=`
- implies `keep=.true.`

`drop=d`
- `d` is an integer scalar
  - drops the `d` last elements of `array` if `array` is rank-1
  - drops the `d` last columns of `array` if `array` is rank-2
- the size of `array` is decreased accordindly
- `lb=` and `ub=` must not be coded together in this case
- cannot be coded together with `extend=`
- implies `keep=.true.`

`mold=m`
- works exactly like in the standard `allocate` statement
- `lb=`, `ub=`, `extend=`, and `drop=` must not be coded with `mold=`

`source=s`
- works *almost* exactly like in the standard `allocate` statement, but:
  - if `s` is a scalar and `keep=.true`, then `s` is used to fill only the new part of the resized array if it is enlarged
- `extend=`, and `drop=` must not be coded with `source=`

### edeallocate()
`call edeallocate(array)`

Replaces the standard `deallocate` statement. Is needed to update the hidden capacity table, but would not be needed with a formal integration to the standard.

### capa()
`c = capa(array)`

- returns the current capacity of `array`
- returns -1 if `array` has been allocated with the standard `allocate()` and `resize()` has not been called at least once on it. This is a limitation of the demonstration code, in a standard implementation any array would have a defined capacity.
- would be named `capacity()` in a standard implementation, but we had to avoid a name collision with the `capacity=` argument in `resize()`

