PROGRAM enhanced_allocatables_p
USE enhanced_allocatables_m
implicit none

real, allocatable :: a(:), b(:)
integer :: i, cap, newcap

! Testing C descriptors for different cases of allocatable arrays
! unallocated
call print_info(a)
! allocated size=0
a = [real:: ]; call print_info(a)
! allocated size>0
a = [1.0]; call print_info(a)
deallocate( a )
! With gfortran 13 at least, something is allocated even for a size=0


! allocates with size=10 & capacity=15
call eallocate(a,lb=1,ub=10,capacity=15)
a(:) = [(real(i),i=1,size(a))]
print*, lbound(a), size(a), capa(a), a(2)

! modify the bounds
call resize(a,lb=-2,keep=.true.)
print*, lbound(a), size(a), capa(a), a(2)

! modify the size
call resize(a,lb=-2,ub=15,keep=.true.)
print*, lbound(a), size(a), capa(a), a(2)

! append elements
call resize(a,keep=.true.,extend=[100.0, 200.0, 300.0])
print*, lbound(a), size(a), capa(a), a(ubound(a,1))

! reset the capacity to fit the size
call resize(a,keep=.true.,container='fit')
print*, lbound(a), size(a), capa(a), a(ubound(a,1))

call edeallocate(a)


! starts with size=0
! iteratively append 1 element, then iteratively drop 1 element
call eallocate(a,lb=1,ub=0)
cap = capa(a)
do i = 1, 100000
   call resize(a,keep=.true.,extend=[real(i)])
   newcap = capa(a)
   if (newcap /= cap) then
      print*, "size = ", size(a), "   capacity changed from", cap, " to:", newcap
      cap = newcap
   end if
end do
do i = 1, 100000
   call resize(a,keep=.true.,drop=1,container='any')
   newcap = capa(a)
   if (newcap /= cap) then
      print*, "size = ", size(a), "   capacity changed from", cap, " to:", newcap
      cap = newcap
   end if
end do
call edeallocate(a)

! start with a single element
! iteratively append doubles size by self-appending the array, 
! then iteratively drop half of the elements
call eallocate(a,lb=0,ub=0)
a(0) = 42.0
do i = 1, 11
   call resize(a,keep=.true.,extend=[a,a])
   newcap = capa(a)
   if (newcap /= cap) then
      print*, "size = ", size(a), "   capacity changed from", cap, " to:", newcap
      cap = newcap
   end if
end do
do i = 1, 11
   call resize(a,keep=.true.,drop=2*size(a)/3,container='any')
   newcap = capa(a)
   if (newcap /= cap) then
      print*, "size = ", size(a), "   capacity changed from", cap, " to:", newcap
      cap = newcap
   end if
end do

END