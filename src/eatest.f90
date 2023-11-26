PROGRAM enhanced_allocatables_p
USE iso_fortran_env
USE enhanced_allocatables_m
implicit none

real, allocatable :: a(:), b(:)
integer :: i, cap, newcap
integer(int64) :: location, newlocation
integer(int64) :: tic, toc
real :: rate

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

print*
print*, "====== WITH A NORMAL ALLOCATABLE"
print*, "starts with size=0"
print*, "iteratively append 1 element, then iteratively drop 1 element"
call system_clock(tic,rate)
allocate(a(0))
location = loc(a)
do i = 1, 100000
   a = [a,real(i)]
   newlocation = loc(a)
   if (newlocation /= location) then
      print*, "size = ", size(a), "   location changed from", location, " to:", newlocation
      location = newlocation
   end if
end do
do i = 1, 100000
   a = a(1:size(a)-1)
   newlocation = loc(a)
   if (newlocation /= location) then
      print*, "size = ", size(a), "   location changed from", location, " to:", newlocation
      location = newlocation
   end if
end do
deallocate(a)
call system_clock(toc,rate)
print*, "Elapsed time =", (toc-tic)/rate

print*
print*, "====== WITH A ENHANCED ALLOCATABLE"
print*, "starts with size=0"
print*, "iteratively append 1 element, then iteratively drop 1 element"
call system_clock(tic,rate)
call eallocate(a,lb=1,ub=0)
cap = capa(a)
do i = 1, 100000
   call resize(a,extend=real(i))
   newcap = capa(a)
   if (newcap /= cap) then
      print*, "size = ", size(a), "   capacity changed from", cap, " to:", newcap
      cap = newcap
   end if
end do
do i = 1, 100000
   call resize(a,drop=1,container='any')
   newcap = capa(a)
   if (newcap /= cap) then
      print*, "size = ", size(a), "   capacity changed from", cap, " to:", newcap
      cap = newcap
   end if
end do
call edeallocate(a)
call system_clock(toc,rate)
print*, "Elapsed time =", (toc-tic)/rate

print*
print*, "====== WITH A ENHANCED ALLOCATABLE"
print*, "starts with size=1"
print*, "iteratively triples the size"
print*, "then iteratively drop 2/3 of the elements"
call eallocate(a,lb=0,ub=0)
a(0) = 42.0
do i = 1, 11
   call resize(a,lb=1,ub=3*size(a),keep=.true.)
   a(size(a)/3+1: ) = 42.0
   newcap = capa(a)
   if (newcap /= cap) then
      print*, "size = ", size(a), "   capacity changed from", cap, " to:", newcap
      cap = newcap
   end if
end do
do i = 1, 11
   call resize(a,drop=2*size(a)/3,container='any')
   newcap = capa(a)
   if (newcap /= cap) then
      print*, "size = ", size(a), "   capacity changed from", cap, " to:", newcap
      cap = newcap
   end if
end do

END