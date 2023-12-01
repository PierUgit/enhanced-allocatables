PROGRAM enhanced_allocatables_p
USE iso_fortran_env
USE enhanced_allocatables_m
implicit none

real, allocatable :: ZERO, S
real, allocatable :: a(:), aa(:,:), bb(:,:)
real :: x
integer :: i, k, cap, newcap, count
integer(int64) :: location, newlocation
integer(int64) :: tic, toc
real :: rate

allocate( ZERO, S)
ZERO = 0.0

print*, "Testing C descriptors for different cases of allocatable arrays"
print*, "unallocated"
call print_info(a)
print*, "unallocated size=0"
a = [real:: ]; call print_info(a)
print*, "unallocated size=1"
a = [1.0]; call print_info(a)
deallocate( a )
! With gfortran 13 at least, something is allocated even for a size=0

print*
print*, "Testing enhanced allocables"
print*, "allocates with size=10 & capacity=15"
call resize(a,lb=1,ub=10,capacity=15)
a(:) = [(real(i),i=1,size(a))]
print*, lbound(a), size(a), capa(a), a(2)
print*, "Setting the lower bound to -2"
call resize(a,lb=-2,keep=.true.)
print*, lbound(a), size(a), capa(a), a(-1)
print*, "increase the size to 18"
call resize(a,lb=-2,ub=15,keep=.true.,source=ZERO)
print*, lbound(a), size(a), capa(a), a(-1)
print*, "append 3 elements 100 200 300"
call resize(a,keep=.true.,extend=[100.0, 200.0, 300.0])
print*, lbound(a), size(a), capa(a), a(-1), a(ubound(a,1))
print*, "reset the capacity to fit the size"
call resize(a,keep=.true.,container='fit')
print*, lbound(a), size(a), capa(a), a(-1), a(ubound(a,1))
call edeallocate(a)

print*
print*, "====== WITH A NORMAL ALLOCATABLE"
print*, "starts with size=0"
print*, "iteratively append 1 element 100000 times times"
call system_clock(tic,rate)
allocate(a(0))
location = loc(a)
count = 0
do i = 1, 100000
   a = [a,real(i)]
   newlocation = loc(a)
   if (newlocation /= location) then
      count = count+1 ! then
   !   print*, "size = ", size(a), "   location changed from", location, " to:", newlocation
   end if
   location = newlocation
end do
print*, "At least",count," reallocations during growing"
call system_clock(toc,rate)
print*, "Elapsed time =", (toc-tic)/rate
print*, "iteratively drop 1 element 100000 times"
call system_clock(tic,rate)
count = 0
do i = 1, 100000
   a = a(1:size(a)-1)
   newlocation = loc(a)
   if (newlocation /= location) then
      count = count+1
   !   print*, "size = ", size(a), "   location changed from", location, " to:", newlocation
   end if
   location = newlocation
end do
print*, "At least",count," reallocations during shrinking"
deallocate(a)
call system_clock(toc,rate)
print*, "Elapsed time =", (toc-tic)/rate

print*
print*, "====== WITH A ENHANCED ALLOCATABLE"
print*, "starts with size=0"
print*, "iteratively append 1 element 100000 times"
call system_clock(tic,rate)
cap = 0
do i = 1, 100000
   call resize(a,extend=real(i))
   newcap = capa(a)
   if (newcap /= cap) then
      print*, "size = ", size(a), "   capacity changed from", cap, " to:", newcap
      cap = capa(a)
   end if
   call random_number(x); k = ceiling(x*i)
   if (nint(a(k)) /= k) error stop "Problem while growing 1 by 1"
end do
call system_clock(toc,rate)
print*, "Elapsed time =", (toc-tic)/rate
call system_clock(tic,rate)
do i = 100000, 1, -1
   call resize(a,drop=1,container='any')
   newcap = capa(a)
   if (newcap /= cap) then
      print*, "size = ", size(a), "   capacity changed from", cap, " to:", newcap
      cap = capa(a)
   end if
   if (i /= 1) then
      call random_number(x); k = ceiling(x*size(a))
      if (nint(a(k)) /= k) error stop "Problem while shrinking 1 by 1"
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
call resize(a,lb=1,ub=1)
a(1) = 0.0
do i = 1, 18
   S=real(i); call resize(a,lb=1,ub=3*size(a),keep=.true.,source=S)
   newcap = capa(a)
   if (newcap /= cap) then
      print*, "size = ", size(a), "   capacity changed from", cap, " to:", newcap
      cap = capa(a)
   end if
end do
do i = 18, 1, -1
   call resize(a,drop=2*size(a)/3,container='any')
   newcap = capa(a)
   if (newcap /= cap) then
      print*, "size = ", size(a), "   capacity changed from", cap, " to:", newcap
      cap = capa(a)
   end if
   if (nint(a(size(a))) /= i-1) error stop "Problem while shrinking by 2/3"
end do
call edeallocate(a)

print*
print*, "====== WITH A 2D ENHANCED ALLOCATABLE"
print*, "starts with size=0"
call resize(aa,1,0,1,0)
print*, lbound(aa), shape(aa), capa(aa)
allocate( bb(0:999,5) )
print*, "resize with mold(0:999,5)"
call resize(aa,mold=bb) 
print*, lbound(aa), shape(aa), capa(aa)
deallocate(bb); allocate( bb(500,-10:10), source=42.0 )
print*, "resize with source(500,-10,10)"
call resize(aa,source=bb) 
print*, lbound(aa), shape(aa), capa(aa), aa(250,0)
if (any(aa /= 42.0)) ERROR STOP "problem with source="
call edeallocate( aa )



END
