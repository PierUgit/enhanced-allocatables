PROGRAM enhanced_allocatables_p
USE enhanced_allocatables_m
implicit none

real, allocatable :: a(:), b(:)
integer :: i, cap, newcap

call eallocate(a,lb=1,ub=10,capacity=15)
a(:) = [(real(i),i=1,size(a))]
print*, lbound(a), size(a), capa(a), a(2)
   
call resize(a,lb=-2,keep=.true.)
print*, lbound(a), size(a), capa(a), a(2)

call resize(a,lb=-2,ub=15,keep=.true.)
print*, lbound(a), size(a), capa(a), a(2)

call append(a,[100.0, 200.0, 300.0])
print*, lbound(a), size(a), capa(a), a(ubound(a,1))

call resize(a,keep=.true.,container='fit')
print*, lbound(a), size(a), capa(a), a(ubound(a,1))

call edeallocate(a)

call eallocate(a,lb=0,ub=0)
a(0) = 0.0
cap = capa(a)
do i = 1, 100000
   call append(a,[real(i)])
   newcap = capa(a)
   if (newcap /= cap) then
      print*, "size = ", size(a), "   capacity changed from", cap, " to:", newcap
      cap = newcap
   end if
end do
do i = 1, 100000
   call drop(a,1)
   newcap = capa(a)
   if (newcap /= cap) then
      print*, "size = ", size(a), "   capacity changed from", cap, " to:", newcap
      cap = newcap
   end if
end do

do i = 1, 11
   call append(a,[a,a])
   newcap = capa(a)
   if (newcap /= cap) then
      print*, "size = ", size(a), "   capacity changed from", cap, " to:", newcap
      cap = newcap
   end if
end do
do i = 1, 11
   call drop(a,2*size(a)/3)
   newcap = capa(a)
   if (newcap /= cap) then
      print*, "size = ", size(a), "   capacity changed from", cap, " to:", newcap
      cap = newcap
   end if
end do

END