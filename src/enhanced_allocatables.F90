!***********************************************************************************************
MODULE enhanced_allocatables_m
USE iso_c_binding
implicit none

   PRIVATE
   
   PUBLIC resize, edeallocate, capa, print_info
   
   integer, parameter :: MAX_OVERP = 100 * 10**6
   integer, parameter :: BLOCK = 4
   
   INTERFACE resize
      MODULE PROCEDURE resize1, resize2
   END INTERFACE
      
   INTERFACE 
   
      integer(c_int) function capa(x) bind(C,name="ea_capacity")
         import c_float, c_int
         real(c_float), allocatable, intent(in) :: x(..)
      end function
      
      subroutine print_info(x) bind(C,name="ea_printInfo")
         import c_float
         real(c_float), allocatable, intent(in) :: x(..)
      end subroutine
      
      subroutine alloc(x,c) bind(C,name="ea_alloc")
         import c_float
         real(c_float), allocatable, intent(inout) :: x(..)
         integer, intent(in), optional :: c
      end subroutine
      
      subroutine set_lbound(x,r,lb) bind(C,name="ea_setLBound")
         import c_float, c_int
         real(c_float), allocatable, intent(inout) :: x(..)
         integer(c_int), intent(in) :: r, lb 
      end subroutine
      
      subroutine set_size(x,r,newsize) bind(C,name="ea_setSize")
         import c_float, c_int
         real(c_float), allocatable, intent(inout) :: x(..)
         integer(c_int), intent(in) :: r, newsize 
      end subroutine
      
      subroutine set_capacity(x,newcap,keep) bind(C,name="ea_setCapacity")
         import c_float, c_int, c_bool
         real(c_float), allocatable, intent(inout) :: x(..)
         integer(c_int), intent(in) :: newcap 
         logical(c_bool) :: keep
      end subroutine
      
      subroutine dealloc(x) bind(C,name="ea_deAlloc")
         import c_float
         real(c_float), allocatable, intent(inout) :: x(..)
      end subroutine
      
   END INTERFACE
   
CONTAINS
   
   !********************************************************************************************
   subroutine resize1(x,lb,ub,keep,capacity,container,append,drop,mold,source)
   !********************************************************************************************
   ! simulation of the new resize statement
   !********************************************************************************************
   real, allocatable, intent(inout)            :: x(:)
   integer,           intent(in),     optional :: lb, ub
   logical,           intent(in),     optional :: keep
   integer,           intent(in),     optional :: capacity
   character(*),      intent(in),     optional :: container
   real,              intent(in),     optional :: append(..)
   integer,           intent(in),     optional :: drop
   real, allocatable, intent(in),     optional :: mold(..) ! should be rank-1 only, but some issue with ifort
   real, allocatable, intent(in),     optional :: source(..)
   
   integer :: newlb, oldsize, newsize, newcap
   logical(c_bool) :: ckeep
   character(8) :: con
   !********************************************************************************************
   ckeep = present(append).or.present(drop)
   if (present(keep)) ckeep = keep
   con = 'grow'; if (present(container)) con = container

   if (.not.allocated(x)) then
      call alloc(x,BLOCK)
      call set_lbound(x,1,1)
      call set_size(x,1,0)
   end if

   newsize = size(x)     ; oldsize = newsize;    
   newcap  = capa(x)
   newlb   = lbound(x,1)
   
   ! determine bounds and size
   if (present(lb)) then
      newlb = lb
      if (present(ub)) newsize = ub-lb+1
   else if (present(ub)) then
      newlb = ub-size(x)+1
   end if
   
   ! mold/source/append/drop
   if (present(mold)) then
      select rank(mold)
      rank(1)
         newsize = size(mold)
         newlb = lbound(mold,1)
      end select
   else if (present(source)) then
      if (rank(source) == 1) then
         newsize = size(source)
         newlb = lbound(source,1)
      end if
   else if (present(append)) then
      newsize = newsize + size(append)
   else if (present(drop)) then
      newsize = newsize - drop
   end if
   
   ! in case newsize drops to zero
   if (newsize <= 0) then
      newlb = 1
      newsize = 0
   end if

   ! determine capacity
   if (present(capacity)) then
      newcap = max(capacity,newsize)
   else if (con == 'fit') then
      newcap = newsize
   else if (newsize > size(x) .and. con /= 'fit') then
      do while (newsize > newcap)
         newcap = 2 * newcap 
      end do 
      newcap = min(newcap, newsize + MAX_OVERP)
   else if (newsize < size(x) .and. con == 'any') then
      do while (newsize < newcap/3)
         newcap = newcap / 2
      end do
      if (newsize < newcap - 3*MAX_OVERP/2) newcap = newsize + MAX_OVERP
   end if
   newcap = max(newcap,1)
   newcap = newcap - 1 + BLOCK - mod(newcap-1,BLOCK)
   
   ! update everything
   call set_capacity(x,newcap,ckeep)
   call set_lbound(x,1,newlb)
   call set_size(x,1,newsize)
   
   ! copy the append content
   if (present(append)) then
      select rank(append)
      rank(0)
         x(newlb+oldsize:newlb+newsize-1) = append
      rank(1)
         x(newlb+oldsize:newlb+newsize-1) = append(:)
      end select
   end if
   ! copy the source content
   if (present(source)) then
      select rank(source)
      rank(0)
         if (ckeep) then
            x(newlb+oldsize:newlb+newsize-1) = source
         else
            x(:) = source
         end if
      rank(1)
         x(:) = source(:)
      end select
   end if
   
   end subroutine
   
   !********************************************************************************************
   subroutine resize2(x,lb1,ub1,lb2,ub2,keep,capacity,container,append,drop,mold,source)
   !********************************************************************************************
   ! simulation of the new resize statement
   !********************************************************************************************
   real, allocatable, intent(inout)            :: x(:,:)
   integer,           intent(in),     optional :: lb1, ub1, lb2, ub2
   logical,           intent(in),     optional :: keep
   integer,           intent(in),     optional :: capacity
   character(*),      intent(in),     optional :: container
   real,              intent(in),     optional :: append(..)
   integer,           intent(in),     optional :: drop
   real, allocatable, intent(in),     optional :: mold(..) ! should be rank-2 only, but some issue with ifort
   real, allocatable, intent(in),     optional :: source(..)
   
   integer :: newlb(2), oldsize(2), newsize(2), newcap, i
   logical(c_bool) :: ckeep
   character(8) :: con
   !********************************************************************************************
   call check_args(2,ckeep,con,lb1,ub1,keep,capacity,container,append,drop,mold,source)
   call check_args(2,ckeep,con,lb2,ub2,keep,capacity,container,append,drop,mold,source)
   if (present(lb1).and.present(ub1)) then
      if (present(keep)) &
         error stop "keep= must not be present if both lb1= and ub1= are present"
      ckeep = .false.
   end if
   if (present(append)) then
      if (size(append,1) /= size(x,1)) &
         error stop "size(append,1) is not equal to size(x,1)"
   end if

   if (.not.allocated(x)) then
      call alloc(x,BLOCK)
      call set_lbound(x,1,1)
      call set_size(x,1,0)
      call set_lbound(x,2,1)
      call set_size(x,2,0)
   end if

   newsize = shape(x) ; oldsize = shape(x);
   newcap  = capa(x)             
   newlb   = lbound(x)
   
   ! determine bounds and size
   if (present(lb1)) then
      newlb(1) = lb1
      if (present(ub1)) newsize(1) = ub1-lb1+1
   else if (present(ub1)) then
      newlb(1) = ub1-size(x,1)+1
   end if
   if (present(lb2)) then
      newlb(2) = lb2
      if (present(ub2)) newsize(2) = ub2-lb2+1
   else if (present(ub2)) then
      newlb(2) = ub2-size(x,2)+1
   end if

   ! mold/source/append/drop
   if (present(mold)) then
      select rank(mold)
      rank(2)
         newsize = shape(mold)
         newlb = lbound(mold);
      end select
   else if (present(source)) then
      if (rank(source) == 2) then
         newsize = shape(source)
         newlb = lbound(source)
      end if
   else if (present(append)) then
      newsize(2) = newsize(2) + size(append,2)
   else if (present(drop)) then
      newsize(2) = newsize(2) - drop
   end if
   
   ! in case newsize drops to 0
   where (newsize(:) <= 0)
      newlb(:) = 1
      newsize(:) = 0
   end where

   ! determine capacity
   if (present(capacity)) then
      newcap = max(capacity,product(newsize))
   else if (con == 'fit') then
      newcap = product(newsize)
   else if (product(newsize) > size(x) .and. con /= 'fit') then
      do while (product(newsize) > newcap)
         newcap = 2 * newcap 
      end do 
      newcap = min(newcap, product(newsize) + MAX_OVERP)
   else if (product(newsize) < size(x) .and. con == 'any') then
      do while (product(newsize) < newcap/3)
         newcap = newcap / 2
      end do
      if (product(newsize) < newcap - 3*MAX_OVERP/2) newcap = product(newsize) + MAX_OVERP
   end if
   newcap = max(newcap,1)
   newcap = newcap - 1 + BLOCK - mod(newcap-1,BLOCK)
   
   ! update everything
   call set_capacity(x,newcap,ckeep)
   do i = 1, 2
      call set_lbound(x,i,newlb(i))
      call set_size(x,i,newsize(i))
   end do
   
   ! copy the append content
   if (present(append)) then
      select rank(append)
      rank(1)
         x(:,newlb(2)+newsize(2)-1) = append(:)
      rank(2)
         x(:,newlb(2)+oldsize(2):newlb(2)+newsize(2)-1) = append(:,:)
      end select
   end if
   ! copy the source content
   if (present(source)) then
      select rank(source)
      rank(0)
         if (ckeep) then
            x(:,newlb(2)+oldsize(2):newlb(2)+newsize(2)-1) = source
         else
            x(:,:) = source
         end if
      rank(2)
         x(:,:) = source(:,:)
      end select
   end if
   
   end subroutine
      
   
   !********************************************************************************************
   subroutine check_args(r,ckeep,con,lb,ub,keep,capacity,container,append,drop,mold,source)
   !********************************************************************************************
   ! simulation of the new resize statement
   !********************************************************************************************
   integer,           intent(in)               :: r
   logical(c_bool),   intent(out)              :: ckeep
   character(*),      intent(out)              :: con
   integer,           intent(in),     optional :: lb, ub
   logical,           intent(in),     optional :: keep
   integer,           intent(in),     optional :: capacity
   character(*),      intent(in),     optional :: container
   real,              intent(in),     optional :: append(..)
   integer,           intent(in),     optional :: drop
   real, allocatable, intent(in),     optional :: mold(..)
   real, allocatable, intent(in),     optional :: source(..)
   
   character(96) :: msg
   !********************************************************************************************
   ckeep = .true. ; if (present(keep))      ckeep = keep
   con   = 'grow' ; if (present(container)) con   = container
   
   if (present(capacity).and.present(container)) &
      error stop "capacity= and container= are mutually exclusive"
         
   if (present(mold)) then
      if (present(lb).or.present(ub)) &
         error stop "lb=/ub= must not be present if mold= is present"
      if (rank(mold) /= r) &
         error stop "mold= must have the same rank as array"
      if (present(keep)) &
         error stop "keep= must not be present if mold= is present"
      ckeep = .false.
   else if (present(source)) then
      if (rank(source) == r) then
         if (present(lb).or.present(ub)) &
            error stop "lb=/ub= must not be present if source= is present and is not a scalar"
         if (present(keep)) &
            error stop "keep= must not be present if source= is present and is not a scalar"
         ckeep = .false.
      else if (rank(source) /= 0) then
         write(msg,*) "source= must be rank 0 or", r
         error stop msg
      end if
   else if (present(append)) then
      if (present(lb).or.present(ub)) &
         error stop "lb=/ub= must not be present if append= is present"
      if (rank(append) < r-1 .or. rank(append) > r) &
         error stop "the rank of append= must be equal to rank(x) or rank(x)-1" 
      if (present(keep)) &
         error stop "keep= must not be present if append= is present"
   else if (present(drop)) then
      if (present(lb).or.present(ub)) &
         error stop "lb=/ub= must not be present if drop= is present"
      if (present(keep)) &
         error stop "keep= must not be present if append= is present"
   end if

   end subroutine
   
   !********************************************************************************************
   subroutine edeallocate(x)
   !********************************************************************************************
   real, allocatable, intent(inout) :: x(..)   
   !********************************************************************************************
   call dealloc(x)
   
   end subroutine
   
END MODULE enhanced_allocatables_m
