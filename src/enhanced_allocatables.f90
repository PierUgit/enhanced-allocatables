!***********************************************************************************************
MODULE enhanced_allocatables_m
USE iso_c_binding
implicit none

   PRIVATE
   
   PUBLIC eallocate, resize, edeallocate, capa, print_info
   
   integer, parameter :: MAX_OVERP = 2*10**4
   
   INTERFACE eallocate
      MODULE PROCEDURE eallocate1, eallocate2
   END INTERFACE
   
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
   subroutine eallocate1(x,lb,ub,capacity)
   !********************************************************************************************
   ! simulation of allocate( a(lb:ub)[, capacity=cap] )
   !
   ! In a standard implementation it would just be the classical allocate() statement
   ! with the "capacity" specifier in addition. 
   ! Needed here to initialize the management of the capacity
   !********************************************************************************************
   real, allocatable, intent(inout)           :: x(:)
   integer,           intent(in)              :: lb, ub
   integer,           intent(in),    optional :: capacity
   !********************************************************************************************
   allocate( x(lb:ub) )
   call alloc(x,capacity)
  
   end subroutine
   
   
   !********************************************************************************************
   subroutine eallocate2(x,lb1,ub1,lb2,ub2,capacity)
   !********************************************************************************************
   ! simulation of allocate( a(lb1:ub1,lb2:ub2) [, capacity=cap] )
   !
   ! In a standard implementation it would just be the classical allocate() statement
   ! with the "capacity" specifier in addition. 
   ! Needed here to initialize the management of the capacity
   !********************************************************************************************
   real, allocatable, intent(inout)           :: x(:,:)
   integer,           intent(in)              :: lb1, ub1, lb2, ub2
   integer,           intent(in),    optional :: capacity
   !********************************************************************************************
   allocate( x(lb1:ub1,lb2:ub2) )
   call alloc(x,capacity)
  
   end subroutine
   
   
   !********************************************************************************************
   subroutine resize1(x,lb,ub,keep,capacity,container,extend,drop,mold,source)
   !********************************************************************************************
   ! simulation of the new resize statement
   !********************************************************************************************
   real, allocatable, intent(inout)            :: x(:)
   integer,           intent(in),     optional :: lb, ub
   logical,           intent(in),     optional :: keep
   integer,           intent(in),     optional :: capacity
   character(*),      intent(in),     optional :: container
   real,              intent(in),     optional :: extend(:)
   integer,           intent(in),     optional :: drop
   real, allocatable, intent(in),     optional :: mold(:)
   real, allocatable, intent(in),     optional :: source(..)
   
   integer :: lb___, newlb, size___, newsize, cap___, newcap
   logical(c_bool) :: keep___
   character(8) :: con
   !********************************************************************************************
   keep___ = .false. ; if (present(keep)) keep___ = keep
   con = 'grow'; if (present(container)) con = container
   
   if (present(capacity).and.present(container)) &
      error stop "capacity= and container= are mutually exclusive"
   if (present(extend).and.present(drop)) &
      error stop "extend= and drop= are mutually exclusive"
   if ((present(extend).or.present(drop)).and..not.keep___) &
      error stop "keep must be .true. is extend= or drop= are present"
   if (present(mold)) then
      if (present(lb).or.present(ub)) &
         error stop "lb=/ub= must not be present if mold= is present"
      if (present(extend).or.present(drop)) &
         error stop "extend=/drop= must not be present if mold= is present"
   end if
   if (present(source)) then
      if (rank(source) == 1) then
         if (present(lb).or.present(ub)) &
            error stop "lb=/ub= must not be present if source= is rank 1"
         if (present(extend).or.present(drop)) &
            error stop "extend=/drop= must not be present if source= is rank 1"
         if (keep___) &
            error stop "keep= must be .false. if source= is rank 1"
      else if (rank(source) > 1) then
         error stop "source= must be rank 0 or 1"
      end if
   end if
   if ((present(extend).or.present(drop))) then
      if (present(lb).and.present(ub)) &
         error stop "lb1= and ub1= must not be both present if extend= or drop= are present"
   end if

   size___ = size(x);         newsize = size___        
   cap___  = capa(x);         newcap  = cap___
   lb___   = lbound(x,dim=1); newlb   = lb___
   
   ! determine bounds and size
   if (present(lb).and..not.present(ub)) then
      newlb = lb
   else if (.not.present(lb).and.present(ub)) then
      newlb = ub-size___+1
   else if (present(lb).and.present(ub)) then
      newsize = max(ub-lb+1,0)
      newlb = lb
   end if
   if (newsize <= 0) then
      newlb = 1
      newsize = 0
   end if

   if (present(mold)) then
      newsize = size(mold)
      newlb = lbound(mold,1)
   else if (present(source)) then
      if (rank(source) == 1) then
         newsize = size(source)
         newlb = lbound(source,1)
      end if
   end if
   if (present(extend)) newsize = newsize + size(extend)
   if (present(drop)) newsize = newsize - drop
   
   ! determine capacity
   if (present(capacity)) then
      newcap = max(capacity,newsize)
   else if (con == 'fit') then
      newcap = newsize
   else if (newsize > size___ .and. con /= 'fit') then
      do while (newsize > newcap)
         newcap = 2 * newcap 
      end do 
      newcap = min(newcap, newsize + MAX_OVERP)
   else if (newsize < size___ .and. con == 'any') then
      do while (newsize < newcap/3)
         newcap = newcap / 2
      end do
      if (newsize < newcap - 3*MAX_OVERP/2) newcap = newsize + MAX_OVERP
   end if
   newcap = max(newcap,1)
   
   ! update everything
   if (newlb   /= lb___  ) call set_lbound(x,1,newlb)
   if (newcap  /= cap___ ) call set_capacity(x,newcap,keep___)
   if (newsize /= size___) call set_size(x,1,newsize)
   
   ! copy the extend content
   if (present(extend)) x(newlb+size___:newlb+newsize-1) = extend(:)
   ! copy the source content
   if (present(source)) then
      select rank(source)
      rank(0)
         if (keep___) then
            x(newlb+size___:newlb+newsize-1) = source
         else
            x(:) = source
         end if
      rank(1)
         x(:) = source(:)
      end select
   end if
   
   end subroutine
   
   !********************************************************************************************
   subroutine resize2(x,lb1,ub1,lb2,ub2,keep,capacity,container,extend,drop,mold,source)
   !********************************************************************************************
   ! simulation of the new resize statement
   !********************************************************************************************
   real, allocatable, intent(inout)            :: x(:,:)
   integer,           intent(in),     optional :: lb1, ub1, lb2, ub2
   logical,           intent(in),     optional :: keep
   integer,           intent(in),     optional :: capacity
   character(*),      intent(in),     optional :: container
   real,              intent(in),     optional :: extend(:,:)
   integer,           intent(in),     optional :: drop
   real, allocatable, intent(in),     optional :: mold(:,:)
   real, allocatable, intent(in),     optional :: source(..)
   
   integer :: lb___(2), newlb(2), size___(2), newsize(2), cap___, newcap, i
   logical(c_bool) :: keep___
   character(8) :: con
   !********************************************************************************************
   keep___ = .false. ; if (present(keep)) keep___ = keep
   con = 'grow'; if (present(container)) con = container
   
   if (present(capacity).and.present(container)) &
      error stop "capacity= and container= are mutually exclusive"
   if (present(extend).and.present(drop)) &
      error stop "extend= and drop= are mutually exclusive"
   if ((present(extend).or.present(drop)).and..not.keep___) &
      error stop "keep must be .true. is extend= or drop= are present"
   if (present(mold)) then
      if (present(lb1).or.present(ub1).or.present(lb2).or.present(ub2)) &
         error stop "lb=/ub= must not be present if mold= is present"
      if (present(extend).or.present(drop)) &
         error stop "extend=/drop= must not be present if mold= is present"
   end if
   if (present(source)) then
      if (rank(source) == 2) then
         if (present(lb1).or.present(ub1).or.present(lb2).or.present(ub2)) &
            error stop "lb=/ub= must not be present if source= is rank 2"
         if (present(extend).or.present(drop)) &
            error stop "extend=/drop= must not be present if source= is rank 2"
         if (keep___) &
            error stop "keep= must be .false. if source= is rank 2"
      else if (rank(source) /= 0) then
         error stop "source= must be rank 0 or 2"
      end if
   end if
   if ((present(extend).or.present(drop))) then
      if (present(lb1).and.present(ub1)) &
         error stop "lb1= and ub1= must not be both present if extend= or drop= are present"
      if (present(lb2).and.present(ub2)) &
         error stop "lb2= and ub2= must not be both present if extend= or drop= are present"
   end if
   
   size___ = shape(x);                      newsize = size___        
   cap___  = capa(x);                       newcap  = cap___
   lb___   = lbound(x);                     newlb   = lb___
   
   ! determine bounds and size
   if (present(lb1).and..not.present(ub1)) then
      newlb(1) = lb1
   else if (.not.present(lb1).and.present(ub1)) then
      newlb(1) = ub1-size___(1)+1
   else if (present(lb1).and.present(ub1)) then
      newsize(1) = max(ub1-lb1+1,0)
      newlb(1) = lb1
   end if
   if (present(lb2).and..not.present(ub2)) then
      newlb(2) = merge(lb2, 1, product(size___) > 0)
   else if (.not.present(lb2).and.present(ub2)) then
      newlb(2) = merge(ub2-size___(2)+1, 1, product(size___) > 0)
   else if (present(lb2).and.present(ub2)) then
      if (present(extend).or.present(drop)) &
         error stop "extend= or drop= must not be coded if both lb= and ub= are coded"
      newsize(2) = max(ub2-lb2+1,0)
      newlb(2) = merge(lb2, 1, product(newsize) > 0)
   end if
   where (newsize(:) <= 0)
      newlb(:) = 1
      newsize(:) = 0
   end where

   if (present(mold)) then
      newsize = shape(mold)
      newlb = lbound(mold)
   else if (present(source)) then
      if (rank(source) == 2) then
         newsize = shape(source)
         newlb = lbound(source)
      end if
   end if
   if (present(extend)) newsize(2) = newsize(2) + size(extend,2)
   if (present(drop)) newsize(2) = newsize(2) - drop
   
   ! determine capacity
   if (present(capacity)) then
      newcap = max(capacity,product(newsize))
   else if (con == 'fit') then
      newcap = product(newsize)
   else if (product(newsize) > product(size___) .and. con /= 'fit') then
      do while (product(newsize) > newcap)
         newcap = 2 * newcap 
      end do 
      newcap = min(newcap, product(newsize) + MAX_OVERP)
   else if (product(newsize) < product(size___) .and. con == 'any') then
      do while (product(newsize) < newcap/3)
         newcap = newcap / 2
      end do
      if (product(newsize) < newcap - 3*MAX_OVERP/2) newcap = product(newsize) + MAX_OVERP
   end if
   newcap = max(newcap,1)
   
   ! update everything
   if (newcap  /= cap___ ) call set_capacity(x,newcap,keep___)
   do i = 1, 2
      if (newlb(i)   /= lb___(i)  ) call set_lbound(x,i,newlb(i))
      if (newsize(i) /= size___(i)) call set_size(x,i,newsize(i))
   end do
   
   ! copy the extend content
   if (present(extend)) x(:,newlb(2)+size___(2):newlb(2)+newsize(2)-1) = extend(:,:)
   ! copy the source content
   if (present(source)) then
      select rank(source)
      rank(0)
         if (keep___) then
            x(:,newlb(2)+size___(2):newlb(2)+newsize(2)-1) = source
         else
            x(:,:) = source
         end if
      rank(2)
         x(:,:) = source(:,:)
      end select
   end if
   
   end subroutine
      !********************************************************************************************
   subroutine edeallocate(x)
   !********************************************************************************************
   real, allocatable, intent(inout) :: x(..)   
   !********************************************************************************************
   call dealloc(x)
   select rank(x)
   rank(1)
      deallocate(x)
   rank(2)
      deallocate(x)
   end select
   
   end subroutine
   
END MODULE enhanced_allocatables_m
