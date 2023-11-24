!***********************************************************************************************
MODULE enhanced_allocatables_m
USE iso_c_binding
implicit none

   PRIVATE
   
   PUBLIC eallocate, resize, edeallocate, capa, print_info
   
   integer, parameter :: MAX_OVERP = 2*10**4
      
   INTERFACE 
   
      integer(c_int) function capa(x) bind(C,name="ea_capacity")
         import c_float, c_int
         real(c_float), allocatable, intent(in) :: x(:)
      end function
      
      subroutine print_info(x) bind(C,name="ea_printInfo")
         import c_float
         real(c_float), allocatable, intent(in) :: x(:)
      end subroutine
      
      subroutine alloc(x,c) bind(C,name="ea_alloc")
         import c_float
         real(c_float), allocatable, intent(inout) :: x(:)
         integer, intent(in), optional :: c
      end subroutine
      
      subroutine set_lbound(x,lb) bind(C,name="ea_setLBound")
         import c_float, c_int
         real(c_float), allocatable, intent(inout) :: x(:)
         integer(c_int), intent(in) :: lb 
      end subroutine
      
      subroutine set_size(x,newsize) bind(C,name="ea_setSize")
         import c_float, c_int
         real(c_float), allocatable, intent(inout) :: x(:)
         integer(c_int), intent(in) :: newsize 
      end subroutine
      
      subroutine set_capacity(x,newcap,keep) bind(C,name="ea_setCapacity")
         import c_float, c_int, c_bool
         real(c_float), allocatable, intent(inout) :: x(:)
         integer(c_int), intent(in) :: newcap 
         logical(c_bool) :: keep
      end subroutine
      
      subroutine dealloc(x) bind(C,name="ea_deAlloc")
         import c_float
         real(c_float), allocatable, intent(inout) :: x(:)
      end subroutine
      
   END INTERFACE
   
CONTAINS

   !********************************************************************************************
   subroutine eallocate(x,lb,ub,capacity)
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
   subroutine resize(x,lb,ub,keep,capacity,container,extend,drop,mold,source)
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
   real,              intent(in),     optional :: mold(:)
   real,              intent(in),     optional :: source(..)
   
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
      select rank(source)
      rank(0)
         continue
      rank(1)
         if (present(lb).or.present(ub)) &
            error stop "lb=/ub= must not be present if source= is rank 1"
         if (present(extend).or.present(drop)) &
            error stop "extend=/drop= must not be present if source= is rank 1"
         if (keep___) &
            error stop "keep= must be .false. if source= is rank 1"
      rank(default)
         error stop "source= must be rank 0 or 1"
      end select
   end if
      
   size___ = size(x);         newsize = size___        
   cap___  = capa(x);         newcap  = cap___
   lb___   = lbound(x,dim=1); newlb   = lb___
   
   ! determine bounds and size
   if (present(lb).and..not.present(ub)) then
      newlb = merge(lb, 1, size___ > 0)
   else if (.not.present(lb).and.present(ub)) then
      newlb = merge(ub-size___+1, 1, size___ > 0)
   else if (present(lb).and.present(ub)) then
      if (present(extend).or.present(drop)) &
         error stop "extend= or drop= must not be coded if both lb= and ub= are coded"
      newsize = max(ub-lb+1,0)
      newlb = merge(lb, 1, newsize > 0)
   else if (present(mold)) then
      newsize = size(mold)
      newlb = merge(lbound(mold), 1, newsize > 0)
   else if (present(source)) then
      select rank(source)
      rank(1)
         newsize = size(source)
         newlb = merge(lbound(source), 1, newsize > 0)
      end select
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
   if (newlb   /= lb___  ) call set_lbound(x,newlb)
   if (newcap  /= cap___ ) call set_capacity(x,newcap,keep___)
   if (newsize /= size___) call set_size(x,newsize)
   
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
   subroutine edeallocate(x)
   !********************************************************************************************
   real, allocatable, intent(inout) :: x(:)   
   !********************************************************************************************
   call dealloc(x)
   deallocate(x)
   
   end subroutine
   
END MODULE enhanced_allocatables_m
