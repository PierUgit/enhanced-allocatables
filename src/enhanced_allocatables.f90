!***********************************************************************************************
MODULE enhanced_allocatables_m
USE iso_c_binding
implicit none

   PRIVATE
   
   PUBLIC eallocate, resize, edeallocate, append, drop, capa, print_info
   
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
   subroutine resize(x,lb,ub,keep,capacity,container)
   !********************************************************************************************
   ! simulation of the new resize statement
   !
   ! resize( a(lb:ub)[, keep=k][, capacity=cap][, container=con] )
   !
   !
   !********************************************************************************************
   real, allocatable, intent(inout)            :: x(:)
   integer,           intent(in),     optional :: lb, ub
   logical,           intent(in),     optional :: keep
   integer,           intent(in),     optional :: capacity
   character(*),      intent(in),     optional :: container
   
   integer :: lb___, newlb, size___, newsize, cap___, newcap
   character(8) :: con
   !********************************************************************************************
   if (present(capacity) .and. present(container)) &
      error stop "capacity= and container= are mutually exclusive"
      
   con = 'grow'; if (present(container)) con = container
   
   size___ = size(x);         newsize = size___        
   cap___  = capa(x);         newcap  = cap___
   lb___   = lbound(x,dim=1); newlb   = lb___
   
   if (present(lb).and..not.present(ub)) then
      newlb = lb
   else if (.not.present(lb).and.present(ub)) then
      newlb = ub-size(x)+1
   else if (present(lb).and.present(ub)) then
      newlb = lb
      newsize = ub-lb+1
      if (newsize > size___ .and. con /= 'fit') then
         do while (newsize > newcap)
            newcap = 2 * newcap 
         end do 
         newcap = min(newcap, newsize + MAX_OVERP)
      end if
      if (newsize < size___ .and. con == 'any') then
         do while (newsize < newcap/3)
            newcap = newcap / 2
         end do
         if (newsize < newcap - 3*MAX_OVERP/2) newcap = newsize + MAX_OVERP
      end if
   end if
   if (con == 'fit') then
      newcap = newsize
   else if (present(capacity)) then
      newcap = max(capacity,newsize)
   end if
   if (newlb   /= lb___  ) call set_lbound(x,newlb)
   if (newcap  /= cap___ ) call set_capacity(x,newcap,logical(keep,kind=c_bool))
   if (newsize /= size___) call set_size(x,newsize)
   
   end subroutine
   
   !********************************************************************************************
   subroutine append(x,y)
   !********************************************************************************************
   real, allocatable, intent(inout) :: x(:)   
   real,              intent(in)    :: y(:)
   
   integer :: l, u
   !********************************************************************************************  
   l = lbound(x,dim=1)
   u = ubound(x,dim=1)
   call resize(x,lb=l,ub=u+size(y),keep=.true.)
   x(u+1: ) = y(:)
   
   end subroutine
   
   !********************************************************************************************
   subroutine drop(x,k)
   !********************************************************************************************
   real, allocatable, intent(inout) :: x(:)   
   integer,           intent(in)    :: k
   
   integer :: l, u
   !********************************************************************************************      
   l = lbound(x,dim=1)
   u = ubound(x,dim=1)
   call resize(x,lb=l,ub=u-k,keep=.true.,container='any')
   
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
