module mod_numerical
    
    ! USE other modules 
    implicit none
    
        
    interface cumprod
    module procedure cumprod_r, cumprod_i
    end interface cumprod
    
   
contains
    
    subroutine myerror(string)

    implicit none
    character(len=*), intent(in) :: string
    write(*,*) "ERROR: ", string
    write(*,*) "Program will terminate.."
    pause
    stop

    end subroutine myerror
    
    !===============================================================================!
    
    ! Compute the cumulative product of the vector x
function cumprod_r(x) result(y)
    implicit none
    real(8), intent(in) :: x(1:)
    real(8) :: y(size(x))
    !local
    real(8) :: tmp
    integer :: i


    y(1) = x(1)
    tmp = x(1)
    do i = 2,size(x)
        tmp = tmp*x(i)
        y(i) = tmp
    end do

end function cumprod_r
!===============================================================================!

function cumprod_i(x) result(y)
    implicit none
    integer, intent(in) :: x(1:)
    integer  :: y(size(x))
    !local
    integer :: tmp
    integer :: i

    y(1) = x(1)
    tmp = x(1)
    do i = 2,size(x)
        tmp = tmp*x(i)
        y(i) = tmp
    end do

end function cumprod_i
!===============================================================================!
    
! Code is based of the matlab ind2sub.m
! Given an integer vector siz that gives the shape of
! some array and the linear index i, convert the linear
! index into an i1,i2,... such that 
!  a(i) = a(i1,i2,...)
function ind2sub(siz,i) result(ivec)
    integer, intent(in) :: siz(:)
    integer :: i
    integer :: ivec(size(siz))
    ! local
    integer :: d
    integer :: j,k
    integer :: cumprodivec(size(siz))

    cumprodivec(1) = 1
    cumprodivec(2:size(siz)) = cumprod(siz(1:size(siz)-1))

    j = i
    do d = size(siz),1,-1
        k = mod(j-1,cumprodivec(d)) + 1
        ivec(d) = (j-k)/cumprodivec(d) + 1
        j = k
    end do

end function
!===============================================================================!

subroutine golden_method(f, a, b, x1, f1, mytol, mymaxit)
    ! Purpose: Applies Golden-section search to search for the *maximum* of a function 
    ! in the interval (a, b).
    ! Source: https://en.wikipedia.org/wiki/Golden-section_search
    ! Adapted to Fortran90 from: https://github.com/QuantEcon
    
    !---------------------------------------------------!
    !INPUTS
    interface
        function f(x)
        implicit none
        real(8), intent(in) :: x
        real(8) :: f
        end function f
    end interface
    real(8), intent(in) :: a, b
    !Some optional inputs
    integer,  optional :: mymaxit
    real(8), optional :: mytol
    !OUTPUTS
    real(8), intent(out) :: x1, f1
    !---------------------------------------------------!
    
    !Locals
    integer :: maxit, it
    real(8) :: tol, alpha1, alpha2, d, f2, x2, s
  
    !! Assign default value to maxit if not defined by user
    if (present(mymaxit)) then
        maxit = mymaxit
    else
        maxit = 1000
    end if
    
    ! Assign default value to tol if not defined by user
    if (present(mytol)) then
        tol = mytol
    else
        tol = 1.0d-6
    end if
  
    alpha1 = (3.d0 - sqrt(5.d0)) / 2.d0
    alpha2 = 1.d0 - alpha1
    d = b - a
    x1 = a + alpha1*d
    x2 = a + alpha2*d
    s = 1.d0
    f1 = f(x1)
    f2 = f(x2)
    d = alpha1*alpha2*d
  
    it = 0
  
    do while ((d.gt.tol).and.(it.lt.maxit))
        it = it + 1
        d = d*alpha2
  
        if (f2.gt.f1) then
            x1 = x2
            f1 = f2
            x2 = x1 + s*d
        else
            x2 = x1 - s*d
        end if
  
        s = sign(s, x2-x1)
        f2 = f(x2)
    end do
  
    if (it.ge.maxit) then
        print *, "Golden method: Maximum iterations exceeded"
    end if
  
    if (f2.gt.f1) then
        x1 = x2
        f1 = f2
    end if
    
    end subroutine golden_method
    !===============================================================================!

end module mod_numerical
