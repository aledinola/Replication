module mod_globals
    implicit none
    
    ! Real parameters
    real(8) :: r,delta,beta,sigma,theta,y_gam,epscon,epsdur,miu,gamma,alpha,tol_val
    
    ! Integer parameters
    integer :: maxit_val,verbose,mqpbounds,howard,n_howard,par_fortran,lowMem
    integer :: do_monot,do_refinement
    integer :: nz,nx,nd,nx_fine,nd_fine
    
    ! Allocatable real arrays
    real(8), allocatable :: z_grid(:),x_grid(:),d_grid(:),z_prob(:,:)
    
    ! Allocatable integer arrays
    integer, allocatable :: sfun_xd(:,:)
    
    
    contains
    
    pure function fun_cash(dp,x,d,z) result(F)
    implicit none
    ! Input/output variables
    real(8), intent(in)  :: dp     ! Durable choice d'
    real(8), intent(in)  :: x,d,z  ! Current state variables x,d,z
    real(8)              :: F
    
    F = x+z-((r+delta)/(1.0d0+r))*dp-adjcost(dp,d)
    
    end function fun_cash
    
    pure function budget_constraint(xp,dp,x,d,z) result(cons)
    implicit none
    ! Input/output variables
    real(8), intent(in)  :: xp,dp  ! Choice variables (future states) x',d'
    real(8), intent(in)  :: x,d,z  ! Current state variables x,d,z
    real(8)              :: cons   ! Consumption
    
    cons = x+z-((r+delta)/(1.0d0+r))*dp-adjcost(dp,d)-xp/(1.0d0+r)
    
    end function budget_constraint
    
    pure function ReturnFn2(xp,dp,d,cash) result(F)
    ! Input/output variables
    real(8), intent(in)  :: xp,dp  ! Choice variables x',d'
    real(8), intent(in)  :: d      ! Durable stock d
    real(8), intent(in)  :: cash   ! All cash
    real(8)              :: F      ! Static payoff F(x',d',x,d,z)
    ! Local variables
    real(8) :: cons, borlim
    
    F = -huge(0d0)
    cons = cash-xp/(1.0d0+r)
    ! Non-negative consumption is imposed in func.util
    ! Borrowing limit depends on durable stock
    borlim = -y_gam+(1d0-miu)*(1d0-delta)*dp
    if ( xp>=borlim ) then
        F = util(cons,d)
    endif
    
    end function ReturnFn2
    !=========================================================!
    
    pure function ReturnFn(xp,dp,x,d,z) result(F)
    ! Input/output variables
    real(8), intent(in)  :: xp,dp  ! Choice variables x',d'
    real(8), intent(in)  :: x,d,z  ! Current state variables x,d,z
    real(8)              :: F      ! Static payoff F(x',d',x,d,z)
    ! Local variables
    real(8) :: cons, borlim
    
    F = -huge(0d0)
    cons = budget_constraint(xp,dp,x,d,z)
    ! Non-negative consumption is imposed in func.util
    ! Borrowing limit depends on durable stock
    borlim = -y_gam+(1d0-miu)*(1d0-delta)*dp
    if ( xp>=borlim ) then
        F = util(cons,d)
    endif
    
    end function ReturnFn
    !=========================================================!
    
    pure function util(c,d) result(U)
    ! Input/output variables
    real(8), intent(in) :: c ! Non-durable consumption
    real(8), intent(in) :: d ! Durable consumption (housing)
    real(8) :: U             ! Utility value
    ! Local variables
    real(8) :: c_lim, agg
    
    c_lim = max(c,1d-10)
    agg   = c_lim**theta*(d+epsdur)**(1.0d0-theta)
    U     = (agg**(1.0d0-sigma)-1)/(1.0d0-sigma)
    
    end function util
    !=========================================================!
    
    pure function adjcost(dprime_val,d_val) result(F)
    ! Input/output variables
    real(8), intent(in) :: dprime_val,d_val
    real(8) :: F
    
    F = (alpha/2d0)*((dprime_val-(1d0-delta)*d_val)/d_val)**2*d_val
    
    end function adjcost
    !=========================================================!
    
    pure function fun_xd2a(x_val,d_val) result(F)
    ! Financial assets "a" as a function of "x" and "d"
    ! Input/output variables
    real(8), intent(in) :: x_val,d_val
    real(8) :: F
    
    F = x_val/(1d0+r)-((1d0-delta)/(1d0+r))*d_val
    
    end function fun_xd2a
    !=========================================================!
    
    
    
end module mod_globals