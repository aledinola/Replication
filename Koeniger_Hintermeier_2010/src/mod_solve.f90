module mod_solve
use mod_globals
use mod_numerical
use LinInterpModule, only: lininterp1 
USE OMP_LIB

implicit none

integer :: dp_c_com, z_c_com
real(8) :: dprime_val_com,d_val_com,cash_com
real(8), allocatable :: EV_com(:,:,:)

!$omp threadprivate(dp_c_com,z_c_com,dprime_val_com,d_val_com,cash_com,EV_com)

contains
    
pure function rhs_bellman(xprime) result(F)
implicit none
! Input/output variables
real(8), intent(in) :: xprime !Next-period value of wealth, x'
real(8) :: F !Function result

F = ReturnFn2(xprime,dprime_val_com,d_val_com,cash_com)+beta*lininterp1(x_grid,EV_com(:,dp_c_com,z_c_com),xprime)


end function rhs_bellman
    
!=========================================================!

subroutine fun_vfi_2endo_states(V,pol_xprime,pol_dprime,polind_dprime,conv_flag)
! DESCRIPTION:
!   Solve DP problem via discrete value function iteration. DP problem is
!   defined as follows:
!       V(x,d,z) = \max_{x',d'} {R(x',d',x,d,z)+discfactor*E[V(x',d',z')|z]}
!   where
!   (x,d) are the two endogenous states (x is financial assets, d is durable stock)
!   z is the exogenous state 
implicit none
! OUTPUT variables
real(8), intent(inout) :: V(:,:,:)          ! Value function V(x,d,z)
real(8), intent(out)   :: pol_xprime(:,:,:) ! Policy function x'(x,d,z), value
real(8), intent(out)   :: pol_dprime(:,:,:) ! Policy function d'(x,d,z), value
integer, intent(out)   :: polind_dprime(:,:,:) ! Policy function d'(x,d,z), index
integer, intent(out)   :: conv_flag         ! Convergence flag (0 = ok, -1 = no converg)
!! INPUT variables
!real(8), intent(in) :: discfactor  ! Discount factor, in (0,1)
!integer, intent(in) :: nz          ! no. grid points grid for exogenous state variable z
!real(8), intent(in) :: z_prob(:,:) ! Markov chain for exgenous shock z

! Local variables
integer :: iter
integer :: z_c, d_c, dp_c, x_c, xp_c
real(8) :: z_val, x_val, d_val, dprime_val, xprime_val, payoff, cash
integer :: last_max_x, max_loc, left_loc, right_loc, d_opt_ind
real(8) :: Vtmp, tmp, xpol_ref, val_ref
real(8) :: check_sum(nz), err
real(8), allocatable :: Vnew(:,:,:), EV(:,:,:), cash_arr(:,:,:,:)
real(8), allocatable :: Vnew_given_dp(:,:,:,:)
real(8), allocatable :: pol_xprime_given_dp(:,:,:,:)
!--------------------- Body of sub_vfi -----------------------------!

! Input checks
if (any(z_prob<0d0)) then
    call myerror("All elements of z_prob must be non-negative")
endif
check_sum = abs(sum(z_prob,dim=2)-1d0)
if (any(check_sum>1d-6)) then
    call myerror("All rows of z_prob must sum to 1")
endif
if (beta<0d0 .or. beta>1d0) then
    call myerror("Discount factor must be in (0,1)")
endif
if (howard==1 .and. mqpbounds==1) then
    call myerror("Cannot use both MQP bounds and Howard")
endif

! Recall: Total number of endo states is ns=nx*nd
allocate(Vnew(nx,nd,nz), EV(nx,nd,nz), EV_com(nx,nd,nz), cash_arr(nx,nd,nd,nz), &
    Vnew_given_dp(nx,nd,nd,nz), pol_xprime_given_dp(nx,nd,nd,nz))

! Set initial guess for V
V    = 0d0
Vnew = 0d0

iter = 1
err  = tol_val+1d0
conv_flag = 0 ! so far, so good

! Compute cash on hand once and for all cash(x,d',d,z)
do z_c = 1,nz ! shock state
    do d_c = 1,nd ! durable state
        do dp_c = 1,nd ! durable choice
            do x_c = 1,nx ! wealth state
                cash_arr(x_c,dp_c,d_c,z_c) = fun_cash(d_grid(dp_c),x_grid(x_c),d_grid(d_c),z_grid(z_c))
            enddo
        enddo
    enddo
enddo


do while (err>tol_val .and. iter<=maxit_val)
    
    ! Compute the continuation value EV(x',d',z)
    ! Note that z' is integrated out
    do z_c = 1,nz 
        do dp_c = 1,nd 
            do x_c = 1,nx 
                EV(x_c,dp_c,z_c) = dot_product(V(x_c,dp_c,:),z_prob(z_c,:))
            enddo
        enddo
    enddo
    
    ! For each (x,d,z), solve d'-contingent problem
    !$omp parallel if (par_fortran==1) default(shared) private(z_c,d_c,dp_c,&
    !$ last_max_x,x_c,z_val,x_val,d_val,dprime_val,cash,Vtmp,xp_c,xprime_val,&
    !$ payoff,tmp,max_loc,left_loc,right_loc,xpol_ref,val_ref,d_opt_ind)
    !$omp do collapse(2)
    do z_c = 1,nz ! exogenous state
        do d_c = 1,nd ! endogenous state d
            do dp_c = 1,nd ! durable choice d'
                
                last_max_x = 1 !Location of last optimal wealth (for monotonicity)
                
                do x_c = 1,nx ! endogenous state x
                    
                    z_val = z_grid(z_c)
                    x_val = x_grid(x_c)
                    d_val = d_grid(d_c)
                    dprime_val = d_grid(dp_c)
                    cash = cash_arr(x_c,dp_c,d_c,z_c)
                    
                    ! Solve for optimal x' given durable choice d'
                    Vtmp = -huge(0d0)
                    do xp_c = last_max_x,nx
                        xprime_val = x_grid(xp_c)
                        payoff = ReturnFn2(xprime_val,dprime_val,d_val,cash)
                        tmp    = payoff + beta*EV(xp_c,dp_c,z_c)
                        if (tmp>Vtmp) then
                            Vtmp    = tmp
                            max_loc = xp_c
						endif
                    enddo !end xp_c
                    Vnew_given_dp(x_c,dp_c,d_c,z_c)   = Vtmp
                    pol_xprime_given_dp(x_c,dp_c,d_c,z_c) = x_grid(max_loc)
                    if (do_monot==1) last_max_x = max_loc ! Store loc of last max on the grid
                    if (do_refinement==1 .and. iter>2) then
                        ! Find the interval bracketing the true maximum
                        left_loc  = max(max_loc-1,1)
                        right_loc = min(max_loc+1,nx)
                        ! Solution of d'-contingent problem, given (x,d,z):
                        ! Set up variables to be communicated <to rhs_bellman>
                        dprime_val_com = dprime_val
                        d_val_com      = d_val
                        cash_com       = cash
                        EV_com         = EV
                        dp_c_com       = dp_c
                        z_c_com        = z_c
                        call golden_method(rhs_bellman,x_grid(left_loc),x_grid(right_loc),xpol_ref,val_ref,1d-5);
                        !val_ref  = rhs_bellman(xpol_ref)
                        Vnew_given_dp(x_c,dp_c,d_c,z_c)   = val_ref
                        pol_xprime_given_dp(x_c,dp_c,d_c,z_c) = xpol_ref
                    endif
                enddo !end x
            enddo !end d'
            
            ! Optimal durable choice is a discrete choice over d':
            do x_c = 1,nx
                d_opt_ind = maxloc(Vnew_given_dp(x_c,:,d_c,z_c),dim=1)
                polind_dprime(x_c,d_c,z_c) = d_opt_ind
                pol_dprime(x_c,d_c,z_c) = d_grid(d_opt_ind)
                Vnew(x_c,d_c,z_c)       = Vnew_given_dp(x_c,d_opt_ind,d_c,z_c)
                pol_xprime(x_c,d_c,z_c) = pol_xprime_given_dp(x_c,d_opt_ind,d_c,z_c)
            enddo
            
        enddo !end d
    enddo !end z
    !$omp enddo nowait
    !$omp end parallel
    
    !------------------- Howard --------------------------------!
    if (howard==1) then
        call sub_howard(Vnew)
    endif
    !-----------------------------------------------------------!
    
    err = maxval(abs(V-Vnew))
    
    if (verbose>=2) then
        write(*,*) "iter = ",iter, " err = ",err
    endif
    
    ! Update
    V = Vnew
    iter = iter+1
    
enddo !END DO WHILE

if (err>tol_val) then
    write(*,*) "VALUE FUNCTION DID NOT CONVERGE!"
    conv_flag = -1
else
    write(*,*) "Value function converged successfully after ", iter, " iterations"
endif

contains

!-----------------------------------------------------------!
subroutine sub_howard(Vnew)
! DESCRIPTION:
! Take Vnew as input, update n_howard times and pass it back to sub_vfi
! Declare input/output variables
real(8), intent(inout) :: Vnew(:,:,:) ! Vnew(x,d,z)
! Declare local variables
integer :: h_c

do h_c = 1,n_howard
    ! Compute the continuation value EVnew(x',d',z)
    ! Note that z' is integrated out
    do z_c = 1,nz 
        do d_c = 1,nd 
            do x_c = 1,nx 
                EV(x_c,d_c,z_c) = dot_product(Vnew(x_c,d_c,:),z_prob(z_c,:))
            enddo
        enddo
    enddo
    do z_c = 1,nz 
        do d_c = 1,nd 
            do x_c = 1,nx 
                z_val = z_grid(z_c)
                x_val = x_grid(x_c)
                d_val = d_grid(d_c)
                dp_c       = polind_dprime(x_c,d_c,z_c)
                dprime_val = pol_dprime(x_c,d_c,z_c)
                xprime_val = pol_xprime(x_c,d_c,z_c)
                cash       = fun_cash(dprime_val,x_val,d_val,z_val)
                ! Set up variables to be communicated <to rhs_bellman>
                dprime_val_com = dprime_val
                d_val_com      = d_val
                cash_com       = cash
                EV_com         = EV
                dp_c_com       = dp_c
                z_c_com        = z_c
                Vnew(x_c,d_c,z_c) = rhs_bellman(xprime_val)
            enddo
        enddo
    enddo
enddo !end h_c howard iter counter

end subroutine sub_howard

end subroutine fun_vfi_2endo_states
!=========================================================!

subroutine MakePolicy(pol_aprime,pol_c, pol_xprime,pol_dprime)
! Compute other policy functions

real(8), intent(in)  :: pol_xprime(:,:,:) ! x'(x,d,z), value
real(8), intent(in)  :: pol_dprime(:,:,:) ! d'(x,d,z), value
! As outputs, other policy functions in values
real(8), intent(out) :: pol_aprime(:,:,:),pol_c(:,:,:)

! Local variables
integer :: z_c,s_c,x_c,d_c
real(8) :: cons, x_opt_val,d_opt_val

! Body of MakePolicy

pol_aprime = 0d0
pol_c      = 0d0

do z_c = 1,nz
    do d_c = 1,nd
        do x_c = 1,nx
            ! Optimal choices x',d'
            x_opt_val = pol_xprime(x_c,d_c,z_c)
            d_opt_val = pol_dprime(x_c,d_c,z_c)
            pol_aprime(x_c,d_c,z_c) = fun_xd2a(x_opt_val,d_opt_val)
            !!subroutine ReturnFn(F,cons, xp,dp,x,d,z)
            cons = budget_constraint(x_opt_val,d_opt_val,x_grid(x_c),d_grid(d_c),z_grid(z_c))
            pol_c(x_c,d_c,z_c) = cons
        enddo
    enddo
enddo


end subroutine MakePolicy
!=========================================================!
    
end module mod_solve
    