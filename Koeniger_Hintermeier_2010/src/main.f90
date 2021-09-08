program main
    
    use mod_utilities
    use mod_globals
    use mod_solve
    use mod_numerical
    USE OMP_LIB 
    
    implicit none
    
    ! Declare variables
    real(8) :: tic, toc
    integer :: flag_vfi
    
    ! Integer policy functions
    !integer, allocatable :: polind_xprime(:,:,:)
    integer, allocatable :: polind_dprime(:,:,:)
    
    ! Value and policy functions to be communicated to Matlab
    real(8), allocatable :: value(:,:,:)      ! V(x,d,z) in the state space (x,d,z), value function
    real(8), allocatable :: pol_xprime(:,:,:) ! x'(x,d,z) in the state space (x,d,z), total wealth
    real(8), allocatable :: pol_dprime(:,:,:) ! d'(x,d,z) in the state space (x,d,z), durable
    real(8), allocatable :: pol_aprime(:,:,:) ! a'(x,d,z) in the state space (x,d,z), financial assets
    real(8), allocatable :: pol_c(:,:,:)      ! c(x,d,z) in the state space (x,d,z), consumption
    
    ! STEP 1 - Read files from Matlab
    write(*,*) "Reading files in Fortran.."
    call readfiles()
    write(*,*) "Files read!"
    
    write(*,*) "Discretization of the state space:"
    write(*,*) "No. of grid points for x (wealth)    =", nx
    write(*,*) "No. of grid points for d (durable)   =", nd
    write(*,*) "No. of grid points for z (exo shock) =", nz
    write(*,*) "  "
    
    !call omp_set_num_threads(5)
    !$omp parallel if (par_fortran==1)
    write(*,*) "How many cores are used in parallel?", omp_get_num_threads()
    !$omp end parallel
      
    ! STEP 2 - Call VFI to solve the model
    if (verbose>=1) then
        write(*,*) 'Starting VFI..'
        if (howard==1) write(*,*) 'with HOWARD'
        tic = omp_get_wtime()
    endif
    allocate(value(nx,nd,nz),pol_xprime(nx,nd,nz),pol_dprime(nx,nd,nz),polind_dprime(nx,nd,nz))
    
    write(*,*) "VALUE FUNCTION STARTS"
    write(*,*) "howard = ", howard
    call fun_vfi_2endo_states(value,pol_xprime,pol_dprime,polind_dprime,flag_vfi)
    
    if (verbose>=1) then
        toc = omp_get_wtime()
        write(*,*) 'Time to do VFI: ', real(toc-tic)
    endif
    if (flag_vfi<0) then
        call myerror('VFI did no converge successfully!')
    endif
    
    ! STEP 3 - Compute other policy functions
    ! subroutine MakePolicy(value,pol_xprime,pol_dprime,pol_aprime,pol_c, polind,V)
    if (verbose>=1) then
        write(*,*) "Compute other policy functions"
    endif
    
    allocate(pol_aprime(nx,nd,nz),pol_c(nx,nd,nz))
    
    call MakePolicy(pol_aprime,pol_c, pol_xprime,pol_dprime)
    
    ! Write output to files to be imported back to Matlab
    write(*,*) "Writing files in Fortran.."
    call writefiles()
    write(*,*) "Files written!"
    
    deallocate(value,pol_xprime,pol_dprime,pol_aprime,pol_c)
    deallocate(z_grid,x_grid,d_grid,z_prob)
    
    contains
    
    !-----------------------------------------------------!
    
    subroutine readfiles()
    ! Variables
    integer :: real_params_dim, int_params_dim, i
    integer :: dimensions(2)
    integer, allocatable :: int_params(:)
    real(8), allocatable :: real_params(:)
    
    ! Body of readfiles
    
    call read1dim(dimensions,'dimensions.txt')
    real_params_dim = dimensions(1)
	int_params_dim  = dimensions(2)
    
    write(*,*) "Done until here"
    
    !Allocate
	allocate(real_params(real_params_dim))
	allocate(int_params(int_params_dim))
    
    !Reading in the real parameters for the problem:
    call read1dim(int_params,'int_params.txt')
    
    !Reading in the integer parameters for the problem:
    call read1dim(real_params,'real_params.txt')
    
    ! Assign real parameters
    i = 1
    r       = real_params(i); i=i+1
    delta   = real_params(i); i=i+1
    beta    = real_params(i); i=i+1
    sigma   = real_params(i); i=i+1
    theta   = real_params(i); i=i+1
    y_gam   = real_params(i); i=i+1
    epscon  = real_params(i); i=i+1
    epsdur  = real_params(i); i=i+1
    miu     = real_params(i); i=i+1
    gamma   = real_params(i); i=i+1
    alpha   = real_params(i); i=i+1
    tol_val = real_params(i)
    
    ! Assign integer parameters
    i = 1
    nz        = int_params(i); i=i+1
    nx        = int_params(i); i=i+1
    nd        = int_params(i); i=i+1
    maxit_val = int_params(i); i=i+1
    verbose   = int_params(i); i=i+1
    mqpbounds = int_params(i); i=i+1
    howard    = int_params(i); i=i+1
    n_howard  = int_params(i); i=i+1
    par_fortran   = int_params(i); i=i+1
    lowMem        = int_params(i); i=i+1
    do_monot      = int_params(i); i=i+1
    do_refinement = int_params(i)
    
    !Reading arrays
    allocate(z_grid(nz),x_grid(nx),d_grid(nd),z_prob(nz,nz))
    
    call read1dim(z_grid,'z_grid.txt')
    call read1dim(x_grid,'x_grid.txt')
    call read1dim(d_grid,'d_grid.txt')
    call read2dim(z_prob,'z_prob.txt')

    end subroutine readfiles
    !-----------------------------------------------------!
    
    subroutine writefiles()
    ! Variables
    
    ! Body of writefiles
    !real(8), allocatable :: value(:,:,:)      ! V(x,d,z) in the state space (x,d,z), value function
    !real(8), allocatable :: pol_xprime(:,:,:) ! x'(x,d,z) in the state space (x,d,z), total wealth
    !real(8), allocatable :: pol_dprime(:,:,:) ! d'(x,d,z) in the state space (x,d,z), durable
    !real(8), allocatable :: pol_aprime(:,:,:) ! a'(x,d,z) in the state space (x,d,z), financial assets
    !real(8), allocatable :: pol_c(:,:,:)      ! c(x,d,z) in the state space (x,d,z), consumption
    ! flag_vfi
    
    ! Write arrays in text format
    !call write3dim(value,'value.txt')
    !call write3dim(pol_xprime,'pol_xprime.txt')
    !call write3dim(pol_dprime,'pol_dprime.txt')
    !call write3dim(pol_aprime,'pol_aprime.txt')
    !call write3dim(pol_c,'pol_c.txt')
    
    ! Write arrays in binary format
    call write1dimBinary(reshape(value,[nx*nd*nz]),'value.bin')
    call write1dimBinary(reshape(pol_xprime,[nx*nd*nz]),'pol_xprime.bin')
    call write1dimBinary(reshape(pol_dprime,[nx*nd*nz]),'pol_dprime.bin')
    call write1dimBinary(reshape(pol_aprime,[nx*nd*nz]),'pol_aprime.bin')
    call write1dimBinary(reshape(pol_c,[nx*nd*nz]),'pol_c.bin')
    
    ! Scalars are always written in text format for simplicity
    call writescalar(flag_vfi,'flag_vfi.txt')
    
    
    
    end subroutine writefiles
    !-----------------------------------------------------!
    
end program main
    
    