function [sol] = solve_model(par)

sol = struct();

% Unpack some parameters
lowMem  = par.lowMem;
beta    = par.beta;
verbose = par.verbose;
x_grid  = par.x_grid;
d_grid  = par.d_grid;
x_grid_fine = par.x_grid_fine;
d_grid_fine = par.d_grid_fine;
z_grid  = par.z_grid;
z_prob  = par.z_prob;
nx      = length(x_grid);
nd      = length(d_grid);
nz      = length(z_grid);

vfi_options.maxit_val = par.maxit_val;
vfi_options.tol_val = par.tol_val;
vfi_options.verbose = par.verbose;
vfi_options.mqpbounds = par.mqpbounds;
vfi_options.howard = par.howard;
vfi_options.n_howard = par.n_howard;

% Compute big array R(x',d',x,d,z) for current payoff if the low memory
% option is not active

if lowMem==0
    disp("Precomputation of R(x',d',x,d,z) is not yet implemented")
elseif lowMem==1
    disp("Low Memory option is active")
end

% Call value function iteration
if verbose>=1
    disp('Starting VFI')
    if par.howard==1; disp("With Howard accel."); end
    tic;
end
if lowMem==0
    disp("lowMem=0 is not yet implemented")
elseif lowMem==1
    [V,polind_xprime,polind_dprime] = fun_vfi_2endo_states_v2(beta,nz,z_grid,z_prob,x_grid,d_grid,x_grid_fine,d_grid_fine,vfi_options,par,1);
end
if verbose>=1
    time=toc;
    fprintf('Time to do VFI: %8.4f \n', time)
end

% Retrieve value and policy function for the state space (x,d,z)
% % V1,pol_xprime,pol_dprime,pol_aprime

value      = zeros(nx,nd,nz);
pol_xprime = zeros(nx,nd,nz);
pol_dprime = zeros(nx,nd,nz); % d'(x,d,z)
pol_c      = zeros(nx,nd,nz); % c(x,d,z)
pol_aprime = zeros(nx,nd,nz); % a'(x,d,z)

for z_c = 1:nz %Current exo state
    for d_c = 1:nd %Current endo state
        for x_c = 1:nx %Current endo state
            % polind maps {1,2,..,nx}*{1,2,..,nd} into values on the finer
            % grids
            pol_xprime(x_c,d_c,z_c) = x_grid_fine(polind_xprime(x_c,d_c,z_c));
            pol_dprime(x_c,d_c,z_c) = d_grid_fine(polind_dprime(x_c,d_c,z_c));
            value(x_c,d_c,z_c)      = V(x_c,d_c,z_c);
            pol_aprime(x_c,d_c,z_c) = func.xd2a(pol_xprime(x_c,d_c,z_c),pol_dprime(x_c,d_c,z_c),par);
            [~,pol_c(x_c,d_c,z_c)] = func.ReturnFn(pol_xprime(x_c,d_c,z_c),pol_dprime(x_c,d_c,z_c),x_grid(x_c),d_grid(d_c),z_grid(z_c),par);
        end
    end
end

% Sanity checks
disp("VFI diagnostics: make sure grid is not too restrictive")
fprintf('Lower bound wealth grid x = %f \n',par.x_grid(1))
fprintf('Min of wealth policy x''  = %f \n',min(pol_xprime(:)))
fprintf('Upper bound wealth grid x = %f \n',par.x_grid(end))
fprintf('Max of wealth policy x''  = %f \n',max(pol_xprime(:)))
fprintf(' \n')

% Pack value and policy functions into the structure sol:
sol.value = value;
sol.pol_xprime = pol_xprime;
sol.pol_dprime = pol_dprime;
sol.pol_c      = pol_c;
sol.pol_aprime = pol_aprime;

end

