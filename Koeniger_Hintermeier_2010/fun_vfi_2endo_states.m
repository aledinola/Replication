function [V,polind_xprime,polind_dprime] = fun_vfi_2endo_states(discfactor,nz,z_grid,z_prob,x_grid,d_grid,x_grid_fine,d_grid_fine,vfi_options,par,dbg)

% DESCRIPTION:
%	Solve DP problem via discrete value function iteration. DP problem is
%	defined as follows:
%   V(x,d,z) = \max_{x',d'} {R(x',d',x,d,z)+discfactor*E[V(x',d',z')|z]}
% IMPORTANT: It does not require to compute in advance the big array R.
% INPUTS:
%   "discfactor"  discount factor \in [0,1)
%   "nx,nd"       no. grid points  for endogenous state variables (x,d)
%   "nz"          no. grid points grid for exogenous state variable z
%   "nx_fine,nd_fine"  no. grid points  for x',d'
%   "z_prob"      Markov chain for exgenous shock z
%   "vfi_options" Structure with several options
% OUTPUTS:
%   "V,polind,pol"         Structure with value and policy functions
% EXAMPLE:
% vfi_options.maxit_val = 1000;
% vfi_options.tol_val   = 1e-6;
% vfi_options.verbose   = 1;
% vfi_options.mqpbounds = 0;
% vfi_options.howard    = 1;
% vfi_options.n_howard  = 50;

% Input checks

if dbg
    validateattributes(z_prob, {'double'}, {'finite', 'nonnan', 'nonempty', 'real', '>=', 0, '<=', 1, ...
        'size', [nz, nz]})
    % Make sure probabilities sum to one
    prSumV = sum(z_prob,2);
    if max(abs( prSumV - 1 )) > 1e-5
        error('Probabilities do not sum to one');
    end
    if ~isstruct(vfi_options)
        error('vfi_options must be a structure')
    end
end

%sol = struct();

beeta = discfactor;
nx    = length(x_grid);
nd    = length(d_grid);

%nx_fine    = length(x_grid_fine);
%nd_fine    = length(d_grid_fine);

% For interpolation

% Unpack options
maxit     = vfi_options.maxit_val;  % maximum  number of iterations for VF
tol       = vfi_options.tol_val;
verbose   = vfi_options.verbose;
mqpbounds = vfi_options.mqpbounds;
howard    = vfi_options.howard;
n_howard  = vfi_options.n_howard;

if howard==1 && mqpbounds==1
    error('Cannot use both MQP bounds and Howard')
end

% Set initial guess for V
V    = zeros(nx,nd,nz);
Vnew = zeros(nx,nd,nz);

% Policy function, indeces
polind_xprime = ones(nx,nd,nz);
polind_dprime = ones(nx,nd,nz);

iter = 1;
err  = tol+1;

% Grids for interpolation
%[x2,d2]           = ndgrid(x_grid,d_grid);
%[x2_fine,d2_fine] = ndgrid(x_grid_fine,d_grid_fine);

[x2,d2]           = meshgrid(d_grid,x_grid);
[x2_fine,d2_fine] = meshgrid(d_grid_fine,x_grid_fine);

while err>tol && iter<=maxit
    
    for z_c = 1:nz
        % Compute expected value EV(x',d',z)
        EV = zeros(nx,nd);
        for zp_c = 1:nz
            EV = EV+V(:,:,zp_c)*z_prob(z_c,zp_c); 
        end
        % this is dim: (nx_fine,nd_fine)
        %interp = griddedInterpolant(x2,d2,EV);
        %EV_interp = interp(x2_fine,d2_fine);
        %ZI = INTERP2(X,Y,Z,XI,YI)
        EV_interp = interp2(x2,d2,EV,x2_fine,d2_fine);
        
        % Maximize the RHS of the Bellman equation for each pair of states
        % (x,d)
        for d_c = 1:nd
            for x_c = 1:nx
                Rmat = func.ReturnFnMat(x_grid_fine,d_grid_fine',x_grid(x_c),d_grid(d_c),z_grid(z_c),par); %dim: (nx',nd')
                RHSmat = Rmat+beeta*EV_interp; %dim: (nx',nd')
                [indx,indd] = argmax2(RHSmat);
                Vnew(x_c,d_c,z_c) = RHSmat(indx,indd);
                polind_xprime(x_c,d_c,z_c) = indx;
                polind_dprime(x_c,d_c,z_c) = indd;
            end
        end
        
    end % end loop exo state
    
    %-------------- Howard ----------------------------%
    if howard==1
        payoff_max = zeros(nx,nd,nz);
        for z_c = 1:nz
            for d_c = 1:nd
                for x_c = 1:nx
                    x_opt = polind_xprime(x_c,d_c,z_c);
                    d_opt = polind_dprime(x_c,d_c,z_c);
                    payoff_max(x_c,d_c,z_c) = func.ReturnFn(x_grid_fine(x_opt),d_grid_fine(d_opt),x_grid(x_c),d_grid(d_c),z_grid(z_c),par);
                end
            end
        end
        
        for h_c = 1:n_howard % Howard iteration counter
            for z_c = 1:nz
                % Compute expected value EV(x',d',z)
                EVh = zeros(nx,nd);
                for zp_c = 1:nz
                    EVh = EVh+Vnew(:,:,zp_c)*z_prob(z_c,zp_c); 
                end
                % this is dim: (nx_fine,nd_fine)
                %interp = griddedInterpolant(x2,d2,EVh);
                EVh_interp = interp2(x2,d2,EVh,x2_fine,d2_fine);
                for d_c = 1:nd
                    for x_c = 1:nx
                        x_opt = polind_xprime(x_c,d_c,z_c);
                        d_opt = polind_dprime(x_c,d_c,z_c);
                        Vnew(x_c,d_c,z_c) = payoff_max(x_c,d_c,z_c)+beeta*EVh_interp(x_opt,d_opt);
                    end
                end
            end
        end
        
    end
    %-------------- End Howard ----------------------------%
    
    err = max(abs(V(:)-Vnew(:)));
    
    if verbose>=2
        fprintf('iter = %d, err = %f \n', iter,err);
    end
    
    % Update
    V = Vnew;
    
%     % MacQueenPorteus bounds!!
%     if mqpbounds==1
%         adjfactor  = 0.5*( (beeta/(1-beeta)) * min(Vnew(:)-V(:)) + (beeta/(1-beeta)) * max(Vnew(:)-V(:)));
%         V = Vnew + adjfactor;
%     else
%         V = Vnew;
%     end
       
    iter = iter+1;
end %end while



% Pack outputs
% sol.V      = V;
% sol.polind = polind;
% sol.pol    = pol;

end

