function [par] = set_parameters()

% Set parameter values and collect them into the struct <par>

par = struct();

% Flags for display on screen and plot
% ====================
par.verbose    = 2; %flag 0/1/2 for display on screen
par.doPlots    = 1; %flag 0/1 to show plots
par.doSave     = 0; %flag 0/1 to save plots as PNG


% Algorithm parameters
% ====================
par.lowMem      = 1;    % flag 0/1: if 1, no precomputation of big return array
par.par_fortran = 1;     % flag 0/1 to activate OpenMP in Fortran
par.maxit_val = 5000;  % maximum  number of iterations for VF
par.tol_val   = 1e-6;  % convergence criterion for VF
par.mqpbounds = 0;
par.howard    = 1;
par.n_howard  = 30;
par.do_monot      = 1; %Exploit monotonicity of x' wrt x, conditional on d'
par.do_refinement = 1; %Activate golden search over x'

% Model parameters (calibration without adjustment costs)
% ================
par.r = 0.03;         % interest rate on savings

par.delta = 0.02;     % depreciation rate durable good

par.beta = 0.9391;    % discount factor 

par.sigma = 2;        % overall risk aversion

par.theta = 0.8092;     % Cobb-Douglas weight on non-durable consumption

par.epscon = 0.0;      % autonomous non-durable consumption

par.epsdur = 0.000001; % autonomous durable consumption

par.miu = 0.97;        % loan-to-value ratio

par.gamma = 0.95;        % seizable fraction of minimum income

par.alpha  = 0; % adjustment cost parameter

par.rhoAR = 0.95;      % autocorrelation of income

par.nz = 5;            % number of markov states

end

