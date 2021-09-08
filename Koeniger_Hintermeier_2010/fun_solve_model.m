function [sol] = fun_solve_model(par,vfi_options)
% This function solves the model
% INPUTS:
%  "par" Structure with model parameter and grids
% OUTPUTS:
%  "sol" Structure with model solution: value and policy functions

% Precompute big payoff matrix F(x',x,z)
Rmat = fun_create_Rmat(par);

% Solve V(x,z) = max {F(x',x,z)+beta*EV(x',z)}
sol = fun_vfi(Rmat,x_grid,z_grid,z_prob,vfi_options);

end

