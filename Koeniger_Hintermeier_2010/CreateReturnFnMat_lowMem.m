function [Rvec] = CreateReturnFnMat_lowMem(s_c,z_c,x_grid,d_grid,z_grid,par)

% DESCRIPTION:
%	% Static return vector conditional on current values of s=(x,d) and z
nx = length(x_grid);
nd = length(d_grid);

% Combine all endogenous states (x,d) into s:
ns = nx*nd;

%Rmat2 = zeros(nx,nd); % dim: (x',d')
xp = x_grid; %Next-period x', dim: (nx,1)
dp = d_grid'; %Next-period d', dim: (1,nd)

[current_x,current_d] = ind2sub([nx,nd],s_c);
x_val = x_grid(current_x);
d_val = d_grid(current_d);
Rmat2 = func.ReturnFnMat(xp,dp,x_val,d_val,z_grid(z_c),par);
        
% Reshape R(x',d') ==> R(s'), where s = (x,d) stacked columnwise
Rvec = reshape(Rmat2,ns,1);


end

