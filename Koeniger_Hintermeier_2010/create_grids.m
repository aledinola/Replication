function [par] = create_grids(par)

par.z_grid = [0.0896477, 0.3848221, 0.7365505, 1.218352, 2.573891]';
par.z_prob =  [0.985381997117748        0.0146180000098449      2.87240442542469e-09      2.66453525910038e-15                         0
        0.0045433184195862         0.845111675826674         0.149117535555026       0.00122736702502513      1.03173688703606e-07
      1.05391444594555e-06         0.135867860470339         0.678688523965513         0.184334037021559       0.00110852462814337
        7.914643120749e-11       0.00294873263921671          0.22081530786779         0.696246633234202        0.0799893261796446
       4.1578419135699e-19      9.34216142109799e-08      0.000645150874309866         0.145433451493471         0.853921304210605];


if par.nz~=length(par.z_grid)
    error("Grid for the shock must have nz elements")
end
   
% Create the grid on the state space
% ==================================
% state space: x, d, markov_state z
par.y_gam = par.gamma*min(par.z_grid);  % seizable income

%% Grid(s) for total wealth x
% x is an endogenous state variable, x = (1 + r)*a + (1 - delta_)*d
x_add       = 0.000001;
x_min       = -par.y_gam + x_add;
x_max       = 20; %!!!!!
par.nx      = 100;        % small grid dimension for current state x

%% Grid(s) for durable holdings d
% d is durable holdings
d_min       = 0.01; % minimum housing constraint d'>=d_min
d_max       = 10;
par.nd      = 50;       % small grid dimension for current state d

par.x_grid = make_grid(x_min,x_max,par.nx,NaN,3);  % set up single exponential grid
par.d_grid = make_grid(d_min,d_max,par.nd,NaN,3);  % set up single exponential grid

% Combined grid s=(x,d)
par.ns = par.nx*par.nd;

end

