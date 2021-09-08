%=========================================================================%
%{ 
Matlab program to solve a model with durable consumption (e.g. housing) 
and collateralized borrowing with value function iteration 
Main reference: https://ideas.repec.org/c/dge/qmrbcd/184.html
 
Model (P1):
 V(a,d,y) = max_{c,a',d'} {u(c,d) + beta*E[V(a',d',y')|y] }
 s.t.
 c+a'+d'+adj(d',d) = (1+r)a+(1-delta)d+y (BC1)
 (1+r)a'>= -gamma*min(y)-mu(1-delta)d' (CC1)
 d'>=d_min,
where a is financial assets, d is durable good and y is the income shock.
Change of variable: let x=(1+r)a+(1-delta)d be total wealth (or
deterministic part of cash at hand). Then (P1) is equivalent to

Model (P2):
 V(x,d,y) = max_{c,a',d'} {u(c,d) + beta*E[V(x',d',y')|y] }
 s.t.
 c+a'+d'+adj(d',d) = x+y (BC2)
 x'=(1+r)a'+(1-delta)d' (DEF) 
 (1+r)a'>= -gamma*min(y)-mu(1-delta)d' (CC2)
 d'>=d_min,
By susbtituting (DEF) into (BC2) we can eliminate a' from the problem and
we get (P3):

Model (P3):
 V(x,d,y) = max_{x',d'} {u(c,d) + beta*E[V(x',d',y')|y] }
 s.t.
 c = x+y-((r+delta)/(1+r))d'-adj(d',d)-x'/(1+r) (BC3)
 x'>= -gamma*min(y)+(1-mu)(1-delta)d' (CC3)
 d'>=d_min

STATE SPACE: (x,d,y), where x and d are the two endogenous states
CHOICES: (x',d')
For the numerical solution we use (P3).
SOLUTION METHOD: Define states (x,d) on a coarse grid and the controls
(x',d') on a much finer grid. Then use bilinear interpolation to evaluate
V(x,d), since V(x,d) is defined on the coarse grid.
Only low memory option is used since precomputing R(x',d',x,d,z) is too
large

BUGS: If I select par_fortran=1 and do_refinment=1 I get an error in
Fortran

% Alessandro Di Nola, September 2021
%}
% ===============================================

clear;clc;close all
format long g

addpath(fullfile('..','..','..','5 - NUMERICAL METHODS','1 - toolbox_matlab'));

%% Set parameters

method = 'fortran'; % 'matlab' OR 'fortran'
DirExe = 'exe'; % folder where fortran exe is stored
main_folder = pwd;

[par] = set_parameters();

% Check restrictions on input parameters
if par.sigma <= 1
   error('This program is built for sigma > 1.')
end

%% Create grids

[par] = create_grids(par);

fprintf(" \n")
fprintf("Discretization of the state space: \n")
fprintf("No. of grid points for x = %d \n",par.nx)
fprintf("No. of grid points for d = %d \n",par.nd)
fprintf("No. of grid points for y = %d \n",par.nz)
fprintf(" \n")

%% Value function iteration

switch method
    case 'matlab'
        sol = solve_model(par);
    case 'fortran'
        sol = solve_model_fortran(par,DirExe,main_folder);
    otherwise
        error('Selected method does not exist')
end

%% Plot results

% Unpack structure SOL into:
% V1,pol_xprime,pol_dprime,pol_aprime
value      = sol.value;
pol_xprime = sol.pol_xprime;
pol_dprime = sol.pol_dprime;
pol_c      = sol.pol_c;
pol_aprime = sol.pol_aprime;

save temp

if par.doPlots==1
    % Intermediate value of durable stock
    %dd = round(par.nd/2);
    
    % Plots paper
    % Restrict x<=5 and pick d=0.05

    x_lim  = find(par.x_grid<=5, 1, 'last' );
    [~,dd] = min(abs(par.d_grid-0.05));
    
    figure
    [x2,d2] = meshgrid(par.d_grid,par.x_grid);
    surf(x2,d2,value(:,:,3))
    
    figure
    plot(par.x_grid,value(:,dd,1),'linewidth',2)
    hold on
    plot(par.x_grid,value(:,dd,2),'linewidth',2)
    hold on
    plot(par.x_grid,value(:,dd,3),'linewidth',2) 
    hold on
    plot(par.x_grid,value(:,dd,4),'linewidth',2)
    hold on
    plot(par.x_grid,value(:,dd,par.nz),'linewidth',2)
    legend("Low income","","","","High income")
    xlabel('Total wealth')
    title("Value function")
    axis tight
    hold off
    if par.doSave==1; print('fig1','-dpng'); end
    
    figure
    plot(par.x_grid,par.x_grid,'--','linewidth',2)
    hold on
    plot(par.x_grid,pol_xprime(:,dd,1),'linewidth',2)
    hold on
    plot(par.x_grid,pol_xprime(:,dd,par.nz),'linewidth',2)
    legend("45 line","Low income","High income","location","best")
    xlabel('Total wealth x')
    ylabel('Total wealth x^{\prime}')
    title("Policy for wealth x'(x,d,z), medium d")
    axis tight
    hold off
    if par.doSave==1; print('fig2','-dpng'); end
    
    figure
    plot(par.x_grid,par.x_grid,'--','linewidth',2)
    hold on
    plot(par.x_grid,pol_xprime(:,end,1),'linewidth',2)
    hold on
    plot(par.x_grid,pol_xprime(:,end,par.nz),'linewidth',2)
    legend("45 line","Low income","High income","location","best")
    xlabel('Total wealth x')
    ylabel('Total wealth x^{\prime}')
    title("Policy for wealth x'(x,d,z), highest d")
    axis tight
    hold off
    if par.doSave==1; print('fig2','-dpng'); end
    
%     figure
%     plot(par.x_grid,pol_aprime(:,dd,1),'linewidth',2)
%     hold on
%     plot(par.x_grid,pol_aprime(:,dd,par.nz),'linewidth',2)
%     legend("Low income","High income","location","best")
%     xlabel('Total wealth x')
%     ylabel('Financial wealth a^{\prime}')
%     title("Policy for financial wealth")
%     axis tight
%     hold off
%     if par.doSave==1; print('fig3','-dpng'); end
    
    figure
    plot(par.x_grid,pol_dprime(:,dd,1),'linewidth',2)
    hold on
    plot(par.x_grid,pol_dprime(:,dd,par.nz),'linewidth',2)
    legend("Low income","High income","location","best")
    xlabel('Total wealth x')
    ylabel('Durable stock d^{\prime}')
    title("Policy for durable")
    axis tight
    hold off
    if par.doSave==1; print('fig4','-dpng'); end
    
    figure
    plot(par.x_grid,pol_c(:,dd,1),'linewidth',2)
    hold on
    plot(par.x_grid,pol_c(:,dd,par.nz),'linewidth',2)
    legend("Low income","High income","location","best")
    xlabel('Total wealth x')
    ylabel('Consumption c')
    title("Policy for non-durable consumption")
    axis tight
    hold off
    if par.doSave==1; print('fig_c','-dpng'); end
    
%     figure
%     plot(par.x_grid,pol_c(:,dd,1),'linewidth',2)
%     hold on
%     plot(par.x_grid,pol_c(:,dd,par.nz),'linewidth',2)
%     legend("Low income","High income","location","best")
%     xlabel('Total wealth x')
%     ylabel('Non-durable consumption c')
%     title("Policy for Non-durable consumption")
%     axis tight
%     hold off
%     if par.doSave==1; print('fig5','-dpng'); end



figure
subplot(3,1,1)
    plot(par.x_grid(1:x_lim),pol_aprime(1:x_lim,dd,1),'linewidth',2)
    hold on
    plot(par.x_grid(1:x_lim),pol_aprime(1:x_lim,dd,2),'linewidth',2)
    hold on
    plot(par.x_grid(1:x_lim),pol_aprime(1:x_lim,dd,3),'linewidth',2)
    hold on
    plot(par.x_grid(1:x_lim),pol_aprime(1:x_lim,dd,4),'linewidth',2)
    hold on
    plot(par.x_grid(1:x_lim),pol_aprime(1:x_lim,dd,5),'linewidth',2)
    hold on
    xlim([par.x_grid(1),par.x_grid(x_lim)])
    legend('zmin','','','','zmax','location','best')
    xlabel('Total wealth x')
    ylabel('Financial wealth a''')
    hold off
    
subplot(3,1,2)
    plot(par.x_grid(1:x_lim),pol_dprime(1:x_lim,dd,1),'linewidth',2)
    hold on
    plot(par.x_grid(1:x_lim),pol_dprime(1:x_lim,dd,2),'linewidth',2)
    hold on
    plot(par.x_grid(1:x_lim),pol_dprime(1:x_lim,dd,3),'linewidth',2)
    hold on
    plot(par.x_grid(1:x_lim),pol_dprime(1:x_lim,dd,4),'linewidth',2)
    hold on
    plot(par.x_grid(1:x_lim),pol_dprime(1:x_lim,dd,5),'linewidth',2)
    hold on
    xlim([par.x_grid(1),par.x_grid(x_lim)])
    legend('zmin','','','','zmax')
    xlabel('Total wealth x')
    ylabel('Future durable d''')
    hold off
subplot(3,1,3)
    plot(par.x_grid(1:x_lim),pol_c(1:x_lim,dd,1),'linewidth',2)
    hold on
    plot(par.x_grid(1:x_lim),pol_c(1:x_lim,dd,2),'linewidth',2)
    hold on
    plot(par.x_grid(1:x_lim),pol_c(1:x_lim,dd,3),'linewidth',2)
    hold on
    plot(par.x_grid(1:x_lim),pol_c(1:x_lim,dd,4),'linewidth',2)
    hold on
    plot(par.x_grid(1:x_lim),pol_c(1:x_lim,dd,5),'linewidth',2)
    hold on
    xlim([par.x_grid(1),par.x_grid(x_lim)])
    legend('zmin','','','','zmax')
    xlabel('Total wealth x')
    ylabel('Non-durable consumption c')
    hold off
 if par.doSave==1; print('fig2_paperHK','-dpng'); end
 
 
end


