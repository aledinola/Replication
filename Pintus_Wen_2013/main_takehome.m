% Takehome exam - DSGE course Uni Konstanz 2019
% Replication of Pintus and Wen (2013)
% This file replicates    
% Figure 1 in Pintus and Wen (2013). Set lender = 'RN' if you want to   
% simulate the model with risk neutral lender, while 'RA' if the       
% risk averse (with log-utility) case is desired. Set 1 in calib for 
% calibration 1, while 2 for calibration 2.    

%% Housekeeping

clear
clc
close all

%% Calibration

% See Table 1 in Pintus and Wen (2012). Notation: attached l,b are for l=lender, b=borrower.
% In the paper, lender's variables have a tilde
% calibration 1: TFP shocks are iid, i.e. rho_a=0
% calibration 2: TFP shocks are persistent, i.e. rho_a>0

%------------ FLAGS -----------------------------------------------------%
lender  = 'RA'; % RN or RA
calib   = 2; %either 1 or 2

theta      = 1.0;   %steady-state of LTC ratio
beta_tilde = 0.99;  %discount factor lenders (implies 4% annual interest rate)
delta      = 0.025; %capital depreciation rate
alpha      = 0.35;  %capital income share
gamma      = 0.05;  %land share
rho        = 0.9;   %consumption habit parameter
rho_theta  = 0.9;   %persistence of loan-to-collateral shock
b          = 100/99;  %so that L_tilde/L=1, immaterial
L_fixed    = 10;     %total supply of land (fixed)

switch lender
    case 'RN'
        %Risk Neutral lender
        sigma_l    = 0; %risk aversion lender
        sigma_w    = 0; %risk aversion lender
        if calib == 1
            sigma_b = 4;
            beta    = 0.5; %discount factor borrowers
            rho_a   = 0;   %persistence TFP shock
        elseif calib == 2
            sigma_b = 2;
            beta    = 0.8; %discount factor borrowers
            rho_a   = 0.9; %persistence TFP shock
            
        end
    case 'RA'
        %Risk Averse lender
        sigma_l    = 1; %risk aversion lender
        sigma_w    = 1; %risk aversion lender
        if calib == 1
            sigma_b = 4;
            beta    = 0.5; %discount factor borrowers
            rho_a   = 0;   %persistence TFP shock
        elseif calib == 2
            sigma_b = 2; % probably they forgot and left it at 2!!
            beta    = 0.8; %discount factor borrowers
            rho_a   = 0.9; %persistence TFP shock
        end
end

%% Steady state

r_ss            = (1/beta_tilde)-1;
ky_ratio        = alpha*beta/(1-beta*(1-delta));
Lbss            = L_fixed/2;
Llss            = L_fixed/2;
k_ss            = (ky_ratio*Lbss^gamma)^(1/(1-alpha));
y_ss            = k_ss^alpha*Lbss^gamma;
q_ss            = (1/(1-beta_tilde))*beta*gamma*(y_ss/Lbss);
Bss             = beta_tilde*q_ss*Lbss;
Clss            = beta*gamma*y_ss;
%c_tilde_ss1    = (1-beta_tilde)*q_ss*Lbss;
Cbss            = y_ss-delta*k_ss-beta*gamma*y_ss;
Css             = Cbss+Clss;
lambda_tilde_ss = Clss^(-sigma_l);
lambda_ss       = (Cbss*(1-rho))^(-sigma_b);
phi_ss          = (beta_tilde-beta)*lambda_ss;

disp('++++++++++++++++++++++++++')
fprintf('SS ratio of land b/w two agents: %5.4f \n',Llss/Lbss);
if abs(Llss/Lbss-1)>1e-5
    error('Wrong calibration')
end
disp('++++++++++++++++++++++++++')

%% Check steady-state equations

% (1,35A) Budget constraint of lender
eq1 = Clss+Bss-((1+r_ss)*Bss)

% (3,36A) Production function
eq3 = y_ss -(k_ss^alpha*Lbss^gamma)

% (4,37A) Market clearing for land
eq4 = L_fixed - Lbss-Llss

% (5,38A) Budget constraint of borrower
eq5 = Cbss+delta*k_ss+(1+r_ss)*Bss-(Bss+y_ss)

% eq7
eq7 = (1+r_ss)*Bss- (q_ss*Lbss)

eq16 = (Cbss-rho*Cbss)^(-sigma_b)-lambda_ss
eq17 = q_ss*lambda_ss -(beta*q_ss*lambda_ss+beta*gamma*y_ss*lambda_ss/Lbss +phi_ss*q_ss )

eq18 = lambda_ss-(beta*lambda_ss*(alpha*y_ss/k_ss+1-delta))

eq19 = lambda_ss-(beta*(1+r_ss)*lambda_ss+phi_ss*(1+r_ss))

%% Call Dynare and solve the model

save param_pintus theta beta_tilde delta alpha gamma rho sigma_l sigma_w...
    sigma_b beta rho_a b y_ss k_ss  Clss Cbss q_ss lambda_ss...
    lambda_tilde_ss Llss Lbss L_fixed r_ss Bss Css rho_theta

dynare pintus_wen.mod noclearall  % running dynare

%% Plot IRFS to TFP shock - FIGURE 1

Tirf = 80;
time = 1:1:Tirf;
fileExt = '-dpng';
%fileExt = '-depsc';

switch lender
    
    %% Risk Neutral Lender
    
    case 'RN'
        if calib==1 %iid TFP shock
            figure(1)
            plot(time,y_e,'-','color','red','LineWidth',2)
            hold on
            plot(time,C_e,'-.','color','c','LineWidth',2)
            hold on
            plot(time,Lb_e,'--','color','green','LineWidth',2)
            hold on
            plot(time,k_e,':','color','m','LineWidth',2)
            hold on
            plot(time,zeros(1,Tirf),'-','color','b','LineWidth',1)
            
            legend('Output','Agg.Cons.','Land','Capital','Steady-State')
            title('Risk Neutral Lender, one-period TFP shock')
            print('Fig1_left_top',fileExt)
            
        elseif calib==2
            figure(1)
            plot(time,y_e,'-','color','red','LineWidth',2)
            hold on
            plot(time,C_e,'-.','color','c','LineWidth',2)
            hold on
            plot(time,Lb_e,'--','color','green','LineWidth',2)
            hold on
            plot(time,k_e,':','color','m','LineWidth',2)
            hold on
            plot(time,zeros(1,Tirf),'-','color','b','LineWidth',1)
            
            legend('Output','Agg.Cons.','Land','Capital','Steady-State')
            title('Risk Neutral Lender, persistent TFP shock')
            print('Fig1_left_bottom',fileExt)
            
        end
        
        %% Risk Averse Lender
    case 'RA'
        if calib==1 %iid TFP shock
            figure(1)
            plot(time,y_e,'-','color','red','LineWidth',2)
            hold on
            plot(time,C_e,'-.','color','c','LineWidth',2)
            hold on
            plot(time,q_e,'--','color','green','LineWidth',2)
            hold on
            plot(time,r_e,':','color','m','LineWidth',2)
            hold on
            plot(time,zeros(1,Tirf),'-','color','b','LineWidth',1)
            
            legend('Output','Agg.Cons.','Land Price','Interest Rate','Steady-State')
            title('Risk Averse Lender, one-period TFP shock')
            print('Fig1_right_top',fileExt)
            
        elseif calib==2
            figure(1)
            plot(time,y_e,'-','color','red','LineWidth',2)
            hold on
            plot(time,C_e,'-.','color','c','LineWidth',2)
            hold on
            plot(time,q_e,'--','color','green','LineWidth',2)
            hold on
            plot(time,r_e,':','color','m','LineWidth',2)
            hold on
            plot(time,zeros(1,Tirf),'-','color','b','LineWidth',1)
            
            legend('Output','Agg.Cons.','Land Price','Interest Rate','Steady-State')
            title('Risk Averse Lender, persistent TFP shock')
            print('Fig1_right_bottom',fileExt)
            
        end
        
        
        
end



%gonzo('pintus_wen')



