// Endogenous variables: attached l,b are for l=lender, b=borrower.
// In the paper, lender's variables have a tilde

var Cl      $\widetilde{c}$ (long_name='consumption lenders')
    Cb      $c$ (long_name='consumption borrowers')
    Ll      $\widetilde{l}$ (long_name='land lenders')
    Lb      $l$ (long_name='land borrowers')
    lambdal $\widetilde{\lambda}$ (long_name='MU cons. lenders')
    lambdab $\lambda$ (long_name='MU cons. borrowers')
    B       $b$ (long_name='borrowing')
    y       $y$ (long_name='output')
    a       $a$ (long_name='TFP shock')
    k       $k$ (long_name='capital')
    q       $q$ (long_name='land price')
	C       $C_{agg}$ (long_name='aggregate consumption')
	r       $r$ (long_name='interest rate')
    fin_shock     $\theta$ (long_name='loan-to-collateral ratio');
	

varexo e u;


parameters alpha beta beta_tilde delta theta rho rho_a rho_theta 
gamma sigma_l k_ss sigma_w sigma_b y_ss Cbss Clss q_ss Lbss Llss 
Bss r_ss Css;

load param_pintus;
set_param_value('theta',theta)
set_param_value('alpha',alpha)
set_param_value('beta',beta)
set_param_value('beta_tilde',beta_tilde)
set_param_value('delta',delta)
set_param_value('rho',rho)
set_param_value('rho_a',rho_a)
set_param_value('rho_theta',rho_theta)
set_param_value('gamma',gamma)
set_param_value('sigma_l',sigma_l)
set_param_value('sigma_w',sigma_w)
set_param_value('sigma_b',sigma_b)
set_param_value('y_ss',y_ss)
set_param_value('q_ss',q_ss)
set_param_value('Cbss',Cbss)
set_param_value('Clss',Clss)
set_param_value('Llss',Llss)
set_param_value('Lbss',Lbss)
set_param_value('Bss',Bss)
set_param_value('r_ss',r_ss)
set_param_value('k_ss',k_ss)
set_param_value('Css',Css)



model(linear);
// First number refers to eq in the main paper, second number to eq in Appendix A 
// (1,35A) Budget constraint of lender
(Clss/Bss)*Cl+(q_ss*Llss/Bss)*(Ll-Ll(-1))+B=
(1+r_ss)*(B(-1)+lambdal(-1)-lambdal);

// (3,36A) Production function
y = a+alpha*k(-1)+gamma*Lb(-1);

// AR1 for TFP shock
a = rho_a*a(-1)+e;

// AR1 for LTC shock
fin_shock = rho_theta*fin_shock(-1)+u;

// (4,37A) Market clearing for land
Ll = -Lb;

// (5,38A) Budget constraint of borrower 
(Cbss/y_ss)*Cb+(k_ss/y_ss)*k-(1-delta)*(k_ss/y_ss)*k(-1)+(q_ss*Lbss/y_ss)*(Lb-Lb(-1))+
((1+r_ss)*Bss/y_ss)*(B(-1)+lambdal(-1)-lambdal)=
(Bss/y_ss)*B+y;

// (7,39A) Borrowing constraint, assumed to be binding
lambdal-lambdal(+1)+B = fin_shock+(q(+1))+Lb;

// (13,40A) MU consumption for the lender
-sigma_l*Cl=lambdal;

// (14,41A) FOC land, lender's maximization
q+lambdal = beta_tilde*(q(+1)+lambdal(+1))-(1-beta_tilde)*sigma_w*Ll; 
                           
// (16,42A) FOC habit consumption, borrower's maximization 
sigma_b*(Cb-rho*Cb(-1))=-(1-rho)*lambdab;

// (17,43A) FOC for land investment, borrower's maximization 
q+(1-theta*beta_tilde)*lambdab = beta*(1-theta)*(q(+1)+lambdab(+1))+
                               ((beta*gamma*y_ss)/(q_ss*Lbss))*(lambdab(+1)+y(+1)-Lb)+
                               theta*beta_tilde*(q(+1)+lambdal(+1)-lambdal);

// (18,44A) FOC for capital investment, borrower's maximization 
lambdab = beta*(1-delta)*(lambdab(+1))+(alpha*beta*y_ss/k_ss)*(lambdab(+1)+y(+1)-k);

// Aggregate consumption, not present in Appendix A
C = (Clss/Css)*Cl+(Cbss/Css)*Cb;

// (15, not in Appendix A) FOC lender, Interest rate
r = lambdal-(lambdal(+1));


end;

shocks;
// It seems that P&W set the stdev to one in Figure 1 
var e; stderr 0.01;
var u; stderr 0.01;
end; 

stoch_simul (linear,nocorr,nomoments,nograph,irf=80);




