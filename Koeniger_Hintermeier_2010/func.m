classdef func
    %This class contains all functions related to the model (functional
    %forms, budget constraint, collateral constraint, etc.
    
    % NOTATION
    % x,d: current period cash on hand and durable asset
    % xp,dp: next-period cash on hand and durable asset
    % z: labor productivity shock
    
    methods (Static)
        
        function [F,cons] = ReturnFn(xp,dp,x,d,z,par)
            % Gives current payoff
            F = -realmax;
            cons = x+z-((par.r+par.delta)/(1+par.r))*dp-func.adjcost(dp,d,par)-xp/(1+par.r);
            % Non-negative consumption is imposed in func.util
            if xp>=func.borcon(dp,par)
                F = func.util(cons,d,par);
            end
            
        end
        
        function [F,cons] = ReturnFnVec(xp,dp,x,d,z,par)
            % Same as ReturnFn, but vectorized over xp
            F = -realmax*ones(length(xp),1);
            cons = x+z-((par.r+par.delta)/(1+par.r))*dp-func.adjcost(dp,d,par)-xp/(1+par.r);
            mask = xp>=func.borcon(dp,par);
            F(mask) = func.util(cons(mask),d,par);
            
        end
        
        function [F,cons] = ReturnFnMat(xp,dp,x,d,z,par)
            % Same as ReturnFn, but vectorized over xp AND dp
            nrows = size(xp,1);
            ncols = size(dp,2);
            F = -realmax*ones(nrows,ncols);
            %cons = x+z-((par.r+par.delta)/(1+par.r))*dp-func.adjcost(dp,d,par)-xp/(1+par.r);
            cons = x+z-((par.r+par.delta)/(1+par.r))*dp-xp/(1+par.r);
            mask = xp>=func.borcon(dp,par);
            F(mask) = func.util(cons(mask),d,par);
            
        end
        
        function U = util(c,d,par)
            % Instantaneous utility function
            % Arguments: non-durable and durable consumption
            c_lim = max(c,1e-10);
            agg   = c_lim.^par.theta.*(d+par.epsdur).^(1.0-par.theta);
            U     = (agg.^(1-par.sigma)-1)/(1-par.sigma);
        end
        function F = adjcost(dp,d,par)
            % Adjustment costs
            F = (par.alpha/2)*((dp-(1-par.delta)*d)/d).^2*d;
        end
        function F = borcon(dp,par)
            % Collateral constraint
            F = -par.y_gam+(1-par.miu)*(1-par.delta)*dp;
        end
        
        function F = xd2a(x,d,par)
            % Financial assets a as a function of x and d
            F = x/(1+par.r)-((1-par.delta)/(1+par.r))*d;
        end
        
       
    end %METHODS
end %CLASS

