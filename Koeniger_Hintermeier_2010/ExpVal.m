function [EV_interp] = ExpVal(x_grid,d_grid,EV,x_grid_fine,d_grid_fine,nx_fine,nd_fine)

EV_interp = zeros(nx_fine,nd_fine);
for dp_c = 1:nd_fine
    for xp_c = 1:nx_fine
        EV_interp(xp_c,dp_c) = mybilinear(x_grid,d_grid,EV,x_grid_fine(xp_c),d_grid_fine(dp_c));
    end
end


end

