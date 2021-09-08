clear
clc
close all

disp("Test routines for 2D linear interpolation")
% [z] = mybilinear(xvec,yvec,zmat,x,y)


xvec = linspace(-5,5,13)';
yvec = linspace(-5,5,13)';
zmat = zeros(length(xvec),length(yvec));
for iy=1:length(yvec)
    for ix=1:length(xvec)
        zmat(ix,iy) = myfun(xvec(ix),yvec(iy));
    end
end
        
figure
surf(xvec,yvec,zmat)


xq = linspace(-5,5,101)';
yq = linspace(-5,5,101)';

zq = zeros(length(xq),length(yq));
for iy=1:length(yq)
    for ix=1:length(xq)
        zq(ix,iy) = mybilinear(xvec,yvec,zmat,xq(ix),yq(iy));
    end
end

figure
surf(xq,yq,zq)

function F = myfun(x,y)

F = sin(x^2 + y^2) / (x^2 + y^2);

end

% n = 10;
% n_fine = 100;
% 
% x_min = -2; x_max = 2;
% y_min = -1; y_max = 1;
% 
% x_grid = linspace(x_min,x_max,n)';
% y_grid = linspace(y_min,y_max,n)';
% 
% x_grid_fine = linspace(x_min,x_max,n_fine)';
% y_grid_fine = linspace(y_min,y_max,n_fine)';
% 
% % Mesh grids
% [x_grid2,y_grid2] = meshgrid(x_grid,y_grid);
% [x_grid_fine2,y_grid_fine2] = meshgrid(x_grid_fine,y_grid_fine);
% 
% % Plot original data
% data = myfun(x_grid,y_grid');
% figure
% surf(x_grid,y_grid,data)
% 
% data = myfun(x_grid2,y_grid2);
% figure
% surf(x_grid2,y_grid2,data')
% 
% data_interp = interp2(x_grid2,y_grid2,data,x_grid_fine2,y_grid_fine2,'linear');
% data_interp_bis = interp2(x_grid,y_grid',data,x_grid_fine,y_grid_fine','linear');
% 
% 
% surf(x_grid_fine,y_grid_fine',data_interp_bis')
% 
% function F = myfun(x,y)
% 
% F = sqrt(x.^2 + y.^2)+ eps;
% 
% end




