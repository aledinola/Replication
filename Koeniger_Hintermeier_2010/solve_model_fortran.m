function [sol] = solve_model_fortran(par,DirExe,main_folder)

%% Write files to be imported in Fortran

disp('SWITCH TO FORTRAN')
disp('Writing files for the fortran executable.')
disp('Please wait...')
    
% Real parameters (must be fields of the struct "par")
real_params_names = {'r','delta','beta','sigma','theta','y_gam','epscon','epsdur','miu','gamma','alpha','tol_val'};
real_params = struct2vec(par,real_params_names);
dlmwrite(fullfile(DirExe,'real_params.txt'), real_params, 'delimiter', '\t', 'precision', 16);

% Integer parameters (must be fields of the struct "par")
int_params_names = {'nz','nx','nd','maxit_val','verbose','mqpbounds','howard','n_howard','par_fortran','lowMem','do_monot','do_refinement'};
int_params = struct2vec(par,int_params_names);
dlmwrite(fullfile(DirExe,'int_params.txt'), int_params, 'delimiter', '\t', 'precision', 16);

% Dimensions
dimensions = [length(real_params);length(int_params)]; %the dimensions
dlmwrite(fullfile(DirExe,'dimensions.txt'), dimensions, 'delimiter', '\t', 'precision', 16);

% Arrays (real or integers)
dlmwrite(fullfile(DirExe,'z_grid.txt'), par.z_grid, 'delimiter', '\t', 'precision', 16);
dlmwrite(fullfile(DirExe,'d_grid.txt'), par.d_grid, 'delimiter', '\t', 'precision', 16);
dlmwrite(fullfile(DirExe,'x_grid.txt'), par.x_grid, 'delimiter', '\t', 'precision', 16);
dlmwrite(fullfile(DirExe,'z_prob.txt'), par.z_prob(:), 'delimiter', '\t', 'precision', 16);

disp('Files written!')

%% Call Fortran executable

disp('Calling the executable now:')
%Find the OS type
os = findos();

%Change path
cd(DirExe)

if strcmp(os, 'unix') == 1 %UNIX
    error('Fortran exe for unix does not exist yet')
elseif strcmp(os, 'unix64') == 1 %UNIX64
    error('Fortran exe for unix does not exist yet')
elseif strcmp(os, 'windows') == 1 %WINDOWS
    [status, ~] = system('run.exe','-echo');
elseif strcmp(os, 'mac') == 1 %MAC
    error('Fortran exe for unix does not exist yet')
else
    error('Specify the variable "os".')
end

if status ~= 0
    error('FORTRAN procedure was not executed properly.')
end

%Return to the old path:
cd(main_folder)

%% Read Fortran output

disp('Reading files generated by fortran.')
disp('Please wait...')

sol = struct();

save temp

if par.verbose>=1
    tic;
end
    
array_names = {'value';'pol_xprime';'pol_dprime';'pol_aprime';'pol_c'};
siz = [par.nx,par.nd,par.nz];
for ii = 1:numel(array_names)
    % Files to be read are in binary format
    filename = strcat(array_names{ii},'.bin');
    sol.(array_names{ii}) = loadBinary(fullfile(DirExe,filename),'double',siz);
    
    % Files to be read are in text format
    %filename = strcat(array_names{ii},'.txt');
    %sol.(array_names{ii}) = loadArray(fullfile(DirExe,filename),siz);
end

% Scalars are always written in text format for simplicity
flag_vfi = load(fullfile(DirExe,'flag_vfi.txt'));

disp('Files read!')

if par.verbose>=1
    time=toc;
    fprintf('Time to read the files: %f \n', time)
end

if flag_vfi<0
    error('VFI did not converge')
end



%% Checks
% Sanity checks
disp("VFI diagnostics: make sure grid is not too restrictive")
fprintf('Lower bound wealth grid x = %f \n',par.x_grid(1))
fprintf('Min of wealth policy x''  = %f \n',min(sol.pol_xprime(:)))
fprintf('Upper bound wealth grid x = %f \n',par.x_grid(end))
fprintf('Max of wealth policy x''  = %f \n',max(sol.pol_xprime(:)))
fprintf(' \n')

end %end function <solve_model_fortran>