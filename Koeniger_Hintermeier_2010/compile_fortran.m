% This script compiles the Fortran files and creates the executable
% WARNING: make sure you have the makefile "makefile_win" in this folder

command = 'nmake /f makefile_win clean';
%command = 'nmake /f makefile_win';

[status, ~] = system(command,'-echo');

if status ~= 0
    error('Command was not executed properly')
end