function [zi,s,t,onemt,ndx,nrows] = interp2_modified(x,y,z,xi,yi)
%
% This code is based on Matlab's interp2 (see following)
%
%INTERP2 2-D interpolation (table lookup).
%   ZI = INTERP2(X,Y,Z,XI,YI) interpolates to find ZI, the values of the
%   underlying 2-D function Z at the points in matrices XI and YI.
%   Matrices X and Y specify the points at which the data Z is given.
%
%   XI can be a row vector, in which case it specifies a matrix with
%   constant columns. Similarly, YI can be a column vector and it 
%   specifies a matrix with constant rows. 
%
%   ZI = INTERP2(Z,XI,YI) assumes X=1:N and Y=1:M where [M,N]=SIZE(Z).
%   ZI = INTERP2(Z,NTIMES) expands Z by interleaving interpolates between
%   every element, working recursively for NTIMES.  INTERP2(Z) is the
%   same as INTERP2(Z,1).
%
%   ZI = INTERP2(...,METHOD) specifies alternate methods.  The default
%   is linear interpolation.  Available methods are:
%
%     'nearest' - nearest neighbor interpolation
%     'linear'  - bilinear interpolation
%     'cubic'   - bicubic interpolation
%     'spline'  - spline interpolation
%
%   For faster interpolation when X and Y are equally spaced and monotonic,
%   use the syntax ZI = INTERP2(...,*METHOD).
%
%   ZI = INTERP2(...,METHOD,EXTRAPVAL) specificies a method and a scalar 
%   value for ZI outside of the domain created by X and Y.  Thus, ZI will
%   equal EXTRAPVAL for any value of YI or XI which is not spanned by Y 
%   or X respectively. A method must be specified for EXTRAPVAL to be used,
%   the default method is 'linear'.
%
%   All the interpolation methods require that X and Y be monotonic and
%   plaid (as if they were created using MESHGRID).  If you provide two
%   monotonic vectors, interp2 changes them to a plaid internally. 
%   X and Y can be non-uniformly spaced.
%
%   For example, to generate a coarse approximation of PEAKS and
%   interpolate over a finer mesh:
%       [x,y,z] = peaks(10); [xi,yi] = meshgrid(-3:.1:3,-3:.1:3);
%       zi = interp2(x,y,z,xi,yi); mesh(xi,yi,zi)
%
%   Class support for inputs X, Y, Z, XI, YI:  
%      float: double, single
%
%   See also INTERP1, INTERP3, INTERPN, MESHGRID, GRIDDATA.

%   Copyright 1984-2005 The MathWorks, Inc.
%   $Revision: 5.33.4.11 $


% Check for plaid data.
%
xx = x(1,:); yy = y(:,1);
if (size(x,2)>1 && ~isequal(repmat(xx,size(x,1),1),x)) || ...
   (size(y,1)>1 && ~isequal(repmat(yy,1,size(y,2)),y))
    error('MATLAB:interp2:meshgrid',...
        ['X and Y must be matrices produced by MESHGRID. Use' ...
        ' GRIDDATA instead \nof INTERP2 for scattered data.']);
end

%
% Check for non-equally spaced data.  If so, map (x,y) and
% (xi,yi) to matrix (row,col) coordinate system.
%


% Determine the nearest location of xi in x
[xxi,j] = sort(xi(:));
[~,i] = sort([xx;xxi]);
ui(i) = 1:length(i);
ui = (ui(length(xx)+1:end)-(1:length(xxi)))';
ui(j) = ui;

% Map values in xi to index offset (ui) via linear interpolation
ui(ui<1) = 1;
ui(ui>length(xx)-1) = length(xx)-1;
ui = ui + (xi(:)-xx(ui))./(xx(ui+1)-xx(ui));

% Determine the nearest location of yi in y
[yyi,j] = sort(yi(:));
[~,i] = sort([yy;yyi(:)]);
vi(i) = 1:length(i);
vi = (vi(length(yy)+1:end)-(1:length(yyi)))';
vi(j) = vi;

% Map values in yi to index offset (vi) via linear interpolation
vi(vi<1) = 1;
vi(vi>length(yy)-1) = length(yy)-1;
vi = vi + (yi(:)-yy(vi))./(yy(vi+1)-yy(vi));

[x,y] = meshgrid(ones(class(x)):size(x,2),ones(class(y)):size(y,1));
xi(:) = ui; yi(:) = vi;



% Now do the interpolation based on method.
[zi,s,t,onemt,ndx,nrows] = linear(x,y,z,xi,yi); 

end

%------------------------------------------------------
function [F,s,t,onemt,ndx,nrows] = linear(arg1,arg2,arg3,arg4,arg5)
%LINEAR 2-D bilinear data interpolation.
%   ZI = LINEAR(EXTRAPVAL,X,Y,Z,XI,YI) uses bilinear interpolation to
%   find ZI, the values of the underlying 2-D function in Z at the points
%   in matrices XI and YI.  Matrices X and Y specify the points at which
%   the data Z is given.  X and Y can also be vectors specifying the
%   abscissae for the matrix Z as for MESHGRID. In both cases, X
%   and Y must be equally spaced and monotonic.
%
%   Values of EXTRAPVAL are returned in ZI for values of XI and YI that are
%   outside of the range of X and Y.
%
%   If XI and YI are vectors, LINEAR returns vector ZI containing
%   the interpolated values at the corresponding points (XI,YI).
%
%   ZI = LINEAR(EXTRAPVAL,Z,XI,YI) assumes X = 1:N and Y = 1:M, where
%   [M,N] = SIZE(Z).
%
%   ZI = LINEAR(EXTRAPVAL,Z,NTIMES) returns the matrix Z expanded by
%   interleaving bilinear interpolates between every element, working
%   recursively for NTIMES. LINEAR(EXTRAPVAL,Z) is the same as
%   LINEAR(EXTRAPVAL,Z,1).
%
%   See also INTERP2, CUBIC.


 
% linear(extrapval,x,y,z,s,t), X and Y specified.
[nrows,ncols] = size(arg3);
mx = numel(arg1); my = numel(arg2);
if (mx ~= ncols || my ~= nrows) && ~isequal(size(arg1),size(arg2),size(arg3))
    error('MATLAB:interp2:linear:XYZLengthMismatch',...
        'The lengths of the X and Y vectors must match Z.');
end
if nrows < 2 || ncols < 2
    error('MATLAB:interp2:linear:sizeZ','Z must be at least 2-by-2.');
end
s = 1 + (arg4-arg1(1))/(arg1(end)-arg1(1))*(ncols-1);
t = 1 + (arg5-arg2(1))/(arg2(end)-arg2(1))*(nrows-1);



if nrows < 2 || ncols < 2
    error('MATLAB:interp2:linear:sizeZsq','Z must be at least 2-by-2.');
end
if ~isequal(size(s),size(t))
    error('MATLAB:interp2:linear:XIandYISizeMismatch',...
        'XI and YI must be the same size.');
end

% Check for out of range values of s and set to 1
sout = find((s<1)|(s>ncols));
if ~isempty(sout), s(sout) = 1; end

% Check for out of range values of t and set to 1
tout = find((t<1)|(t>nrows));
if ~isempty(tout), t(tout) = 1; end

% Matrix element indexing
ndx = floor(t)+floor(s-1)*nrows;

% Compute intepolation parameters, check for boundary value.
if isempty(s), d = s; else d = find(s==ncols); end
s(:) = (s - floor(s));
if ~isempty(d), s(d) = s(d)+1; ndx(d) = ndx(d)-nrows; end

% Compute intepolation parameters, check for boundary value.
if isempty(t), d = t; else d = find(t==nrows); end
t(:) = (t - floor(t));
if ~isempty(d), t(d) = t(d)+1; ndx(d) = ndx(d)-1; end

% Now interpolate.
onemt = 1-t;

    F =  ( arg3(ndx).*(onemt) + arg3(ndx+1).*t ).*(1-s) + ...
         ( arg3(ndx+nrows).*(onemt) + arg3(ndx+(nrows+1)).*t ).*s;
    %disp('hi')


% Now set out of range values to NaN.
if ~isempty(sout)
    F(sout) = NaN; 
end
if ~isempty(tout)
    F(tout) = NaN; 
end

end



