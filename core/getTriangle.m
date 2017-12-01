function points = getTriangle(base, rotation, dimensions)
%% Draw a triangle
% - by Matt Jaquiery
% 
% usage: drawTriangle(base[, orientation[, dimensions]])
%
% Inputs: 
% base - the [x y] coordinates for the centre of the (unrotated) triangle base
% orientation - the orientation of the triangle (angle theta of the
% rotation)
% dimensions - the [width, height] dimensions of the triangle
%
% Outputs:
% points - the set of points which define the triangle
%

if nargin < 3, dimensions = [10 10]; end
if nargin < 2, rotation = 0; end
if nargin < 1, error('No coordinates specified for triangle.'); end

w = floor(dimensions(1)/2); % /2 for ease of reference
h = floor(dimensions(2)); 

points = [[0,h]; ... % top points
    [-w, 0]; ... % bottom-left
    [w, 0]]; % bottom-right
       
% rotation       
t = rotation;
points = [points(:,1)*cosd(t) + points(:,2)*sind(t), ...
    points(:,1)*sind(t) + points(:,2)*cosd(t)];

% move to the requested absolute position
points = [points(:,1)+base(1), points(:,2)+base(2)];
