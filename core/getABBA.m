function id = getABBA(x)
%% Return the id which is xth in counterbalanced sequence ABBAABBA...
% - by Matt Jaquiery
%
% usage: id = getABBA(x)
%
% Inputs:
% x - xth element from the sequence
%
% Outputs:
% id - the id of the xth element in the sequence [12211221...]
%

if nargin < 1, error('x not specified'); end


n = mod(x, 4);

if n==2 || n==3 % 2nd and 3rd positions
    id = 2;
else
    id = 1;
end
