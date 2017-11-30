function showAdvisorPolitics(advisorId)
%% Display one political opinion for an advisor
% - by Matt Jaquiery
%
% Look at the advisor's political opinions, find one which is still unset,
% calculate an appropriate value using the participant's political
% orientation and the political agreement profile of the advisor, and
% display that new value. 
% If all values are taken, issue a warning and repeat a value at random.
%
% 

if nargin < 1
    error('No advisor ID specified.');
end