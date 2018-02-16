function a = hasAdvice(trial)
%% Return true if trial has a genuine advisor (false on nothing or placeholder)
% Matt Jaquiery, Feb 2018
%
% usage: a = hasAdvice(trial)
%
if isnan(trial.advisorId) || trial.advisorId == 0
    a = false;
else
    a = true;
end