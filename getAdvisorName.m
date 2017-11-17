function [advisorName] = getAdvisorName(advisorVoiceId)
%% Get the name of an advisor from their Voice Nubmer
% - by Matt Jaquiery 
% 
% usage: advisorName = getAdvisorName([advisorVoiceId]);
%
% inputs:
%   advisorVoiceId: (optional) the voice number assigned to the advisor
%   (NaN and empty[default] are special cases)
%
% outputs:
%   advisorName: string containing the advisor's name
%
% Advisors introduce themselves verbally, and this function maps a string
% representation of the advisor's name to the numerical identifier of the
% voice performing the introduction.
%

switch advisorVoiceId
    case 1
        advisorName = 'Annie';
    case 2
        advisorName = 'Bae';
    case 3
        advisorName = 'Kate';
    case 4
        advisorName = 'Debbie';
    case NaN
        advisorName = 'No advice';
    otherwise % no choice
        advisorName = '';
end