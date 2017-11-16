function [advisorName] = getAdvisorName(advisorVoiceID)
% returns a string of the name the advisor uses to introduce themselves
% for voice advisorVoiceID

switch advisorVoiceID
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