function advisor = getNewAdvisor()
%% Return a blank advisor template
% Matt Jaquiery, Jan 2018
% 
% usage: advisor = getNewAdvisor()
%
% outputs:
% advisor - new advisor template

advisor.id = NaN; % unique identifier across advisors
advisor.adviceType = NaN; % advice type
advisor.pic = NaN; % portrait id
advisor.voice = NaN; % voice id
advisor.name = 'UNNAMED'; % name
advisor.firstQuestionnaireDone = false; % whether the prospective questionnaire has been answered
advisor.timeFirstIntroduced = NaN; % time the advisor was introduced