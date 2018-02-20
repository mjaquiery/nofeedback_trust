function defineQuiz()
%% Set up the quiz task
% - by Matt Jaquiery
% 
% usage: defineQuiz();
%
% Set up the quiz: load questions and set the display variables
%
global cfg; % configuration object
global Sc; % screen object

%% Load quiz questions
cfg.quiz = xml2struct(xmlread([cfg.path.stims cfg.path.slash 'generalKnowledgeQuestions.xml']));
cfg.quiz = cfg.quiz.quiz.question;
for i = 1:length(cfg.quiz)
    cfg.quiz{i}.text = cfg.quiz{i}.text.Text;
    cfg.quiz{i}.answer = cfg.quiz{i}.answer.Text;
    cfg.quiz{i}.distractor = cfg.quiz{i}.distractor.Text;
end



% Shuffle the questions
cfg.quizOrder = [1 2 3 3+randperm(length(cfg.quiz)-3)];

%% Display settings
cfg.display.quiz.qPosY = Sc.size(2) * .2;
cfg.display.quiz.aPosY = Sc.size(2) * .35;
cfg.display.quiz.aOffsetX = Sc.size(1) * .25;

%% Timing settings
cfg.stim.quiz.SRI1 = 1; % stimulus-response interval (stimulus--cj1)
cfg.stim.quiz.RSI1 = cfg.stim.RSI1; % response-stimulus interval (cj1 -- advice-prompt)
cfg.stim.quiz.RSI2 = cfg.stim.RSI2; % response-stimulus interval (advice-prompt -- advice)
