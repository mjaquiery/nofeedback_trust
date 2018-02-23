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

%% Predefined settings (don't override)
if ~isfield(cfg, 'quizOptions') || ~ isfield(cfg.quizOptions, 'adviceFormat')
    cfg.quizOptions.adviceFormat = cfg.adviceFormat.voice;
end

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
cfg.display.quiz.adviceBubbleColor = [0 0 0];
cfg.display.quiz.feedback.textSize = 18;
cfg.display.quiz.feedback.lineSpacing = 1.2;
cfg.display.quiz.feedback.questionsPerPage = 53;
cfg.display.quiz.feedback.answerColor.incorrect = [.8 0 0];
cfg.display.quiz.feedback.answerColor.correct = [0 .8 0];
cfg.display.quiz.feedback.questionColor = [0 0 0];
cfg.display.quiz.feedback.questionMaxWidth = Sc.size(1) * .4;
cfg.display.quiz.feedback.marginY = 30;
cfg.display.quiz.feedback.answerMarginY = 0;
cfg.display.quiz.feedback.answerMarginX = 5;
cfg.display.quiz.feedback.startX = Sc.size(1) * .05;
cfg.display.quiz.feedback.startY = 75;
cfg.display.quiz.feedback.instructionsY = Sc.size(2) * .9;
cfg.display.quiz.feedback.instructions = {'Page ', '/', '. Left arrow = back; press any other key to continue...'};

%% Timing settings
cfg.stim.quiz.SRI1 = 0; % stimulus-response interval (stimulus--cj1)
cfg.stim.quiz.RSI1 = 0; % response-stimulus interval (cj1 -- advice-prompt)
cfg.stim.quiz.RSI2 = cfg.stim.RSI2; % response-stimulus interval (advice-prompt -- advice)
