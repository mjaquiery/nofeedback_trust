function quizFeedback(trials, startTrial, endTrial)
%% Give a participant feedback on their performance on the quiz
% Matt Jaquiery, Feb 2018
%
% usage: quizFeedback(trials[, startTrial[, endTrial]])
%
% inputs:
% trials - trial list object
% startTrial - index in trials at which to start (default first)
% endTrial - index in trials at which to end (default last)

global cfg;
global Sc;

if nargin < 3, endTrial = length(trials); end
if nargin < 2, startTrial = 1; end

qData = struct();
i = 1;

% compile a list of quiz trials which were answered
for t = startTrial:endTrial
    if trials(t).taskType == cfg.taskType.quiz
        qData(i).text = trials(t).question.text;
        qData(i).answer = trials(t).question.answer;
        qData(i).cor = trials(t).cor;
        i = i + 1;
    end
end

% print the answers nicely by pages
pages = ceil((i-1)/cfg.display.quiz.feedback.questionsPerPage);
p = 1;
while true
    drawQuizFeedback(qData, (p-1)*cfg.display.quiz.feedback.questionsPerPage+1, p, pages);
    % allow navigation
    [~, ~, keycode] = collect_response(inf);
    switch keycode
        case 'ESCAPE'
            sca
        case 'LeftArrow'
            % go back if we can
            if p > 1
                p = p - 1; 
            end
        otherwise
            if p == pages
                return;
            else
                p = p + 1;
            end
    end
end

function drawQuizFeedback(qData, startAt, pNum, pCount)
% present quiz feedback on screen
global cfg;
global Sc;

y = [cfg.display.quiz.feedback.startY cfg.display.quiz.feedback.startY];

oldTextSize = Screen('TextSize', Sc.window, cfg.display.quiz.feedback.textSize);
for q = startAt:startAt+cfg.display.quiz.feedback.questionsPerPage-1
    if q > length(qData)
        break
    end
    % draw in the correct column
    column = mod(q, 2)+1;
    if column == 1 % left
        x = cfg.display.quiz.feedback.startX;
    else
        x = Sc.size(1) * .5;
    end
    % answer color
    if qData(q).cor
       aColor = cfg.display.quiz.feedback.answerColor.correct;
    else
       aColor = cfg.display.quiz.feedback.answerColor.incorrect;
    end
    % wrap question text if necessary
    qText = centerMultilineText(qData(q).text, [x,0], ...
        cfg.display.quiz.feedback.questionMaxWidth, ...
        cfg.display.quiz.feedback.lineSpacing);
    % undo vertical centering
    if qText.lines{1}.y < 0
        yStart = qText.lines{1}.y * -1;
    else
        yStart = 0;
    end
    for L = 1:length(qText.lines)
        qText.lines{L}.y = qText.lines{L}.y + yStart;
        Screen('DrawText', Sc.window, qText.lines{L}.text, x, ...
            y(column)+qText.lines{L}.y, ...
            cfg.display.quiz.feedback.questionColor);
    end
    % we can also assume the answer will never need wrapping.
    % Work out the answer position: if it fits on the same line as the
    % last line of the question then simply append it, otherwise draw it
    % right-aligned on a new line
    aBounds = Screen('TextBounds', Sc.window, qData(q).answer);
    if aBounds(3) + qText.lines{end}.bounds(3) + cfg.display.quiz.feedback.answerMarginX ...
            < cfg.display.quiz.feedback.questionMaxWidth
        % answer fits
        answerX = x + qText.lines{end}.bounds(3) + cfg.display.quiz.feedback.answerMarginX;
        answerY = y(column) + qText.lines{end}.y;
    else
        % right-aligned on a new line
        answerX = (x+cfg.display.quiz.feedback.questionMaxWidth) - aBounds(3);
        answerY = y(column) + qText.lines{end}.y + qText.lines{end}.bounds(4) + cfg.display.quiz.feedback.answerMarginY;
    end
    Screen('DrawText', Sc.window, qData(q).answer, answerX, answerY, aColor);
    % save the y-offset for next question
    y(column) = answerY + cfg.display.quiz.feedback.marginY;
end
instructions = [cfg.display.quiz.feedback.instructions{1} int2str(pNum) ...
    cfg.display.quiz.feedback.instructions{2} int2str(pCount) ...
    cfg.display.quiz.feedback.instructions{3}];
iBounds = Screen('TextBounds', Sc.window, instructions);
Screen('DrawText', Sc.window, instructions, ...
    Sc.size(1)/2 - iBounds(3)/2, cfg.display.quiz.feedback.instructionsY, ...
    cfg.display.quiz.feedback.questionColor);
Screen('Flip', Sc.window);
Screen('TextSize', Sc.window, oldTextSize);