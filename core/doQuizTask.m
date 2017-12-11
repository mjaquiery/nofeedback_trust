function trials = doQuizTask(trials, t)
%% Run a single trial of the Quiz
% - by Matt Jaquiery
%
% usage: trials = doQuizTask(trials, t)
%
% Inputs:
% trials: list of trials in the current experiment
% t: index of the current trial in the trial list
%
% Outputs:
% trials: list of trials in the current experiment updated with the results
% of this task at trials(t)
%
global cfg; % configuration object
global Sc; % screen object

if ~isfield(cfg, 'quiz')
    defineQuiz();
end

% define time: if first trial of block getTime; if normal trial, time =
    % end of previous trial
    if t == 1 || cfg.restarted == 1 || trials(t-1).break == 1 
        time = GetSecs; 
    else
        time = trials(t-1).endTime;
    end
    
trials(t).question = cfg.quiz{cfg.quizOrder(t)};
if trials(t).wherelarger == 1
    L.text = trials(t).question.answer;
    R.text = trials(t).question.distractor;
else
    L.text = trials(t).question.distractor;
    R.text = trials(t).question.answer;
end

Q.text = trials(t).question.text;
Q.bounds = Screen('TextBounds', Sc.window, Q.text);
L.bounds = Screen('TextBounds', Sc.window, L.text);
R.bounds = Screen('TextBounds', Sc.window, R.text);

drawQuiz(Q, L, R);

trials(t).time_starttrial = Screen('Flip', Sc.window);

WaitSecs(cfg.stim.quiz.RSI1);

%% Get a first answer
lastAns = NaN;
while true
    % acquire response
    [x, ~, buttons] = GetMouseWrapper;
    [keydown, a.rt, keycode] = KbCheck;
    
    % clean response
    
    
    % check response
    if keydown && ~isnan(ans)
        key = KbName(keycode);
        if iscell(key), key = key{1}; end % if two buttons at the same time
        switch key
            case 'space'           
                a.rt = GetSecs-onset_t;
                break;
            case 'ESCAPE'
                sca
        end
    end
    
    if ans==lastAns && ~any(buttons)
        % skip drawing if there's no progress to update
        continue;
    end
    
    % draw the screen
    drawQuiz(Q, L, R);
    draw_static([1 0 1 2 1]);
    Screen('Flip', Sc.window);
        
end

%% Display advice

%% Get a second answer


function drawQuiz(Q, L, R)
%% Draw the Quiz question and answer options
% - Matt Jaquiery, 2017
%
% usage: drawQuiz(Q, L, R)
% 
% Inputs:
% Q: question struct containing .text (text), .bounds (text bounds)
% L: left text struct as for Q
% R: right text struct as for Q
%
global cfg; % configuration object
global Sc; % screen object
oldTextSize = Screen('TextSize', Sc.window, cfg.instr.textSize.medium);
Screen('TextFont', Sc.window, 'Myriad Pro');

Screen('DrawText', Sc.window, Q.text, ...
    Sc.center(1) - floor(Q.bounds(3)/2), ...
    cfg.display.quiz.qPosY);
Screen('DrawText', Sc.window, L.text, ...
    Sc.center(1) - cfg.display.quiz.aOffsetX - floor(L.bounds(3)/2), ...
    cfg.display.quiz.aPosY);
Screen('DrawText', Sc.window, R.text, ...
    Sc.center(1) + cfg.display.quiz.aOffsetX - ceil(R.bounds(3)/2), ...
    cfg.display.quiz.aPosY);
