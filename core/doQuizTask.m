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

oldTextSize = Screen('TextSize', Sc.window, cfg.instr.textSize.medium);

% define time: if first trial of block getTime; if normal trial, time =
% end of previous trial
if t == 1 || cfg.restarted == 1 || trials(t-1).break == 1 
    time = GetSecs; 
else
    time = trials(t-1).time_endTrial;
end
   
cfg.taskManager.quiz.trialNumber = cfg.taskManager.quiz.trialNumber + 1;
trials(t).question = cfg.quiz{cfg.quizOrder(cfg.taskManager.quiz.trialNumber)};
if trials(t).wherelarger == 1
    % correct answer on the left
    L.text = trials(t).question.answer;
    R.text = trials(t).question.distractor;
else
    % correct answer on the right
    L.text = trials(t).question.distractor;
    R.text = trials(t).question.answer;
end

Q.text = trials(t).question.text;
Q = centerMultilineText(Q.text, [Sc.center(1), cfg.display.quiz.qPosY], ...
    Sc.size(1) * .6);
L = centerMultilineText(L.text, ...
    [Sc.center(1) - cfg.display.quiz.aOffsetX, cfg.display.quiz.aPosY], ...
    Sc.size(1) * .3);
R = centerMultilineText(R.text, ...
    [Sc.center(1) + cfg.display.quiz.aOffsetX, cfg.display.quiz.aPosY], ...
    Sc.size(1) * .3);

drawQuiz(Q, L, R);

trials(t).time_starttrial = Screen('Flip', Sc.window);

WaitSecs(cfg.stim.quiz.SRI1);
ShowCursorCenter('Arrow');

%% Get a first answer
lastResponse = NaN;
while true
    % acquire response
    [x, ~, buttons] = GetMouseWrapper();
    [keydown, ~, keycode] = KbCheck();
    
    % clean response
    if any(buttons)
        response = getQuizResponse(x);
    else
        response = lastResponse;
    end
    
    % check response
    if keydown && ~isnan(response)
        key = KbName(keycode);
        if iscell(key), key = key{1}; end % if two buttons at the same time
        switch key
            case 'space'           
                trials(t).time_response1 = GetSecs-time;
                trials(t).cj1 = abs(response);
                trials(t).int1 = find([-1 1]==sign(response));
                firstResponse = response;
                break;
            case 'ESCAPE'
                sca
        end
    end
    
    if response ~= lastResponse
        % draw the screen
        drawQuiz(Q, L, R);
        draw_static([1 0 1 2 1]);
        if ~isnan(response)
            drawQuizMarkers(response, cfg.bar.color.cursor);
            Screen('Flip', Sc.window);
        else
            [~, trials(t).time_responseStart1] = Screen('Flip', Sc.window);
        end
        lastResponse = response;
    end
end

% mark the answer
trials(t).cor1 = trials(t).int1 == trials(t).wherelarger;
trials(t).cor = trials(t).cor1;

HideCursor();

if trials(t).advisorId == 0 || cfg.quizOptions.adviceFormat == cfg.adviceFormat.none
    % tidy up
    Screen('TextSize', Sc.window, oldTextSize);
    PsychPortAudio('Close');
    Screen('Close');
    trials(t).time_endTrial = time;
    return;  % duck out early if no advice is to follow
end

%% Display advice
drawQuiz(Q, L, R);
draw_static([1 0 1 2 1]);
drawQuizMarkers(firstResponse, cfg.bar.color.cj1)
Screen('Flip', Sc.window);

if trials(t).block>1, PsychPortAudio('Close'); end

% Choice of advisor
if ~isempty(trials(t).choice)
    % get the judge's choice
    [trials(t).choiceDecision, trials(t).choiceTime] = getAdvisorChoice(trials(t).choice(1), trials(t).choice(2));
    % fill in the remaining trial details from the choice
    trials(t).advisorId = trials(t).choice(trials(t).choiceDecision);
end

% define advisor behaviour
% NOTE: advisor behaviour depends on initial judgements, not
% post-advice judgements
if ~isnan(trials(t).advisorId)
    % advisors are 60/70/80 or 80/70/60 agreement in correct (based on
    % confidence) and 30% agreement when participant is incorrect
    if trials(t).cor1 == 1
        toi = [trials(1:t).cor1] == 1 & ... % use last 2 blocks for reference dsitribution
            ([trials(1:t).block] == trials(t).block-1 | [trials(1:t).block] == trials(t).block-2); 
        if ~isnan(trials(t).overrideAdviceType)
            adviceType = trials(t).overrideAdviceType;
        else
            adviceType = cfg.advisor(trials(t).advisorId).adviceType;
        end
        [trials(t).agree, trials(t).step] = ...
            agreementf(trials(t).cj1,adviceType,abs([trials(toi).cj1]),'stepwise');
        clear toi
    else
        trials(t).agree = rand < .3; % flat agreement on incorrect trials
        trials(t).step  = NaN;
    end
    % define observer's accuracy
    if trials(t).agree == 1
        trials(t).obsacc = trials(t).cor;
    else
        trials(t).obsacc = 1 - trials(t).cor;
    end
else % null
    trials(t).agree  = NaN;
    trials(t).obsacc = NaN;
    trials(t).step   = NaN;
end

% masks for present_advice and prevent_delay
mask1 = [1 0 1 2 0];
mask2 = mask1;
if bitand(cfg.quizOptions.adviceFormat, cfg.adviceFormat.voice)
    load_observer_audio;
    WaitSecs(cfg.stim.quiz.RSI2);
    % advice presentation
    if ~isnan(trials(t).advisorId)
        present_advice;
    end
end
if isnan(trials(t).advisorId)
    present_delay;
end
ShowCursor('Arrow');

%% Get a second answer
if hasAdvice(trials(t))
    lastResponse = NaN;
    trials(t).time_responseStart2 = GetSecs();
    while true
        % acquire response
        [x, ~, buttons] = GetMouseWrapper();
        [keydown, ~, keycode] = KbCheck();

        % clean response
        if any(buttons)
            response = getQuizResponse(x);
        else
            response = lastResponse;
        end

        % check response
        if keydown && ~isnan(response)
            key = KbName(keycode);
            if iscell(key), key = key{1}; end % if two buttons at the same time
            switch key
                case 'space'           
                    trials(t).time_response2 = GetSecs-time;
                    trials(t).cj2 = abs(response);
                    trials(t).int2 = find([-1 1]==sign(response));
                    break;
                case 'ESCAPE'
                    sca
            end
        end

        if response ~= lastResponse
            % draw the screen
            drawQuiz(Q, L, R);
            if bitand(cfg.quizOptions.adviceFormat, cfg.adviceFormat.speechBubble)
                drawQuizAdviceOverlay(trials(t));
            end
            draw_static([1 0 1 2 1]);
            if ~isnan(response)
                drawQuizMarkers(firstResponse, cfg.bar.color.cj1, response, cfg.bar.color.cursor);
                Screen('Flip', Sc.window);
            else
                drawQuizMarkers(firstResponse, cfg.bar.color.cj1);
                Screen('Flip', Sc.window);
            end
            lastResponse = response;
        end
    end

    % mark the answer
    trials(t).cor2 = trials(t).int2 == trials(t).wherelarger;
    trials(t).cor = trials(t).cor2;
end
HideCursor();
Screen('TextSize', Sc.window, oldTextSize);

%% feedback
if trials(t).feedback
    if ~trials(t).cor, playFeedback(); end
    %colors=[.8 .2 .2;.2 .8 .2];
end

%--close audio/screen buffers
PsychPortAudio('Close');
Screen('Close');

trials(t).time_endTrial = time;

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
Screen('TextFont', Sc.window, 'Myriad Pro');

txt = {Q L R};

for i = 1:3
    T = txt{i};
    for ii = 1:length(T.lines)
        Screen('DrawText', Sc.window, T.lines{ii}.text, ...
            T.lines{ii}.x, T.lines{ii}.y);
    end
end

function drawQuizAdviceOverlay(trial)
%% Draw an overlay of a speech bubble with the advisor's portrait
% Matt Jaquiery, Feb 2018
% 
% usage: drawQuizAdviceOverlay(trial)
%
% Inputs:
% trial: current trial object
global cfg;
global Sc;
% fetch the option endorsed by the advisor
if trial.obsacc == 1
    adviceString = trial.question.answer;
else
    adviceString = trial.question.distractor;
end

adviceString = ['I think the answer is ' adviceString '.'];
x = Sc.center(1);
y = 300;
args = struct('offset', [x y], 'bubbleFrameColor', cfg.display.quiz.adviceBubbleColor);
drawAdvisorBubble(trial.advisorId, adviceString, args);

function response = getQuizResponse(mouse_x)
%% Return the scale value designated by the current mouse position
% Matt Jaquiery, 2017
% 
% usage: response = getQuizResponse(mouse_x)
%
% Inputs:
% mouse_x: x-coordinate of the mouse
% 
% Outputs:
% response: the response as capped by the scale limits
global cfg; % configuration object
global Sc; % screen object
response = find(mouse_x < (cfg.bar.xshift+cfg.bar.cursorwidth.*.5),1) - cfg.bar.maxScale;

if mouse_x < Sc.center(1) % if mouse is on the left rect
        response = response-1;
    if response == 0
        response = -1;
    end
else
    if response == 0
        response = 1;
    elseif isempty(response)
        response = cfg.bar.maxScale;
    end
end


function drawQuizMarkers(varargin)
%% Draw the markers on top of the scale
% Matt Jaquiery, 2017
% 
% usage: drawQuizMarkers(score1, colour1[, score2, colour2[, ...]])
%
% Inputs:
% Score-colour pairs. Score is a scale score (see define_scale.m), and
% colour is the colour to draw the marker.
%
global cfg; % configuration object
global Sc; % screen object

if nargin < 2, error('No marker defined.'); end
if mod(nargin,2), error('Unbalanced marker definitions (perhaps a colour was omitted)'); end

markers = zeros(4, length(varargin)/2);
colours = zeros(3, length(varargin)/2);
for i = 1:2:length(varargin)
    score = varargin{i};
    if isnan(score), continue; end
    
    box = [0 0 cfg.bar.cursorwidth cfg.bar.cursorheight];
    switch score > 0
        case 0 % Left
            positions = linspace(cfg.bar.gaprect(1)-cfg.bar.cursorwidth.*.5, ...
                cfg.bar.barrect(1)+cfg.bar.cursorwidth.*.5,cfg.bar.maxScale);
        case 1 % Right
            positions = linspace(cfg.bar.gaprect(3)+cfg.bar.cursorwidth.*.5, ...
                cfg.bar.barrect(3)-cfg.bar.cursorwidth.*.5,cfg.bar.maxScale);
    end
    box = CenterRectOnPoint(box, positions(abs(score)), Sc.rect(4).*cfg.bar.positiony);
    
    markers = [markers box'];
    colours = [colours varargin{i+1}];
end

Screen('FillRect', Sc.window, colours, markers);
