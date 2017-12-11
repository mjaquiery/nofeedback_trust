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
ShowCursor();

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
                rt1 = GetSecs-time;
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
        end
        Screen('Flip', Sc.window);
        lastResponse = response;
    end
end

response
rt1

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
markers = [];
colours = [];

if nargin < 2, error('No marker defined.'); end
if mod(nargin,2), error('Unbalanced marker definitions (perhaps a colour was omitted)'); end

for i = 1:2:length(varargin)
    score = varargin{i}
    
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
