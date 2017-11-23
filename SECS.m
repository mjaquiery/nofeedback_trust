function [score, score_breakdown, answers] = SECS()
%% Presentation of the Social + Economic Conseravtism Scale
% - by Matt Jaquiery
%
% usage: [score, score_breakdown, answers] = SECS()
%
% Outputs: 
% score: the raw score obtained by the participant
% score_breakdown: a struct with subscore fields: social, economic, total
% answers: the answers arranged in a struct with the following fields:
%   abortion, government, security, religion, welfare, guns, marriage,
%   values, money, business, family, patriotism
%   Each of these is a struct containing:
%       score: calculated score, allowing for reverse-scoring
%       rt: response time
%       social: whether or not the question is social (not implies
%       economic)
%       reverse_scored: whether or not the question is reverse-scored
%       text: the text of the prompt
%       presentation_order: when this was presented
%
% Scale reference: Everett, J. A. C. (2013). The 12 Item Social and
% Economic Conservatism Scale (SECS). PLOS ONE, 8(12), e82131.
% https://doi.org/10.1371/journal.pone.0082131
%
% adaptation of questionnaire script by Niccolo Pescetelli
%
% Present subject with a 11-point scale (0-10) for their positivity towards
% 12 different items. 6 are social and 6 economic, 1 item in each set is
% reverse-scored
%
% Participants answer on a sliding scale with the mouse and confirm
% selections with the spacebar

global cfg; % configuration object
global Sc; % screen object

%% define the questionnaire
questions.text = {...
    'Abortion' ...
    'Limited government' ...
    'Military and national security' ...
    'Religion' ...
    'Welfare benefits' ...
    'Gun ownership' ...
    'Traditional marriage' ...
    'Traditional values' ...
    'Fiscal responsibility' ...
    'Business' ...
    'The family unit' ...
    'Patriotism'};
questions.name = {...
    'abortion' ...
    'government' ...
    'security' ...
    'religion' ...
    'welfare' ...
    'guns' ...
    'marriage' ...
    'values' ...
    'money' ...
    'business' ...
    'family' ...
    'patriotism'};
question_order = randperm(length(questions.text)); % randomize questions presentation
revMask = zeros(1,12); % reverse scoring mask
revMask([1 5]) = 1; % Abortion and Welfare are reverse-scored
socialMask = zeros(1,12); 
socialMask([1 3 4 7 8 11 12]) = 1; % identify the social subscale items
answers = struct;
prompt = 'Please indicate the extent to which you feel positive or negative towards each issue';
labels = {'Negative', 'Neutral', 'Positive'};
%% Define screen presentation details
maxS=100; % max score
minS=1; % min score
ns=100; % number of points
cw=cfg.bar.cursorwidth; % cursor [visual indicator] width
ch=cfg.bar.cursorheight; % cursor height 
by = Sc.size(2)*.7; % bar y position
qy = Sc.size(2)*.4; % question position y
qSize = 46; % question text size
brct=CenterRectOnPoint([0 0 (ns *cw) (ch)], Sc.center(1), by);
bl = brct(3)- brct(1); % length (width) of the scale rectangle
Screen('Flip', Sc.window);
oldTextSize = Screen('TextSize',Sc.window);
Screen('TextFont', Sc.window, 'Myriad Pro');
ShowCursor('Arrow');

%% Show questionnaire
for ii = 1: length(questions.text)
    a = struct;
    a.text = questions.text{question_order(ii)};
    a.presentation_order = ii;
    a.social = socialMask(question_order(ii));
    a.reverse_scored = revMask(question_order(ii));
    a.rt = NaN; % initialize response time variable
    a.score = NaN;
    name = questions.name{question_order(ii)};
    haschanged = false;   % to avoid automatic responses
    onset_t = NaN;
    
    %% Calculate the text bounds    
    Screen('TextSize', Sc.window, qSize);
    Qbounds = Screen('TextBounds', Sc.window, a.text);
    Screen('TextSize', Sc.window, cfg.instr.textSize.small);
    labelBounds = zeros(1,length(labels));
    for j = 1:length(labels)
        sb = Screen('TextBounds', Sc.window, labels{j});
        labelBounds(j) = sb(3); % we only care about the width here    
    end    
    Screen('TextSize', Sc.window, cfg.instr.textSize.medium);
    promptBounds = Screen('TextBounds', Sc.window,prompt);
  
    %% detect response
    while true
        [x, ~, buttons] = GetMouseWrapper;
        [keydown, a.rt, keycode] = KbCheck;
        % only draw if buttons are held down or it's the first run
        if ~haschanged || any(buttons) || keydown
            % draw question text
            Screen('TextSize', Sc.window, qSize);
            Screen('DrawText', Sc.window, a.text, Sc.center(1)- (Qbounds(3)/2), qy, 0);
            % draw prompt
            Screen('TextSize', Sc.window, cfg.instr.textSize.medium);
            Screen('DrawText', Sc.window, prompt, Sc.center(1) - promptBounds(3)/2, ...
                by+cfg.instr.Q.position.labelOffsetY, 0);
            %add labels
            Screen('TextSize', Sc.window, cfg.instr.textSize.small);
            Screen('DrawText', Sc.window, labels{1}, ...
                Sc.center(1) - bl/2 - labelBounds(1)/2, ... center - half bar length - offset
                by-cfg.instr.Q.position.labelOffsetY, 0);
            Screen('DrawText', Sc.window, labels{2}, ...
                Sc.center(1) - labelBounds(2)/2, ... center - offset
                by-cfg.instr.Q.position.labelOffsetY, 0);
            Screen('DrawText', Sc.window, labels{3}, ...
                Sc.center(1) + bl/2 - labelBounds(3)/2, ... center - half bar length - offset
                by-cfg.instr.Q.position.labelOffsetY, 0);
            % draw response bar
            if ~isnan(a.score)
                % add cursor position
                cursorrect = CenterRectOnPoint([0,0,cw,ch],...
                    Sc.center(1) -((ns*cw/2)+cw) + (a.score * cw  + cw/2), by);
                rect = [brct' cursorrect'];
                Screen('FillRect', Sc.window, [cfg.bar.color.bar cfg.bar.color.cursor],rect);
            else
                Screen('FillRect', Sc.window, cfg.bar.color.bar, brct');
            end
            tm = Screen('Flip', Sc.window);
            % record onset time if necessary
            if isnan(onset_t)
                onset_t = tm;
            end

            %update answer if we clicked            
            if any(buttons) % button clicked
                if x < brct(1) % boundaries of the slider
                    x = brct(1);
                elseif x > brct(3)
                    x = brct(3);
                end
                ans = x - brct(1); % where abouts on the scale are we?
                if ans ~= a.score
                    haschanged = 1;
                end
                a.score = ceil(ans/cw);
                if cfg.debug
                    Screen('DrawText', Sc.window, ['x:' int2str(ceil(x)) '; a:' int2str(ans) '; btn:' int2str(any(buttons))], 0, 0);
                end
                
                % bound visibility
                if a.score > maxS % max
                    a.score = maxS;
                elseif a.score < minS % minScale
                    a.score = minS;
                end
            end

            % check for keys
            if keydown && ~isnan(a.score)
                key = KbName(keycode);
                if iscell(key), key = key{1}; end % if two buttons at the same time
                switch key
                    case 'space'                        
                        if ~haschanged
                            % mark that a change occurred
                            haschanged = true;
                        else
                            a.rt = GetSecs-onset_t;
                            break;
                        end
                    case 'ESCAPE'
                        sca
                end
            end
        end
    end
    if a.reverse_scored
        a.score = abs(a.score - maxS); % reverse the score
    end
    a.score = round(a.score/10); % round score to the nearest 10
    answers.(name) = a; % store the answer
    WaitSecs(0.5);
end
% return text size
Screen('TextSize', Sc.window, oldTextSize);
HideCursor;

%% calculate scores
score_breakdown.social = 0;
score_breakdown.economic = 0;
fld = fields(answers);
for ii = 1:length(fld)
    s = answers.(fld{ii}).score;
    if answers.(fld{ii}).social
        score_breakdown.social = score_breakdown.social + s;
    else
        score_breakdown.economic = score_breakdown.economic + s;
    end
end

score_breakdown.total = score_breakdown.social + score_breakdown.economic;
score = score_breakdown.total;
