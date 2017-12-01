function [score, score_breakdown, answers] = SECS(showInstructions)
%% Presentation of the Social + Economic Conseravtism Scale
% - by Matt Jaquiery
%
% usage: [score, score_breakdown, answers] = SECS([showInstructions])
%
% Inputs:
% showInstructions: whether to show participant instructions for filling in
% the SECS. Defaults to false.
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

if nargin < 1, showInstructions = 0; end

defineSECS();

if showInstructions
    instructions('SECS');
end

answers = struct;

Screen('Flip', Sc.window);
oldTextSize = Screen('TextSize',Sc.window);
Screen('TextFont', Sc.window, 'Myriad Pro');
ShowCursor('Arrow');

%% Show questionnaire
for ii = 1: length(cfg.SECS.questions.text)
    a = struct;
    a.text = cfg.SECS.questions.text{cfg.SECS.question_order(ii)};
    a.presentation_order = ii;
    a.social = cfg.SECS.socialMask(cfg.SECS.question_order(ii));
    a.reverse_scored = cfg.SECS.revMask(cfg.SECS.question_order(ii));
    a.rt = NaN; % initialize response time variable
    a.score = NaN;
    name = cfg.SECS.questions.name{cfg.SECS.question_order(ii)};
    haschanged = false;   % to avoid automatic responses
    onset_t = NaN;
  
    %% detect response
    while true
        [x, ~, buttons] = GetMouseWrapper;
        [keydown, a.rt, keycode] = KbCheck;
        % only draw if buttons are held down or it's the first run
        if ~haschanged || any(buttons) || keydown
            
            tm = drawSECS(cfg.SECS.question_order(ii), a.score);
            % record onset time if necessary
            if isnan(onset_t)
                onset_t = tm;
            end

            %update answer if we clicked            
            if any(buttons) % button clicked
                if x < cfg.SECS.brct(1) % boundaries of the slider
                    x = cfg.SECS.brct(1);
                elseif x > cfg.SECS.brct(3)
                    x = cfg.SECS.brct(3);
                end
                ans = x - cfg.SECS.brct(1); % where abouts on the scale are we?
                if ans ~= a.score
                    haschanged = 1;
                end
                a.score = ceil(ans/cfg.SECS.cw);
                if cfg.debug
                    Screen('DrawText', Sc.window, ['x:' int2str(ceil(x)) '; a:' int2str(ans) '; btn:' int2str(any(buttons))], 0, 0);
                end
                
                % bound visibility
                if a.score > cfg.SECS.maxS % max
                    a.score = cfg.SECS.maxS;
                elseif a.score < cfg.SECS.minS % minScale
                    a.score = cfg.SECS.minS;
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
        a.score = abs(a.score - cfg.SECS.maxS); % reverse the score
    end
    %a.score = round(a.score/10); % round score to the nearest 10
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
