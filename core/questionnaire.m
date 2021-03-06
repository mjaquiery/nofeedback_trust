function question = questionnaire(advisorId)
%% questionnaire for trust rating and reliability
% 
% usage: questionnaire([advisorId])
% 
% Inputs: 
% advisorId - ID of the advisor to query participant about (if not set,
% query all advisors)
%
% created from a previous script on visibility (get_visiiblity.m)
% this function offer the subject the possibility to rate several questions
% on semicontinous sliding bars 

global cfg;
global Sc;

if nargin < 1, advisorId = NaN; end

%%
%---- subjective ratings
maxS=100; % max score
minS=1; % min score
ns=100; % number of points
cw=cfg.bar.cursorwidth; % cursor [visual indicator] width
ch=cfg.bar.cursorheight; % cursor height 
brct=CenterRectOnPoint([0 0 (ns *cw) (ch)], Sc.center(1), cfg.bar.Q.y);
bl = brct(3)- brct(1); % length (width) of the scale rectangle
advisorCenter = [Sc.center(1) cfg.instr.Q.position.advisorY];
Screen('Flip', Sc.window);
oldTextSize = Screen('TextSize',Sc.window,cfg.instr.textSize.small);
Screen('TextFont', Sc.window, 'Myriad Pro');
id=0;
question=[];
if isnan(advisorId)
    for obs= 1 : cfg.advisors.count.real
        for q = 1: 4
            id                  = id+1;
            question(id).id     = id;
            question(id).obs    = obs;
            question(id).quest  = q;
        end
    end
else
    for q = 1:4        
        id                  = id + 1;
        question(q).id      = id;
        question(q).obs     = advisorId;
        question(q).quest   = q;
    end
end
question = question(randperm(length(question))); % randomize questions presentation
clear id
for ii = 1: length(question)
    obs = question(ii).obs;
    q = question(ii).quest;
    question(ii).ans = NaN;
    question(ii).initial_position = question(ii).ans; % remember the initial position so we can check anchoring later
    question(ii).presentation_order = ii; %record the presentation order
    question(ii).haschanged = 0;                % to avoid automatic responses
    question(ii).response_t = NaN;              % initialize response time variable
    question(ii).onset_t = NaN;
    
    if ~cfg.advisor(obs).firstQuestionnaireDone
        questionList = cfg.instr.Q.q.pro.text;
    else
        questionList = cfg.instr.Q.q.retro.text;
    end
    Screen('TextSize',Sc.window,cfg.instr.textSize.medium);
    Qbounds         = Screen('TextBounds',Sc.window,questionList{q});
    Screen('TextSize',Sc.window,cfg.instr.textSize.small);
    rightText       = cfg.instr.Q.a.text{1};
    middleRightText = cfg.instr.Q.a.text{2};
    middleLeftText  = cfg.instr.Q.a.text{3};
    leftText        = cfg.instr.Q.a.text{4};
    RTbounds        = Screen('TextBounds',Sc.window,rightText);
    MRTbounds       = Screen('TextBounds',Sc.window,middleRightText);
    MLTbounds       = Screen('TextBounds',Sc.window,middleLeftText);
    LTbounds        = Screen('TextBounds',Sc.window,leftText);
    inst            = cfg.instr.instr.text;
    Ibounds{1}  = Screen('TextBounds',Sc.window,inst{1});
    Ibounds{2}  = Screen('TextBounds',Sc.window,inst{2});
  
    % show cursor
    ShowCursorCenter('Arrow', [NaN brct(2)+(brct(4)-brct(2))/2]);
    
    while true
        [x, ~, buttons] = GetMouseWrapper;
        [keydown, question(ii).response_t, keycode] = KbCheck;
        % only draw if buttons are held down or it's the first run
        if ~question(ii).haschanged || any(buttons) || keydown
            Screen('TextSize',Sc.window,cfg.instr.textSize.medium);
            Screen('DrawText', Sc.window, questionList{q}, Sc.center(1)- (Qbounds(3)/2), cfg.instr.Q.position.q.y, 0);
            Screen('TextSize',Sc.window,cfg.instr.textSize.small);
            Screen('TextFont', Sc.window, 'Myriad Pro');
            %add response istructions
            Screen('DrawText', Sc.window, inst{1}, Sc.center(1)- (Ibounds{1}(3)/2), ...
                cfg.instr.Q.position.instr.y, 0);
            Screen('DrawText', Sc.window, inst{2}, Sc.center(1)- (Ibounds{2}(3)/2), ...
                cfg.instr.Q.position.instr.y+cfg.instr.Q.position.instr.gap, 0);
            if ~isnan(question(ii).ans)
                cursorrect = CenterRectOnPoint([0,0,cw,ch],...
                    Sc.center(1) -((ns*cw/2)+cw) + (question(ii).ans* cw  + cw/2), cfg.bar.Q.y);
                rect = [brct' cursorrect'];
                Screen('FillRect', Sc.window, [cfg.bar.color.bar cfg.bar.color.cursor],rect);
            else
                Screen('FillRect', Sc.window, cfg.bar.color.bar, brct');
            end
            Screen('DrawText', Sc.window, rightText, Sc.center(1)+ bl/2 - RTbounds(3)/2, ...
                cfg.bar.Q.y-cfg.instr.Q.position.labelOffsetY, 0);
            Screen('DrawText', Sc.window, middleRightText, Sc.center(1)+ bl/6 - MRTbounds(3)/2, ...
                cfg.bar.Q.y-cfg.instr.Q.position.labelOffsetY, 0);
            Screen('DrawText', Sc.window, middleLeftText, Sc.center(1)- bl/6 - MLTbounds(3)/2, ...
                cfg.bar.Q.y-cfg.instr.Q.position.labelOffsetY, 0);
            Screen('DrawText', Sc.window, leftText, Sc.center(1)- bl/2 - LTbounds(3)/2, ...
                cfg.bar.Q.y-cfg.instr.Q.position.labelOffsetY, 0);
            drawAdvisor(obs, advisorCenter);
            %disp(question(ii).haschanged);
            % display response bar
            tm = Screen('Flip', Sc.window);
            % record onset time if necessary
            if isnan(question(ii).onset_t)
                question(ii).onset_t = tm;
            end

            %update answer if we clicked            
            if any(buttons) % button clicked
                if x < brct(1) % boundaries of the slider
                    x = brct(1);
                elseif x > brct(3)
                    x = brct(3);
                end
                a = x - brct(1); % where abouts on the scale are we?
                if a ~= question(ii).ans
                    question(ii).haschanged = 1;
                end
                question(ii).ans = ceil(a/cw);
                if cfg.debug
                    Screen('DrawText', Sc.window, ['x:' int2str(ceil(x)) '; a:' int2str(a) '; btn:' int2str(any(buttons))], 0, 0);
                end
                
                % bound visibility
                if question(ii).ans > maxS % max
                    question(ii).ans = maxS;
                elseif question(ii).ans < minS % minScale
                    question(ii).ans = minS;
                end
            end

            % check for keys
            if keydown && ~isnan(question(ii).ans)
                key = KbName(keycode);
                if iscell(key), key = key{1}; end % if two buttons at the same time
                switch key
                    case 'space'                        
                        if ~question(ii).haschanged
                            % mark that a change occurred
                            question(ii).haschanged = 1;
                        else
                            question(ii).response_t = GetSecs-question(ii).onset_t;
                            break;
                        end
                    case 'ESCAPE'
                        sca
                end
            end
        end
    end
    WaitSecs(0.5);
end
% sort questions in the original order
[Y, I] = sort([question.id]);
if isnan(advisorId)
    question(1:cfg.advisors.count.real*length(questionList)) = question(I);
    for i = 1:cfg.advisors.count.real
        cfg.advisor(i).firstQuestionnaireDone = true;
    end
else
    question(1:length(questionList)) = question(I);
    cfg.advisor(advisorId).firstQuestionnaireDone = true;
end

%% pause before next stimuli session
Screen('TextSize',Sc.window,cfg.instr.textSize.medium);
DrawFormattedText(Sc.window, 'Press any button to continue','center', Sc.center(2)-50, [0 0 0]);

Screen('Flip', Sc.window);
WaitSecs(.25);
collect_response(inf);
Screen('Flip', Sc.window);
WaitSecs(.5);

Screen('TextSize', Sc.window, oldTextSize);
HideCursor;
clear tm cw ch bl ns maxS minS brct advisorCenter id;