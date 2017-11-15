%% questionnaire for trust rating and reliability
% created from a previous script on visibility (get_visiiblity.m)
% this function offer the subject the possibility to rate several questions
% on semicontinous sliding bars 

%%
%---- subjective ratings
maxS=100; % max score
minS=1; % min score
ns=100; % number of points
cw=cfg.bar.cursorwidth; % cursor [visual indicator] width
ch=cfg.bar.cursorheight; % cursor height 
brct=CenterRectOnPoint([0 0 (ns *cw) (ch)],Sc.center(1),Sc.center(2)+Sc.size(2)/5);
bl = brct(3)- brct(1); % length (width) of the scale rectangle
advisorCenter = [Sc.center(1) Sc.rect(4)*.4];
Screen('Flip', Sc.window);
oldTextSize = Screen('TextSize',Sc.window,cfg.instr.textSize.small);
Screen('TextFont', Sc.window, 'Myriad Pro');
ShowCursor('Arrow');
id=0;
question=[];
for obs= 1 : cfg.advisors.count.real
    for q = 1: 4
        id          = id+1;
        question(id).id = id;
        question(id).obs = obs;
        question(id).quest = q;
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
    
    if trials(t).block == 3
        questionList={'How accurate do you think this person will be performing the task?'...
            'How much are you going to like this person?'...
            'How trustworthy will be the opinions of this person?'...
            'How much will you be influenced by the opinions of this person?'};
    else
        questionList={'How accurate do you think this person was when performing the task?'...
            'How much do you like this person?'...
            'How trustworthy are the opinions of this person?'...
            'How much are you influenced by the opinions of this person?'};
    end
    Screen('TextSize',Sc.window,cfg.instr.textSize.medium);
    Qbounds         = Screen('TextBounds',Sc.window,questionList{q});
    Screen('TextSize',Sc.window,cfg.instr.textSize.small);
    rightText       = 'Extremely';
    middleRightText = 'Fairly';
    middleLeftText  = 'Not so much';
    leftText        = 'Not at all';
    RTbounds        = Screen('TextBounds',Sc.window,rightText);
    MRTbounds       = Screen('TextBounds',Sc.window,middleRightText);
    MLTbounds       = Screen('TextBounds',Sc.window,middleLeftText);
    LTbounds        = Screen('TextBounds',Sc.window,leftText);
    inst            = cfg.instr.instr;
    Ibounds{1}  = Screen('TextBounds',Sc.window,inst{1});
    Ibounds{2}  = Screen('TextBounds',Sc.window,inst{2});
  
    while true
        [x, ~, buttons] = GetMouseWrapper(Sc);
        [keydown, question(ii).response_t, keycode] = KbCheck;
        % only draw if buttons are held down or it's the first run
        if ~question(ii).haschanged || any(buttons) || keydown
            Screen('TextSize',Sc.window,cfg.instr.textSize.medium);
            Screen('DrawText', Sc.window, questionList{q}, Sc.center(1)- (Qbounds(3)/2), Sc.center(2)+(Sc.size(2)/9), 0);
            Screen('TextSize',Sc.window,cfg.instr.textSize.small);
            Screen('TextFont', Sc.window, 'Myriad Pro');
            %add response istructions
            Screen('DrawText', Sc.window, inst{1}, Sc.center(1)- (Ibounds{1}(3)/2), Sc.center(2)+0.35*(Sc.size(2)), 0);
            Screen('DrawText', Sc.window, inst{2}, Sc.center(1)- (Ibounds{2}(3)/2), Sc.center(2)+0.35*(Sc.size(2))+50, 0);
            if ~isnan(question(ii).ans)
                cursorrect = CenterRectOnPoint([0,0,cw,ch],...
                    Sc.center(1) -((ns*cw/2)+cw) + (question(ii).ans* cw  + cw/2), Sc.center(2)+ Sc.size(2)/5);
                rect = [brct' cursorrect'];
                Screen('FillRect', Sc.window, [[.2 .2 .2]' [.8 .8 .8]'],rect);
            else
                Screen('FillRect', Sc.window, [.2 .2 .2]', brct');
            end
            Screen('DrawText', Sc.window, rightText, Sc.center(1)+ bl/2 - RTbounds(3)/2, Sc.center(2)+(Sc.size(2)/7), 0);
            Screen('DrawText', Sc.window, middleRightText, Sc.center(1)+ bl/6 - MRTbounds(3)/2, Sc.center(2)+(Sc.size(2)/7), 0);
            Screen('DrawText', Sc.window, middleLeftText, Sc.center(1)- bl/6 - MLTbounds(3)/2, Sc.center(2)+(Sc.size(2)/7), 0);
            Screen('DrawText', Sc.window, leftText, Sc.center(1)- bl/2 - LTbounds(3)/2, Sc.center(2)+(Sc.size(2)/7), 0);
            drawAdvisor(Sc, cfg, obs, advisorCenter);
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
            end

            % check for keys
            if keydown
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
            % bound visibility
            if question(ii).ans > maxS % max
                question(ii).ans = maxS;
            elseif question(ii).ans < minS % minScale
                question(ii).ans = minS;
            end
        end
    end
    WaitSecs(0.5);
end
[Y, I] = sort([question.id]);
question(1:cfg.advisors.count.real*length(questionList)) = question(I); % sort questions in the original order
trials(t).qanswers = question;

%% pause before next stimuli session
Screen('TextSize',Sc.window,cfg.instr.textSize.medium);
if trials(t).block==3 && trials(t-1).block==2 % baseline questionnaire
    DrawFormattedText(Sc.window, 'Press any button to start the experiment','center', Sc.center(2)-50, [0 0 0]);
else
    DrawFormattedText(Sc.window, 'Press any button to continue','center', Sc.center(2)-50, [0 0 0]);
end
Screen('Flip', Sc.window);
WaitSecs(.25);
collect_response(cfg.response, inf);
Screen('Flip', Sc.window);
WaitSecs(.5);

Screen('TextSize', Sc.window, oldTextSize);
HideCursor;
clear tm cw ch bl ns maxS minS brct advisorCenter id question;