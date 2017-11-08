%% questionnaire for trust rating and reliability
% created from a previous script on visibility (get_visiiblity.m)
% this function offer the subject the possibility to rate several questions
% on semicontinous sliding bars 

%%
%---- subjective ratings
maxS=50;
minS=1;
ns=50; 
cw=Sc.size(1)/100;
ch=5;
brct=CenterRectOnPoint([0 0 (ns *cw) (ch)],Sc.center(1),Sc.center(2)+Sc.size(2)/5);
bl = brct(3)- brct(1);
advisorCenter = [Sc.center(1) Sc.rect(4)*.4];
Screen('Flip', Sc.window);
id=0;
question=[];
for obs= 1 : 3
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
    question(ii).ans = randi(ns);
    question(ii).initial_position = question(ii).ans; % remember the initial position so we can check anchoring later
    question(ii).presentation_order = ii; %record the presentation order
    question(ii).haschanged = 0;                % to avoid automatic responses
    question(ii).response_t = NaN;              % initialize response time variable
    key = 1;
    while sum(key) ~= 0
        [T, key, question(ii).response_t, keycode] = evalc('KbCheck');        % present only when release button
    end
    key = 'firstMove';
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
    Screen('TextSize', Sc.window, 13);
    rightText       = 'Extremely';
    middleRightText = 'Fairly';
    middleLeftText  = 'Not so much';
    leftText        = 'Not at all';
    Qbounds         = Screen('TextBounds',Sc.window,questionList{q});
    RTbounds        = Screen('TextBounds',Sc.window,rightText);
    MRTbounds       = Screen('TextBounds',Sc.window,middleRightText);
    MLTbounds       = Screen('TextBounds',Sc.window,middleLeftText);
    LTbounds        = Screen('TextBounds',Sc.window,leftText);
    inst{1}    = 'Move the cursor with Left and Right Arrows (keyboard).';
    inst{2}    = 'Press spacebar to provide response.';
    Ibounds{1}  = Screen('TextBounds',Sc.window,inst{1});
    Ibounds{2}  = Screen('TextBounds',Sc.window,inst{2});
    while true
        %add response istructions
        Screen('TextSize', Sc.window, 13);Screen('TextFont', Sc.window, 'Myriad Pro');
        Screen('DrawText', Sc.window, inst{1}, Sc.center(1)- (Ibounds{1}(3)/2), Sc.center(2)+0.35*(Sc.size(2)), 0);
        Screen('DrawText', Sc.window, inst{2}, Sc.center(1)- (Ibounds{2}(3)/2), Sc.center(2)+0.35*(Sc.size(2))+50, 0);
        disp(question(ii).haschanged);
        % display response bar
        if strcmp (key,'firstMove')
            cursorrect = CenterRectOnPoint([0,0,cw,ch],...
                Sc.center(1) -((ns*cw/2)+cw) + (question(ii).ans* cw  + cw/2), Sc.center(2)+ Sc.size(2)/5);
            rect = [brct' cursorrect'];
            Screen('FillRect', Sc.window, [[.2 .2 .2]' [.8 .8 .8]'],rect);
            Screen('TextFont', Sc.window, 'Myriad Pro');
            Screen('DrawText', Sc.window, questionList{q}, Sc.center(1)- (Qbounds(3)/2), Sc.center(2)+(Sc.size(2)/9), 0);
            Screen('DrawText', Sc.window, rightText, Sc.center(1)+ bl/2 - RTbounds(3)/2, Sc.center(2)+(Sc.size(2)/7), 0);
            Screen('DrawText', Sc.window, middleRightText, Sc.center(1)+ bl/6 - MRTbounds(3)/2, Sc.center(2)+(Sc.size(2)/7), 0);
            Screen('DrawText', Sc.window, middleLeftText, Sc.center(1)- bl/6 - MLTbounds(3)/2, Sc.center(2)+(Sc.size(2)/7), 0);
            Screen('DrawText', Sc.window, leftText, Sc.center(1)- bl/2 - LTbounds(3)/2, Sc.center(2)+(Sc.size(2)/7), 0);
            drawAdvisor(Sc, cfg, obs, advisorCenter);

            Screen('Flip', Sc.window, question(ii).response_t + .100); % add lag to avoid too fast moving of the cursor
        end

        if strcmp(key, 'bottom') || strcmp(key, 'top')
            cursorrect = CenterRectOnPoint([0,0,cw,ch],...
                Sc.center(1)- ((ns*cw/2)+cw) + (question(ii).ans* cw  + cw/2), Sc.center(2)+ Sc.size(2)/5);
            rect = [brct' cursorrect'];
            Screen('FillRect', Sc.window, [[.2 .2 .2]' [.8 .8 .8]'],rect);
            Screen('TextFont', Sc.window, 'Myriad Pro');
            Screen('DrawText', Sc.window, questionList{q}, Sc.center(1)- (Qbounds(3)/2), Sc.center(2)+(Sc.size(2)/9), 0);
            Screen('DrawText', Sc.window, rightText, Sc.center(1)+ bl/2 - RTbounds(3)/2, Sc.center(2)+(Sc.size(2)/7), 0);
            Screen('DrawText', Sc.window, middleRightText, Sc.center(1)+ bl/6 - MRTbounds(3)/2, Sc.center(2)+(Sc.size(2)/7), 0);
            Screen('DrawText', Sc.window, middleLeftText, Sc.center(1)- bl/6 - MLTbounds(3)/2, Sc.center(2)+(Sc.size(2)/7), 0);
            Screen('DrawText', Sc.window, leftText, Sc.center(1)- bl/2 - LTbounds(3)/2, Sc.center(2)+(Sc.size(2)/7), 0);
            drawAdvisor(Sc, cfg, obs, advisorCenter);

            Screen('Flip', Sc.window, question(ii).response_t + .025); % add lag to avoid too fast moving of the cursor
        end

        % wait for key press
        [key, keycode, question(ii).response_t] = deal(0);                   % start collecting keyboard response
        while sum(key) == 0
            [T, key, question(ii).response_t, keycode] = evalc('KbCheck');    % get timing and key
        end
        %update answer
        key = KbName(keycode);
        if iscell(key), key = key{1}; end                                        % if two buttons at the same time
        switch key                                                  % sort answer
            case 'LeftArrow'       
                key = 'bottom';
                question(ii).ans = question(ii).ans -1;
                question(ii).haschanged = 1;
            case 'space'           
                key = 'space';
                if question(ii).haschanged
                    break;
                else
                    question(ii).haschanged = 1;
                    KbReleaseWait;
                end
            case 'RightArrow'        
                key = 'top';
                question(ii).ans = question(ii).ans +1;
                question(ii).haschanged = 1;
            case 'ESCAPE'
                sca
        end
        % bound visibility
        if question(ii).ans > maxS % max
            question(ii).ans = maxS;
        elseif question(ii).ans < minS % minScale
            question(ii).ans = minS;
        end

    end
end
[Y, I] = sort([question.id]);
question(1:3*length(questionList)) = question(I); % sort questions in the original order
trials(t).qanswers = question;

%% pause before next stimuli session
Screen('TextSize',Sc.window,18);
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
