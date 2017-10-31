%---- subjective rating of confidence on continuous double scale
t_ = Screen('Flip', Sc.window);


key = 1;
while sum(key) ~= 0
    [key resp_t keycode] = KbCheck;                 % present only when release button
end
key = 'firstMove';

add_bar

gs = round(gap_size/2);
resp = 0;while resp< gs && resp>-gs, resp = randi(nScale) - maxScale+1;end
while true
        %add response istructions
        Screen('TextSize', Sc.window, 13);Screen('TextFont', Sc.window, 'Myriad Pro');
        Screen('DrawText', Sc.window, instr{1}, Sc.center(1)- (Ibounds{1}(3)/2), Sc.center(2)+0.35*(Sc.size(2)), 0);
        Screen('DrawText', Sc.window, instr{2}, Sc.center(1)- (Ibounds{2}(3)/2), Sc.center(2)+0.35*(Sc.size(2))+50, 0);
%         disp(haschanged);
        % display response bar
        if strcmp (key,'firstMove')
            cursorrect = CenterRectOnPoint([0,0,cursorwidth,cursorheight],...
                Sc.center(1) -((nScale*cursorwidth/2)+cursorwidth) + ((resp+maxScale) * cursorwidth  + cursorwidth/2), Sc.center(2)+ Sc.size(2)/5);
            gap = CenterRectOnPoint([0,0,cursorwidth * gap_size,cursorheight],...
                Sc.center(1) -((nScale*cursorwidth/2)+cursorwidth) + (maxScale * cursorwidth  + cursorwidth/2), Sc.center(2)+ Sc.size(2)/5);
            rect = [barrect' cursorrect' gap'];
            Screen('FillRect', Sc.window, [[.2 .2 .2]' [.8 .8 .8]' [.5 .5 .5]'],rect);
            Screen('TextFont', Sc.window, 'Myriad Pro');
            
            % draw confidence landmarks
            Screen('DrawText', Sc.window, rTxt, Sc.center(1)+ barlength/2 - RTbounds(3)/2, Sc.center(2)+(Sc.size(2)/7), 0);
            Screen('DrawText', Sc.window, middleRT, Sc.center(1)+ barlength/14 - MRTbounds(3)/2, Sc.center(2)+(Sc.size(2)/7), 0);
            Screen('DrawText', Sc.window, middleLT, Sc.center(1)- barlength/12 - MLTbounds(3)/2, Sc.center(2)+(Sc.size(2)/7), 0);
            Screen('DrawText', Sc.window, lTxt, Sc.center(1)- barlength/2 - LTbounds(3)/2, Sc.center(2)+(Sc.size(2)/7), 0);
            % draw interval landmarks
            Screen('DrawText', Sc.window, interval{1}, Sc.center(1)- barlength/3.5 - LintBounds(3)/2, Sc.center(2)+(Sc.size(2)/4), 0);
            Screen('DrawText', Sc.window, interval{2}, Sc.center(1)+ barlength/3.5 - RintBounds(3)/2, Sc.center(2)+(Sc.size(2)/4), 0);
            
            
            % Flip on screen
            Screen('Flip', Sc.window, resp_t +.05); % add lag to avoid too fast moving of the cursor
        end

        if strcmp(key, 'Left') || strcmp(key, 'Right')
            cursorrect = CenterRectOnPoint([0,0,cursorwidth,cursorheight],...
                Sc.center(1)- ((nScale*cursorwidth/2)+cursorwidth) + ((resp+maxScale) * cursorwidth  + cursorwidth/2), Sc.center(2)+ Sc.size(2)/5);
            gap = CenterRectOnPoint([0,0,cursorwidth * gap_size,cursorheight],...
                Sc.center(1) -((nScale*cursorwidth/2)+cursorwidth) + (maxScale * cursorwidth  + cursorwidth/2), Sc.center(2)+ Sc.size(2)/5);
            rect = [barrect' cursorrect' gap'];
            Screen('FillRect', Sc.window, [[.2 .2 .2]' [.8 .8 .8]' [.5 .5 .5]'],rect);
            Screen('TextFont', Sc.window, 'Myriad Pro');

            % draw confidence landmarks
            Screen('DrawText', Sc.window, rTxt, Sc.center(1)+ barlength/2 - RTbounds(3)/2, Sc.center(2)+(Sc.size(2)/7), 0);
            Screen('DrawText', Sc.window, middleRT, Sc.center(1)+ barlength/14 - MRTbounds(3)/2, Sc.center(2)+(Sc.size(2)/7), 0);
            Screen('DrawText', Sc.window, middleLT, Sc.center(1)- barlength/12 - MLTbounds(3)/2, Sc.center(2)+(Sc.size(2)/7), 0);
            Screen('DrawText', Sc.window, lTxt, Sc.center(1)- barlength/2 - LTbounds(3)/2, Sc.center(2)+(Sc.size(2)/7), 0);
            % draw interval landmarks
            Screen('DrawText', Sc.window, interval{1}, Sc.center(1)- barlength/3.5 - LintBounds(3)/2, Sc.center(2)+(Sc.size(2)/4), 0);
            Screen('DrawText', Sc.window, interval{2}, Sc.center(1)+ barlength/3.5 - RintBounds(3)/2, Sc.center(2)+(Sc.size(2)/4), 0);
            
            Screen('Flip', Sc.window, resp_t+.05); % add lag to avoid too fast moving of the cursor
        end
        
        % wait for key press
        [key keycode resp_t] = deal(0);                     % start collecting keyboard response
        while sum(key) == 0
            [key resp_t keycode] = KbCheck;                 % get timing and key
        end
        %update answer
        key = KbName(keycode);
        if iscell(key), key = key{1}; end                                        % if two buttons at the same time
        switch key                                                  % sort answer
            case 'LeftArrow',       key = 'Left'; resp = resp -1; haschanged = 1;
            case 'space',           key = 'space';
                if haschanged
                    break;
                else
                    haschanged = 1;
                    KbReleaseWait;
                end
            case 'RightArrow',        key = 'Right'; resp = resp +1; haschanged = 1;
            case 'ESCAPE'
                sca
        end
        % bound visibility
        if resp > maxScale % max
            resp = maxScale;
        elseif resp < minScale % minScale
            resp = minScale;
        end
        % avoid zero
        if resp< gs && resp>-gs && strcmp(key,'Left')
            resp = resp -gap_size;
        elseif resp< gs && resp>-gs && strcmp(key,'Right')
            resp = resp +gap_size;
        end
resp
    end