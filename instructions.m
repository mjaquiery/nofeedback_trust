%% provide instructions
switch trials(t).block
    case 1
        a = imread([cfg.path.base '/instructions/instr1a.PNG']);
        h = Screen('MakeTexture',Sc.window,a);
        Screen('DrawTexture',Sc.window,h,[],Sc.rect);
        Screen('Flip',Sc.window);
        collect_response(cfg,inf);
        Screen('Flip',Sc.window);
        
        a = imread([cfg.path.base '/instructions/instr1b.PNG']);
        h = Screen('MakeTexture',Sc.window,a);
        Screen('DrawTexture',Sc.window,h,[],Sc.rect);
        Screen('Flip',Sc.window);
        collect_response(cfg,inf);
        Screen('Flip',Sc.window);
        
        a = imread([cfg.path.base '/instructions/instr1c.PNG']);
        h = Screen('MakeTexture',Sc.window,a);
        Screen('DrawTexture',Sc.window,h,[],Sc.rect);
        Screen('Flip',Sc.window);
        collect_response(cfg,inf);
        Screen('Flip',Sc.window);
    case 2
        a = imread([cfg.path.base '/instructions/instr2a.PNG']);
        h = Screen('MakeTexture',Sc.window,a);
        Screen('DrawTexture',Sc.window,h,[],Sc.rect);
        Screen('Flip',Sc.window);
        collect_response(cfg,inf);
        Screen('Flip',Sc.window);
        
        a = imread([cfg.path.base '/instructions/instr2b.PNG']);
        h = Screen('MakeTexture',Sc.window,a);
        Screen('DrawTexture',Sc.window,h,[],Sc.rect);
        Screen('Flip',Sc.window);
        collect_response(cfg,inf);
        Screen('Flip',Sc.window);
    case 3
        a = imread([cfg.path.base '/instructions/instr3.PNG']);
        h = Screen('MakeTexture',Sc.window,a);
        Screen('DrawTexture',Sc.window,h,[],Sc.rect);
        Screen('Flip',Sc.window);
        collect_response(cfg,inf);
        Screen('Flip',Sc.window);
    case 4
        a = imread([cfg.path.base '/instructions/instr4.PNG']);
        h = Screen('MakeTexture',Sc.window,a);
        Screen('DrawTexture',Sc.window,h,[],Sc.rect);
        Screen('Flip',Sc.window);
        collect_response(cfg,inf);
        Screen('Flip',Sc.window);
    otherwise
end

add_fixation;
Screen('Flip',Sc.window);
WaitSecs(1.000);