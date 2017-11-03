function resp = estimated_obsacc(Sc, cfg)
% This function asks the subejct to rate the estimated accuracy of
% different advisors by inputing through keyboard. Advisors are presented
% according to the obstype order: 1.baseline, 2.agree in confidence, 3.
% agree in uncertainty

Screen('TextSize', Sc.window, 13);
help_string = '';

resp = [];

for o = 1:cfg.nobs
    valid = false;
    
    % load observer picture
    obs_data = imread([cfg.stims_path '/observer' num2str(cfg.observer.pic(o)) '.jpg']);
    
    % make texture image out of image matrix 'observer data'
    obs_tex  = Screen('MakeTexture', Sc.window, obs_data);    
    
    % draw observer picture
    Screen('DrawTexture', Sc.window, obs_tex, [], CenterRectOnPoint([0 0 258 325],Sc.center(1),Sc.center(2)));
    
    % Draw instructions
    DrawFormattedText(Sc.window,cfg.instr.estimated_obsacc{1},'center', Sc.rect(4) .* .1)
    Screen('TextSize', Sc.window, 20);
    DrawFormattedText(Sc.window,cfg.instr.estimated_obsacc{2},'center', Sc.rect(4) .* .15)
    Screen('TextSize', Sc.window, 13);
    DrawFormattedText(Sc.window,cfg.instr.estimated_obsacc{3},'center', Sc.rect(4) .* cfg.bar.positiony)
    DrawFormattedText(Sc.window,cfg.instr.estimated_obsacc{4},'center', Sc.rect(4) .* cfg.bar.positiony + 50)
    
    % flip screen
    ft = Screen('Flip',Sc.window);
    
    % avoid button press overlaps
    WaitSecs(2);
    
    % wait for button press
    collect_response(cfg,inf);
    
    while ~valid % continue prompting until valid value has entered
        % ask for estimated accuracy
        reply=Ask(Sc.window,[help_string cfg.instr.estimated_obsacc{5}],[],[.5 .5 .5],'GetChar','center','center',13);
        try
            resp(o) = str2num(reply);
            valid = true;
            help_string = '';

            % check value is within range
            if resp(o)>=0 && resp(o)<=100,
                valid = true;
                help_string = '';
            else
                valid = false;
                help_string = 'Value out of range! ';
            end
        catch
            valid = false;
            help_string = 'Invalid character! ';
        end
    end
    
end

return