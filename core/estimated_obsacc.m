function resp = estimated_obsacc()
%% Ask participant for their assessement of the advisors' accuracy
% - by Niccolo Pescetelli
%
% usage: resp = estimated_obsacc()
%
% This function asks the subject to rate the estimated accuracy of
% different advisors by inputing through keyboard. Advisors are presented
% according to the obstype order: 1.baseline, 2.agree in confidence, 3.
% agree in uncertainty

global cfg; % configuration object
global Sc; % Screen object

Screen('TextSize', Sc.window, cfg.instr.textSize.small);
help_string = '';

resp = [];

for o = 1:cfg.advisors.count.real
    valid = false;
    
    drawEstimPrompts(o);
    % flip screen
    Screen('Flip',Sc.window);
    
    % avoid button press overlaps
    WaitSecs(1);    
    drawEstimPrompts(o, true);
    Screen('Flip', Sc.window);
    
    % wait for button press
    collect_response(inf);
    oldTextSize = Screen('TextSize', Sc.window, cfg.instr.textSize.medium);
    
    while ~valid % continue prompting until valid value has entered
        % ask for estimated accuracy
        reply=Ask(Sc.window, [help_string cfg.instr.estimated_obsacc.text{5}], ...
            cfg.instr.textColor.default, cfg.display.color.background, ...
            'GetChar', 'center', 'center', cfg.instr.textSize.medium);
        try
            resp(o) = str2num(reply);
            valid = true;

            % check value is within range
            if resp(o)>=0 && resp(o)<=100
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
    
    Screen('TextSize', Sc.window, oldTextSize);
    
end


function drawEstimPrompts(advisorId, allowKeys)
global cfg; % configuration object
global Sc; % Screen object

if nargin < 2, allowKeys = false; end
% draw observer picture
drawAdvisor(advisorId);

oldTextSize = Screen('TextSize', Sc.window, cfg.instr.textSize.medium);

% Draw instructions
DrawFormattedText(Sc.window, cfg.instr.estimated_obsacc.text{2}, 'center', cfg.instr.estimated_obsacc.position.y(2))
DrawFormattedText(Sc.window, cfg.instr.estimated_obsacc.text{1}, 'center', cfg.instr.estimated_obsacc.position.y(1))
DrawFormattedText(Sc.window, cfg.instr.estimated_obsacc.text{3}, 'center', cfg.instr.estimated_obsacc.position.y(3))
if allowKeys
    DrawFormattedText(Sc.window, cfg.instr.estimated_obsacc.text{4}, 'center', cfg.instr.estimated_obsacc.position.y(4))
end

Screen('TextSize', Sc.window, oldTextSize);