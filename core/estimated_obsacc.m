function resp = estimated_obsacc(advisorId)
%% Ask participant for their assessement of the advisors' accuracy
% - by Niccolo Pescetelli
% updated: Matt Jaquiery, Jan 2018
%
% usage: resp = estimated_obsacc([advisorId])
%
% Inputs: 
% advisorId: if provided, only query for the specified advisor
%
% Outputs:
% resp: the response (0-100) provided by the participant 
%
% This function asks the subject to rate the estimated accuracy of
% different advisors by inputing through keyboard. Advisors are presented
% according to the obstype order: 1.baseline, 2.agree in confidence, 3.
% agree in uncertainty

global cfg; % configuration object
global Sc; % Screen object

oldTextSize = Screen('TextSize', Sc.window, cfg.instr.textSize.medium);

if nargin < 1
    resp = [];
    for advisorId = 1:cfg.advisors.count.real
        resp(advisorId) = getAdvisorAccuracyEstimate(advisorId);
    end
else
    resp = getAdvisorAccuracyEstimate(advisorId);
end

Screen('TextSize', Sc.window, oldTextSize);

function resp = getAdvisorAccuracyEstimate(advisorId)
global cfg; % configuration object
global Sc; % Screen object

if nargin < 1, error('No advisorId specified'); end
help_string = '';
valid = false;

bounds = Screen('TextBounds', Sc.window, [cfg.instr.estimated_obsacc.ask.text '   ']);
bounds = CenterRectOnPoint(bounds, Sc.center(1), Sc.center(2));

while true
    % Draw instructions
    drawAdvisor(advisorId);
    DrawFormattedText(Sc.window, cfg.instr.estimated_obsacc.text{2}, 'center', cfg.instr.estimated_obsacc.position.y(2))
    DrawFormattedText(Sc.window, cfg.instr.estimated_obsacc.text{1}, 'center', cfg.instr.estimated_obsacc.position.y(1))
    if ~isempty(help_string)
        DrawFormattedText(Sc.window, help_string, 'center', cfg.instr.estimated_obsacc.position.y(3))
    end
    
    reply = GetEchoString(Sc.window,cfg.instr.estimated_obsacc.ask.text,bounds(1),cfg.instr.estimated_obsacc.ask.y);
    Screen('Flip', Sc.window);
    try
        resp = str2num(reply);

        % check value is within range
        if resp>=0 && resp<=100
            return
        else
            help_string = 'Value out of range! ';
        end
    catch
        help_string = 'Invalid character! ';
    end
end
