function [cj,resp_t,interval,hasconfirmed] = drag_slider(cj1, trial)
% Usage:
% [cj resp_t interval hasconfirmed] = drag_slider([cj1, [trial]])
% Inputs:
% cj1: first confidence judgement. If present cj1 is shown in shaded color
% trial: if included, data from trial can be used to draw an advice overlay
%
% Outputs:
% cj: current value of the slider position
% resp_t: response time
% interval: whether the answer selected is left (1) or right (2)
% hasconfirmed: whether the answer has been confirmed
%
% function by niccolo.pescetelli@psy.ox.ac.uk

global cfg; % configuration object
global Sc; % Screen object

if nargin < 2, trial = []; end
if nargin < 1, cj1 = []; end

%% initialize variables
resp = 0; buttons=[]; haschanged=false; hasconfirmed=false; int=0;

%% display cursor
if isempty(cj1)
    % Show mouse pointer
    ShowCursorCenter('Arrow');
    ft = display_response_([haschanged,resp+int]);
else
    % Show mouse pointer
    ShowCursor('Arrow');
    if isempty(trial)
        ft = display_response_([haschanged,resp],cj1);
    else
        ft = display_response_([haschanged, resp], cj1, trial);
    end
end

%% collect response
while ~any(buttons) % wait for click
    [~, ~, buttons] = GetMouseWrapper;
end

while ~hasconfirmed
    while any(buttons) || ~haschanged   % wait for release and change of cj and confirmation
        [resp_x, ~, buttons] = GetMouseWrapper;
        
        if resp_x>=cfg.bar.barrect(1) && resp_x<Sc.center(1) % if mouse is on the left rect
            resp = find(resp_x < (cfg.bar.xshift+cfg.bar.cursorwidth.*.5),1) - cfg.bar.maxScale-1;
            haschanged = true;
            int = -1;
            if resp==0, resp=int;end
        elseif resp_x>=Sc.center(1) && resp_x<=cfg.bar.barrect(3) % if mouse is on the right rect
            resp = find(resp_x < (cfg.bar.xshift+cfg.bar.cursorwidth.*.5),1) - cfg.bar.maxScale;
            haschanged = true;
            int = 1;
            if isempty(resp), resp=cfg.bar.maxScale;end
        end
        
        %--- display response
        if isempty(cj1)
            ft = display_response_([haschanged,resp]);
        else
            if isempty(trial)
                ft = display_response_([haschanged,resp],cj1);
            else
                ft = display_response_([haschanged, resp], cj1, trial);
            end
        end
    end
    
    % check for confirmation
    if ~hasconfirmed
        [x,y,buttons] = GetMouseWrapper;
        [isdown resp_t keycode] = KbCheck;                 % get timing and key
        % translate key code into key name
        name = KbName(keycode);
        % only take first response if multiple responses
        if ~iscell(name), name = {name}; end
        name = name{1};
        if strcmp('space',name),hasconfirmed = true;end
        if strcmp('ESCAPE',name),sca;end

        %until release
        if cfg.until_release
            [resp_release x name] = KbCheck;          % get cfg.timing and resp1 from keyboard
            if sum(resp_release) == 1
                if strcmp('',KbName(name))
                    resp_release = 0;
                end
            end
        end
    end
    
end


%% compute confidence judgment
cj = resp ;

% change interval to [1 2] range
interval = 2-(int<0);

%% hide cursor again
HideCursor;
