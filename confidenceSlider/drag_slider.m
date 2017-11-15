function [cj,resp_t,interval,hasconfirmed] = drag_slider(Sc,cfg,varargin)
% Usage:
% [cj resp_t interval hasconfirmed] = drag_slider(Sc,cfg [,cj1])
% Inputs:
% Sc: Sc structure
% cfg: cfg structure
% cj1: first confidence judgement. If present cj1 is shown in shaded color
%
% function by niccolo.pescetelli@psy.ox.ac.uk


if nargin < 3
    cj1 = [];
else
    cj1 = varargin{1};
end

%% Show mouse pointer
ShowCursor('Arrow');

%% initialize variables
resp = 0; buttons=[]; haschanged=false; hasconfirmed=false;int=0;

%% display cursor
if isempty(cj1)
    ft = display_response_(Sc,cfg,[haschanged,resp+int]);
else
    ft = display_response_(Sc,cfg,[haschanged,resp+int],cj1);
end

%% collect response
while ~any(buttons) % wait for click
    [x,y,buttons] = GetMouseWrapper(Sc);
end
while ~hasconfirmed
    while any(buttons) || ~haschanged   % wait for release and change of cj and confirmation
        [resp_x, resp_y, buttons] = GetMouseWrapper(Sc);
        
        if resp_x>=cfg.bar.barrect(1) && resp_x<Sc.center(1) % if mouse's on the left rect
            resp = find(resp_x < (cfg.bar.xshift+cfg.bar.cursorwidth.*.5),1) - cfg.bar.maxScale-1;
            haschanged = true;
            int = -1;
            if resp==0, resp=int;end
        elseif resp_x>=Sc.center(1) && resp_x<=cfg.bar.barrect(3) % if mouse's on the right rect
            resp = find(resp_x < (cfg.bar.xshift+cfg.bar.cursorwidth.*.5),1) - cfg.bar.maxScale;
            haschanged = true;
            int = 1;
            if isempty(resp), resp=cfg.bar.maxScale;end
        end
        
        %--- display response
        if isempty(cj1)
            ft = display_response_(Sc,cfg,[haschanged,resp]);
        else
            ft = display_response_(Sc,cfg,[haschanged,resp],cj1);
        end
    end
    
    % check for confirmation
    if ~hasconfirmed
        switch 'keyboard'
            case 'mouse'
                [x,y,buttons] = GetMouseWrapper(Sc);
                if buttons(3)==1, hasconfirmed = true;end
                resp_t = GetSecs;
            case 'keyboard'
                [x,y,buttons] = GetMouseWrapper(Sc);
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
    
end


%% compute confidence judgment
cj = resp ;

% change interval to [1 2] range
interval = 2-(int<0);

%% hide back cursor
HideCursor;

return