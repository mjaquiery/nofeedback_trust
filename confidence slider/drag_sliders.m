function [responses resp_t rt confirm_t interval C] = drag_sliders(Sc,cfg,varargin)
% [responses resp_t rt confirm_t interval C] = drag_sliders(Sc,cfg,[,condition],[,cj1],[,delay])
%
% responses is a 2-subjects by n-confidence time points matrix
%
% resp_t is the 2 x n matrix with the timestamps of the associated
% confidence levels
%
% rt is a 2x1 vector representing each subject's RTs
%
% confirm_t is the 2x1 timestamp of subjects' confirmation press. Only
% valid for first choices.
%
% interval is a 2x1 vector representing each subject's interval [1 or 2]
%
% C is a structure giving some stats about loop execution time. It ought to
% be smaller than cfg.samplingFreq for meaningful confidence recording
%
% (c) Niccolo Pescetelli niccolo.pescetelli@psy.ox.ac.uk
%

if nargin < 3
    condition   = '';
    cj1 = deal([]);
    delay       = inf;
elseif nargin < 4
    condition   = varargin{1};
    cj1 = deal([]);
    delay       = inf;
elseif nargin < 5
    condition   = varargin{1};
    cj1         = varargin{2};
    delay       = inf;
else
    condition   = varargin{1};
    cj1         = varargin{2};
    delay       = varargin{3};
end

%% initialize variables
if isempty(cj1),[resp,int]=deal(zeros(2,1));else int=sign(cj1);resp=cj1-int;end
[havechanged haveconfirmed]             = deal([false;false]);
[button buttons resp_t responses]       = deal([]);

% terminated condition: depending on subjects confirmation (first decision)
% or based on elapsed time (second response). Initialized as false
terminated                              = false;

%% Show mouse pointer
if isempty(cj1)
    % set initial position based on screen center
    SetMouse(Sc.rect(3)*.25,Sc.rect(4)*.5,Sc.window,cfg.master_id(1));
    SetMouse(Sc.rect(3)*.75,Sc.rect(4)*.5,Sc.window,cfg.master_id(2));
else
    % set initial position based on 1st decision
    SetMouse(round(cfg.bar.xshift(cfg.bar.maxScale+cj1(1)+(cj1(1)<0),1)),...
        Sc.rect(4)*cfg.bar.positiony,Sc.window,cfg.master_id(1));
    SetMouse(round(cfg.bar.xshift(cfg.bar.maxScale+cj1(2)+(cj1(2)<0),2)),...
        Sc.rect(4)*cfg.bar.positiony,Sc.window,cfg.master_id(2));
end

% hide real cursors
HideCursor(Sc.window,cfg.master_id(1));
HideCursor(Sc.window,cfg.master_id(2));

% record mouse position
for i=1:2
    [X(i,1),Y(i,1)] = GetMouse(Sc.window,cfg.master_id(i));
end

%% display cursors
if isempty(cj1)
    ft = display_responses(Sc,cfg,[havechanged,resp+int, X, Y]);
else
    ft = display_responses(Sc,cfg,[havechanged,cj1, X, Y],condition,cj1);
end

%% wait for first click in 1st decisions
if isempty(cj1)
    % wait for any button release
    while ~any(buttons) % wait for click
        %------get mouse position
        for i=1:2
            [X(i,1),Y(i,1),button{i}] = GetMouse(Sc.window,cfg.master_id(i));
        end
        buttons=[button{1}(1:3);button{2}(1:3)];
        
        %------ bound X and Y to own screen
        if X(1)>Sc.center(1),X(1)=Sc.center(1);end
        if X(2)<Sc.center(1),X(2)=Sc.center(1);end
        
        %----- display cursors
        if isempty(cj1)
            ft = display_responses(Sc,cfg,[havechanged,resp+int, X, Y]);
        else
            ft = display_responses(Sc,cfg,[havechanged,resp+int, X, Y],condition,cj1);
        end
    end
end

%% start recording time
starttime       = GetSecs;
cycle           = [];

%% update cursor until termination
while ~terminated                         % wait for confirmation/delay
    tic
    %% check mouse click in both mice
    for i=1:2
        [X(i,1),Y(i,1),button{i}] = GetMouse(Sc.window,cfg.master_id(i));
    end
    buttons=[button{1}(1:3);button{2}(1:3)];
    
    %% bound X and Y to own screen
    if X(1)>Sc.center(1),X(1)=Sc.center(1);end
    if X(2)<Sc.center(1),X(2)=Sc.center(1);end
    
    %% no need to click in case of second decision
    if ~isempty(cj1),for i=1:2,buttons=ones(2,3);end, end
    
    %% update response of anybody who clicked
    for i=find(any(buttons,2))'
        if ~isempty(i)
            if X(i)>=cfg.bar.barrect(i,1) && X(i)<(Sc.center(1)*(i-.5)) % if mouse's on the left rect
                resp(i) = find(X(i) < (cfg.bar.xshift(:,i)+cfg.bar.cursorwidth.*.5),1) - cfg.bar.maxScale-1;
                havechanged(i)    = true;
                int(i)            = -1;
                if resp(i)==0, resp(i)=int(i);end
            elseif X(i)>(Sc.center(1)*(i-.5)) && X(i)<cfg.bar.barrect(i,3) % if mouse's on the right rect
                resp(i) = find(X(i) < (cfg.bar.xshift(:,i)+cfg.bar.cursorwidth.*.5),1) - cfg.bar.maxScale;
                havechanged(i)    = true;
                int(i)            = 1;
            end
        end
    end
    
    %% display response
    if isempty(cj1)
        ft = display_responses(Sc,cfg,[havechanged,resp, X, Y]);
    else
        ft = display_responses(Sc,cfg,[havechanged,resp, X, Y],condition,cj1);
    end
    
    %% record response every 200ms (defined in cfg.samplingFreq)
    if (GetSecs - starttime)>(cfg.samplingFreq)*size(responses,2)
        responses   = cat(2,responses,resp);
        resp_t      = cat(2,resp_t,GetSecs);
    end
    
    %% check for confirmation if it's a first decision
    if isempty(cj1)
        %update key presses
        [keyIsDown_s1, t, keyCode_s1] = deal(0);                   % start collecting keyboard response
        [keyIsDown_s2, t, keyCode_s2] = deal(0);                   % start collecting keyboard response
        
        [keyIsDown_s1, t, keyCode_s1] = PsychHID('KbCheck', cfg.kb_id(1));    % get timing and key
        [keyIsDown_s2, t, keyCode_s2] = PsychHID('KbCheck', cfg.kb_id(2));    % get timing and key
        
        % Key Name
        key{1} = KbName(keyCode_s1);
        key{2} = KbName(keyCode_s2);
        
        % check if empty
        for i=1:2
            if isempty(key{i}),        key{i} = ''; end
        end
        
        % check correct formatting
        for i=1:2
            if ~iscell(key{i}), key{i} = {key{i}}; end
            key{i} = key{i}{1};
        end
        
        % check confirmation or escape key press
        for i=1:2
            switch key{i}
                case cfg.response.confirm,
                    key{i} = 'space';
                    if havechanged(i,1) == 1 
                        haveconfirmed(i,1) = 1;
                        confirm_t(i,1)     = GetSecs;
                        rt(i,1)            = confirm_t(i,1) - starttime;
                    else
                        havechanged(i,1) = 0;
                        %                         KbReleaseWait(cfg.kb_id(i));
                    end
                case cfg.response.escape
                    sca
                    duplicate_mouse(0);
            end
        end
        
        %until release
        if cfg.response.until_release
            for i=1:2
                KbReleaseWait(cfg.kb_id(i));
            end
        end
    end
    
    %% update termination condition
    if isempty(cj1) ...                                     % if it's a first decision
            && sum(haveconfirmed)==2 ...                    % terminate based on subjects' confirmation
            && sum(havechanged)==2  ...                     % and conditional on both having moved at least once
            && (responses(1,size(responses,2))~=0 ...       % ...
            && responses(2,size(responses,2))~=0)           % and both final responses are different from 0
        terminated=true;
    elseif (GetSecs-starttime) > delay % if it's a second decision, terminates based on elapsed time
%         endtime             = GetSecs;                  % Consider assigning NaN here. RT is not really meaningful
%         confirm_t           = deal([endtime;endtime]);  %
%         rt                  = confirm_t - starttime;    %
        terminated          = true;
    end
    
    %% record cycle time
    cycle=cat(2,cycle,toc);
end


%% ---------------------------------------------------------------
%% define interval chosen: [1 2] range
interval = 2-(int<0);

%% define rt and confirm_t for 2nd decisions
if ~isempty(cj1)
    [confirm_t rt] = deal([NaN;NaN]);
end

%% Wait until keyboard buttons are released
for i=1:2
    KbReleaseWait(cfg.kb_id(i));
end

%% hide back cursor
HideCursor(Sc.window,cfg.master_id(1));
HideCursor(Sc.window,cfg.master_id(2));

%% return stats on loop execution time
C.average       = mean(cycle);
C.min           = min(cycle);
C.max           = max(cycle);

return