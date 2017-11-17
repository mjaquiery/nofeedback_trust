function [resp t code]  = collect_response(delay)
%% Listens for a keypress
% - by Niccolo Pescetelli
%
% usage: [resp t code] = collect_response(delay);
%
% Inputs:
%   delay - max seconds to wait for keypress
%
% Outputs:
% resp - 1 if keyIsDown, 0 if it is not, 2 if experimenter pressed ESCAPE
% t - time of keypress
% code - keycode for pressed key
%

global cfg; % configuration object

if ~isfield(cfg,'port'),            cfg.port = 'keyboard';end
if ~isfield(cfg,'experimenter'),    cfg.experimenter = 'space';end
if ~isfield(cfg,'escape'),          cfg.escape = 'ESCAPE';end
if ~isfield(cfg,'pause'),           cfg.pause = 'P';end
if ~isfield(cfg,'until_release'),   cfg.until_release = true;end

delay = GetSecs + delay;
 
%% get response
resp                        = 0; % response key
t                           = 0; % time of response
code                        = '';
while (sum(resp) == 0 || isempty(code)) && t <= delay
    t = GetSecs;
    %experimenter
    [exp_resp exp_t exp_code]       = KbCheck;          % get cfg.timing and resp1 from keyboard
    % translate
    exp_code = KbName(exp_code);
    % only take first response if multiple responses
    if ~iscell(exp_code), exp_code = {exp_code}; end
    exp_code = exp_code{1};
    if ~isempty(exp_code)
        switch exp_code
            case cfg.experimenter
                resp = exp_resp;
                code = exp_code;
                t = exp_t;
                break;
            case cfg.pause
                pause(1);
                w = 1;
                while w == 1
                    if KbCheck > 0,break;end
                end
            case cfg.escape, sca; resp = 2; break;
        end
    end
    
    [resp z code]       = KbCheck;          % get cfg.timing and resp1 from keyboard
    % translate
    code = KbName(code);
    % only take first response if multiple responses
    if ~iscell(code), code = {code}; end
    code = code{1};
end

%-- until release
if cfg.until_release
    resp_release = resp;
    while sum(resp_release) ~= 0
        [resp_release x name] = KbCheck;          % get cfg.timing and resp1 from keyboard
        if sum(resp_release) == 1
            if strcmp('',KbName(name))
                resp_release = 0;
            end
        end
    end
end

if sum(resp) == 0
    resp = [];
    t = [];
    code = [];
end
