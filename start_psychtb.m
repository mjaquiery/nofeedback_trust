function Sc = start_psychtb(targetScreen, cfg, experiment)
if nargin < 1
    position = 0;
else 
    position = targetScreen;
end
if nargin < 2
    experiment = 'behavioral';
end
%% ---------------------- start psych toolbox ----------------------------
% run '/home/experiment/Toolbox/PsychToolbox/Psychtoolbox/SetupPsychtoolbox.m';
try    Screen('CloseAll'); end;                                             % reset psych toolbox

Sc.ppd      = 30;                                                           % pixels per degree of visual angle (assuming ~60 cm between screen and participants' eyes)

%-- editable parameters
%Sc.size = [1600 900];                                                      % size of the screen
%Sc.size = [1920 1200];                                                       % size of the screen
if ~isempty(position)
    Sc.nb = position; % left screen used
else
    switch experiment
        case 'meg', Sc.nb = 1; % left screen used
        otherwise, Sc.nb = max(Screen('Screens')); % maximum available screen
    end
end
Sc.bkgCol = floor([0.5 0.5 0.5] .* 255);                                           % background color

Sc.frameDuration = 1;                                                       % update screen every x refresh


% setup hardware
AssertOpenGL;
res = Screen('Resolution',Sc.nb);
if isfield(Sc,'size')
    res.width = Sc.size(1);
    res.height = Sc.size(2);
else
    Sc.size = [res.width res.height];
end

[Sc.window, Sc.rect] = Screen('OpenWindow', Sc.nb, Sc.bkgCol);  % start psychtoobox window
Sc.center = Sc.rect(3:4)/2;
[Sc.x,Sc.y] = Screen('WindowSize',Sc.window);
Sc.fps = Screen('FrameRate',Sc.window);
%Sc.nbfi = Screen('GetFlipInterval',Sc.window,100,50e-6,10);
Priority(MaxPriority(Sc.window));
% set background to 30 cd/m2
Screen('FillRect', Sc.window, Sc.bkgCol);                                   % fill background color
Screen('Flip',Sc.window);
Screen('ColorRange',Sc.window,1);
Screen('TextFont',Sc.window,'Calibri');
Screen('TextSize',Sc.window,round(1*Sc.ppd));
% 
% if small
%     Sc.center = round(Sc.position(3:4)./2); 
% Sc.center(1) = Sc.center(1) - 1;
% else
%     Sc.center = [0 0];
% end

KbName('UnifyKeyNames');