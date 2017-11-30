function debugTrialInfo()
%% Writes trial info to the top-left of the screen for debugging purposes
% - by Matt Jaquiery
%
% usage: debugTrialInfo();
%

global cfg; % configuration object
global Sc; % Screen object

oldTextSize = Screen('TextSize',Sc.window,cfg.instr.textSize.small);
t = cfg.currentTrial;

str = ['Trial Number: ' int2str(cfg.currentTrialNumber) '/' int2str(cfg.ntrialsall) '\n' ...
    'Trial Id:' int2str(t.id) '\n' ...
    'Block: ' int2str(t.block) '\n' ...
    ];
if ~isempty(t.choice)
    str = [str ...
        'ChoiceL: ' int2str(t.choice(1)) '\n' ...
        'ChoiceR: ' int2str(t.choice(2)) '\n' ...
        ];
end
str = [str ...
    'AdvisorId: ' int2str(t.advisorId) '\n' ...
    ];
if ~isnan(t.advisorId)
    str = [str ...
        'Advisor: ' cfg.advisor(t.advisorId).name '\n' ...
        'Advice Type: ' int2str(cfg.advisor(t.advisorId).adviceType) '\n' ...
        'Good Advice?: ' int2str(t.obsacc) '\n' ...
        ];
end
str = [str ...
    '\n' ...
    'Answer1: ' int2str(t.wherelarger) '\n' ...
    'Correct?: ' int2str(t.cor) '\n' ...
    ];

DrawFormattedText(Sc.window, str, 0, cfg.instr.textSize.small);
Screen('TextSize',Sc.window,oldTextSize);