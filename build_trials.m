trials = [];
trialid = 0;
index=1;
[block_trials0, block_trials1] =deal([]); %initialize as empty vectors


% practice trials
for b = 1: cfg.nblocksprac
    block_trials0 = []; %clear vector
    for t = 1:cfg.ntrialsprac
        trialid                                 = trialid+1;
        block_trials0(end+1).trialid            = trialid;
        block_trials0(end).estim_obsacc         = [];
        block_trials0(end).block                = b;
        block_trials0(end).feedback             = true; 
        block_trials0(end).cj1                  = [];
        block_trials0(end).obsacc               = NaN;
        block_trials0(end).qanswers             = [];
        if b > 1
            block_trials0(end).obstype          = 0; % practice
            block_trials0(end).pic              = cfg.observer.pic(4);     % practice picture
            block_trials0(end).voice            = cfg.observer.voice(4);   % practice voice
        else
            block_trials0(end).obstype          = NaN;
            block_trials0(end).pic              = NaN;
            block_trials0(end).voice            = NaN;
        end
    end
    block_trials0   = block_trials0(randperm(length(block_trials0)));      % randomize trials within block
    trials          = cat(2,trials,block_trials0);                         % concatenate practice block
end

% experimental trials
for b = b+1 : cfg.nblocks+cfg.nblocksprac
    block_trials1 = [];
    for o = 1:cfg.nobs
        for t = 1:((cfg.ntrials - cfg.nullt) ./ 3) % 10 presentations of observers/block
            trialid                                 = trialid+1;
            block_trials1(end+1).trialid            = trialid;
            block_trials1(end).estim_obsacc         = [];
            block_trials1(end).block                = b;
            block_trials1(end).feedback             = false;
            block_trials1(end).cj1                  = [];
            block_trials1(end).obsacc               = NaN; % conditional on subjects confidence
            block_trials1(end).obstype              = o-1;
            block_trials1(end).pic                  = cfg.observer.pic(o); 
            block_trials1(end).voice                = cfg.observer.voice(o); 
            block_trials1(end).qanswers             = [];
        end
    end
    for t = 1:cfg.nullt % 5 null trials per block
        trialid                                 = trialid+1;
        block_trials1(end+1).trialid            = trialid;
        block_trials1(end).estim_obsacc         = [];
        block_trials1(end).block                = b;
        block_trials1(end).feedback             = false;
        block_trials1(end).cj1                  = [];
        block_trials1(end).obsacc               = NaN; 
        block_trials1(end).obstype              = NaN;
        block_trials1(end).pic                  = NaN;
        block_trials1(end).voice                = NaN;
        block_trials1(end).qanswers             = [];
    end
    block_trials1    = block_trials1(randperm(length(block_trials1)));         % randomize trials within a block
    trials    = cat(2,trials,block_trials1);                                   % concatenate to main trial vector
end
clear  block_trial*;

wl0 = repmat([1 2],1,(cfg.ntrialsprac/2) * cfg.nblocksprac);

% this method for where larger computation has been implemented only for
% study3f. Check previous versions of the study!
wl1 = repmat([1 2],1,(cfg.ntrials./2) * cfg.nblocks);
%% WARNING! Massive problem detected. 
% This way of assigning wherelarger is imbalanced!
% wl1 = repmat([1 2],1,(cfg.ntrials./2 +cfg.nullt) * cfg.nblocks);
% Has this method been implemented for study3???
%
%% 
wl0 = wl0(randperm(length(wl0)));
wl1 = wl1(randperm(length(wl1)));
wl  = cat(2,wl0,wl1);
for t = 1 : length(trials)
    trials(t).wherelarger = wl(t);
    trials(t).wheredots = zeros(2,400);
end
clear wl

%-- extra information at each trial
for t = 1:length(trials)
    %-- add breaks and instruction points
    if t == 1
        trials(t).break         = false;
        trials(t).instr         = true;
        trials(t).questionnaire = false;
    elseif ...
            trials(t-1).block ~= trials(t).block 
        trials(t).break = true;
        trials(t).feedback = true;
        if trials(t).block <= cfg.nblocksprac+1
            trials(t).instr = true;
        else trials(t).instr = false;
        end
        if ismember(trials(t).block,[3:2:cfg.nblocks+cfg.nblocksprac]), % questionnaires every 2 blocks
            trials(t).questionnaire = true; 
        else trials(t).questionnaire = false; 
        end
    else
        trials(t).break         = false;
        trials(t).instr         = false;
        trials(t).questionnaire = false;
    end
end

%-- check fields: none should be empty
fields = fieldnames(trials);
for f= 1:length(fields)
    if eval(['length({trials.' fields{f} '})']) ~= length(trials)
        disp(['Empty fields in ' fields{f} '!']);
    end
end
clear f
if 0
    % check the design manually
    % after randomization
    img_dsg(trials,{'block', 'feedback', 'obstype', 'instr' 'wherelarger'})
    figure(gcf+1);
    [z index] = sort([trials.trialid]);clear z;
    img_dsg(trials(index),{'block', 'feedback', 'obstype','instr' 'wherelarger'})
end