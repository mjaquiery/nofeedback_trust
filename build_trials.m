trials = [];
trialid = 0;
[block_trials0, block_trials1] =deal([]); %initialize as empty vectors

% practice trials
for b = 1: cfg.practice.block_count
    block_trials0 = []; %clear vector
    for t = 1:cfg.practice.trial_count
        trialid                                 = trialid+1;
        block_trials0(end+1).trialid            = trialid;
        block_trials0(end).estim_obsacc         = [];
        block_trials0(end).block                = b;
        block_trials0(end).feedback             = true; 
        block_trials0(end).cj1                  = [];
        block_trials0(end).obsacc               = NaN;
        block_trials0(end).qanswers             = [];
        block_trials0(end).choice               = [];
        block_trials0(end).choiceDecision       = -1; % NaN is used for selecting 'no advisor'
        block_trials0(end).choiceTime           = NaN;
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
for b = b+1 : cfg.practice.block_count+cfg.block_count
    block_trials1 = [];
    for ts = 1 : cfg.block.trialset_count % trialset
        for o = 1:cfg.nobs
            for c = 1:cfg.trialset.choice % choice trials
                for oo = 1:cfg.nobs 
                    if oo ~= o % don't give choices with the same option each side
                        make_base_trial;
                        block_trials1(end).choice = [o oo];
                    elseif cfg.trialset.include_void_choice
                        make_base_trial;
                        block_trials1(end).choice = [NaN o];
                        block_trials1(end).choice = block_trials1(end).choice(randperm(2));
                    end
                end
            end   
            for nc = 1:cfg.trialset.nochoice % no-choice trials
                make_base_trial;
                block_trials1(end).obstype = o-1; % we subtract 1 for some reason...
                block_trials1(end).pic = cfg.observer.pic(o);
                block_trials1(end).voice = cfg.observer.voice(o);
            end
        end
        for t = 1:cfg.trialset.null % null trials per trialset
            make_base_trial;
        end
    end
    block_trials1    = block_trials1(randperm(length(block_trials1)));         % randomize trials within a block
    trials    = cat(2,trials,block_trials1);                                   % concatenate to main trial vector
end

% this fills an appropriately sized list with 1, 2, 1, 2, 1, 2, ...
% It gets shuffled with randperm later.
practice_trial_count = length(block_trials0)*cfg.practice.block_count;
wl0 = repmat([1 2],1,ceil(practice_trial_count/2));

% this method for where larger computation has been implemented only for
% study3f. Check previous versions of the study!
wl1 = repmat([1 2],1,length(trials)-practice_trial_count);

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
    elseif trials(t-1).block ~= trials(t).block % first trial in a new block
        trials(t).break = true;
        trials(t).feedback = true;
        if trials(t).block <= cfg.practice.block_count+1 % first trials in practice blocks
            trials(t).instr = true;
        else trials(t).instr = false;
        end
        if ismember(trials(t).block,[3:cfg.block.questionnaire_frequency:cfg.block_count+cfg.practice.block_count]) % questionnaires every few blocks
            if cfg.debug==0 % don't put questionnaires in the debgging routine
                trials(t).questionnaire = true; 
            end
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

% clean the workspace up a bit
% these variables are retrievable later since they are calculated from
% specified variables which ARE recorded
clear  f block_trial* b c fields nc o oo practice_trial_count t trialid ts wl0 wl1;
cfg.ntrialsall = length(trials); % record this for the progress bar

%     % check the design manually
%     % after randomization
%     img_dsg(trials,{'block', 'feedback', 'obstype', 'instr' 'wherelarger'})
%     figure(gcf+1);
%     [z index] = sort([trials.trialid]);clear z;
%     img_dsg(trials(index),{'block', 'feedback', 'obstype','instr' 'wherelarger'})