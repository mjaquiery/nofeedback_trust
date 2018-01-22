trials = [];
trialid = 0;
[block_trials0, block_trials1] =deal([]); %initialize as empty vectors

% practice trials
for b = 1: cfg.practice.block_count
    block_trials0 = []; %clear vector
    for t = 1:cfg.practice.trial_count
        trialid                                 = trialid+1;
        block_trials0 = [block_trials0 getNewTrial(trialid, b)];
        block_trials0(end).feedback = true;
        block_trials0(end).practice = true;
        if b > 1
            if cfg.practice.allowChoice
                block_trials0(end).choice = [cfg.advisors.count.real+1:cfg.advisors.count.all];
            else
                block_trials0(end).advisorId = getRandomAdvisor(2);
                block_trials0(end).choice = [0 block_trials0(end).advisorId];
            end
            block_trials0(end).choice = block_trials0(end).choice(randperm(2));
        end
    end
    block_trials0   = block_trials0(randperm(length(block_trials0)));      % randomize trials within block
    trials          = cat(2,trials,block_trials0);                         % concatenate practice block
end

% experimental trials
for b = b+1 : cfg.practice.block_count+cfg.block_count
    block_trials1 = [];
    for ts = 1 : cfg.block.trialset_count % trialset
        for o = 1:cfg.advisors.count.real
            for c = 1:cfg.trialset.choice % choice trials
                for oo = 1:cfg.advisors.count.real
                    if oo ~= o % don't give choices with the same option each side
                        trialid = trialid + 1;
                        block_trials1 = [block_trials1 getNewTrial(trialid, b)];
                        block_trials1(end).choice = [o oo];
                    elseif cfg.trialset.include_void_choice
                        trialid = trialid + 1;
                        block_trials1 = [block_trials1 getNewTrial(trialid, b)];
                        block_trials1(end).choice = [NaN o];
                        % shuffle the choice since we don't counterbalance this
                        block_trials1(end).choice = block_trials1(end).choice(randperm(2)); 
                    end
                end
            end   
            for nc = 1:cfg.trialset.nochoice % no-choice trials
                trialid = trialid + 1;
                block_trials1 = [block_trials1 getNewTrial(trialid, b)];
                block_trials1(end).advisorId = o;
                block_trials1(end).choice = [0 o]; % force a choice between non-selectable option and this advisor
                % shuffle the choice since we don't counterbalance this
                block_trials1(end).choice = block_trials1(end).choice(randperm(2)); 
            end
        end
        for t = 1:cfg.trialset.null % null trials per trialset
            trialid = trialid + 1;
            block_trials1(end+1) = getNewTrial(trialid, b);
            block_trials1(end).choice = [NaN 0]; % forced choice of null
            block_trials1(end).choice = block_trials1(end).choice(randperm(2)); % randomise the force selection of no advisor top/bottom
        end
    end
    block_trials1    = block_trials1(randperm(length(block_trials1)));         % randomize trials within a block
    trials    = cat(2,trials,block_trials1);                                   % concatenate to main trial vector
end

%% Calculate answers
% this fills an appropriately sized list with 1, 2, 1, 2, 1, 2, ...
% It gets shuffled with randperm later.
practice_trial_count = length(block_trials0)*cfg.practice.block_count;
wl0 = repmat([1 2],1,ceil(practice_trial_count/2));

% this method for where larger computation has been implemented only for
% study3f. Check previous versions of the study!
wl1 = repmat([1 2],1,length(trials)-practice_trial_count);

wl0 = wl0(randperm(length(wl0)));
wl1 = wl1(randperm(length(wl1)));
wl  = cat(2,wl0,wl1);
for t = 1 : length(trials)
    trials(t).wherelarger = wl(t);
    trials(t).wheredots = zeros(2,400);
end
clear wl

%% Add extra information at each trial
for t = 1:length(trials)
    %-- add breaks and instruction points
    if t == 1
        trials(t).instr         = true;
    elseif trials(t-1).block ~= trials(t).block % first trial in a new block
        trials(t).break = true;
        trials(t).feedback = true;
        if trials(t).block <= cfg.practice.block_count+1 % first trials in practice blocks
            trials(t).instr = true;
        end
        if ismember(trials(t).block,[3:cfg.block.questionnaire_frequency:cfg.block_count+cfg.practice.block_count]) % questionnaires every few blocks
            if cfg.debug==0 % don't put questionnaires in the debgging routine
                trials(t).questionnaire = true; 
            end
        end
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
