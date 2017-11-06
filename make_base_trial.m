% should turn this into a function at some point
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
block_trials1(end).choice               = [];
block_trials1(end).choiceDecision       = -1; % NaN is used for selecting 'no advisor'
block_trials1(end).choiceTime           = NaN;
block_trials1(end).qanswers             = [];