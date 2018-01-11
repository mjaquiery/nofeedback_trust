function [agree, step] = agreementf(rawx,type,distribution,funct)
%% Agreement function for advisors
% - by Niccolo Pescetelli
%
% usage : [agree step] = agreementf(raw-x,obstype,distribution,function type)
% Inputs:
% raw-x : current confidence judgment
% type: (adviceType) 1-agree in confidence, 2-agree in uncertainty, 0- baseline 70%
% distribution: vector containing (correct) confidence judgments so far 
% function: one among 'semilinear' 'sigmoid' 'stepwise'
%
% Outputs:
% agree: (logical) whether the advisor agrees with the participant
% step: whether the participant's answer is given which low (-1), medium
%   (0), or high (1) confidence given their recent use of the confidence scale
%
% This function calculates whether or not the advisor agrees with the
% participant. The confidence value of the participant's answer is assessed
% in terms of the confidence values provided by that participant in the
% last two blocks. Different adviceType profiles have different agreement
% probabilities depending upon how confident the participant is (relative
% to their idiosyncratic usage of the scale). 
% 
% The baseline advice type (0) agrees with the participant 70% of the time
% regardless of confidence. Other advice types either agree more often when
% the participant is highly confident (top 30% of confidence ratings) or
% very unconfidenct (bottom 30%). All advisors agree at the baseline rate
% of 70% when the participant is moderately confident (middle 40%). 
%
% This function is only invoked when the participant is CORRECT. All
% advisors agree 30% of the time with a participant when they are mistaken,
% and this value is irrespective of confidence.
%
x =abs(rawx);
b = quantile(distribution,[.3 .7]); % boundaries (30% and 70% of the mass)

switch funct
    case 'sigmoid'
        % define sigmoid function parameters
        sat = .5; % lower saturation at 50%
        phase = 1;
        slope1 = 2; % obstype 1
        slope2 = -2; % obstype 2
        
        step   = NaN;
        switch type
            case 1 % agreement under confidence
                pagree= sigmoid(x,phase,slope1)*.5 + sat; % agreeing in confidence
            case 2 % agreement under uncertainty
                pagree = sigmoid(x,phase,slope2)*.5 + sat; % agreeing in uncertainty
            case 0 % baseline type
                pagree = .7;
        end
        
    case 'semilinear'
        switch type
            case 1 % agreement under confidence
                if x > b(1) && x <= b(2) % around 60% of the mass is between -.52 and +.52 sigmas
                    pagree = .7;
                    step   = 0;
                elseif x <= b(1)
                    minv = min(distribution); % minimum value of the distribution corresponds to 60%
                    beta = -.1 / (b(2) + minv);
                    intercept = .7 + beta * b(2);
                    pagree = intercept + beta * x;
                    step   = -1;
                elseif x > b(2)
                    maxv = max(distribution);
                    beta = .1 / (b(1) + maxv);
                    intercept = .7 - beta * b(2);
                    pagree = intercept + beta * x;
                    step   = 1;
                end
            case 2 % agreement under uncertainty
                if x > b(1) && x <= b(2)
                    pagree = .7;
                    step   = 0;
                elseif x <= b(1)
                    minv = min(distribution); % minimum value of the distribution corresponds to 60%
                    beta = .1 / (b(2) + minv);
                    intercept = .8 - beta * (minv);
                    pagree = intercept + beta * x;
                    step   = -1;
                elseif x > b(2)
                    maxv = max(distribution);
                    beta = -.1 / (b(1) + maxv);
                    intercept = .7 - beta * b(2);
                    pagree = intercept + beta * x;
                    step   = 1;
                end
            case 0 % baseline type
                pagree = .7;
                step   = 0;
        end
    case 'stepwise'
        switch type
            case 1 % agreement under confidence 
                if x > b(1) && x <= b(2)
                    pagree = .7;
                    step   = 0;
                elseif x <= b(1)
                    pagree = .6;
                    step   = -1;
                elseif x > b(2)
                    pagree = .8;
                    step   = 1;
                end
            case 2 % agreement under uncertainty
                if x > b(1) && x <= b(2)
                    pagree = .7;
                    step   = 0;
                elseif x <= b(1)
                    pagree = .8;
                    step   = -1;
                elseif x > b(2)
                    pagree = .6;
                    step   = 1;
                end
            case 0 % baseline type
                if x > b(1) && x <= b(2)
                    pagree = .7;
                    step   = 0;
                elseif x <= b(1)
                    pagree = .7;
                    step   = -1;
                elseif x > b(2)
                    pagree = .7;
                    step   = 1;
                end
        end        
end

agree = rand<pagree;

