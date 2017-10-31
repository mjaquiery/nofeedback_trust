function [agree, step] = agreementf(rawx,type,distribution,funct)
% use : [agree step] = agreementf(raw-x,obstype,distribution,function type)
%
% agree: logical 
% step: -1-left tail, 0-central mass, 1-right tail of the distribution
% raw-x : current confidence judgment
% obstype: 1-agree in confidence, 2-agree in uncertainty, 0- baseline 70%
% distribution: vector containing (correct) confidence judgments so far 
% function: one among 'semilinear' 'sigmoid' 'stepwise'
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

