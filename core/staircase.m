function out = staircase(correct,value)
% 2Dvalue = staircase(correct,varargin)
% correct(1): last trial accuracy
% correct(2): second last trial accuracy
% - optional
%   value:  2D vector [0 1]; last two values
%   ratio:  [.5488] => converge towards 80.35% accuracy
% traditional 2D 1 up 2 down staircase based on 
% Forced-choice staircases with ям?xed step sizes: asymptotic and
% small-sample properties, M. Garcia Perez, Vision Research 1998

%-- what's the point of this varargin thing?
% if nargin == 1, varargin = {}; end
% for ii = 1:2:length(varargin)
%     eval([varargin{ii} '=varargin{ii+1};'])
% end

if ~exist('value','var'),       value = 0;  end
if ~exist('ratio','var'),       ratio = 1; end %ratio = .5488 should converge toward 0.8035 accuracy 
if ~exist('stepsize','var'),    stepsize= 1;    end

switch 'method 2'
    case 'method 1' % method 1 (JR)
        if nansum(correct) == 2 % down if 2 successful response
            change = -(1/ratio);
        elseif correct(1) == 0; % up if incorrect response in last trial
            change = 1;
        else
            change = 0;
        end
        
        if length(value) == 2
            out = value + [1;-1] * change * stepsize;
        else
            out = value + change * stepsize;
        end
        
        % bounds output
        if out < 1,
            out = 1;
        elseif out > 50,
            out= 50;
        end
    case 'method 2' % Bahador
        errorCoeff    = 1.0;
        minimumOut  = 1;
        %--progressively tuning function
        if 0 % use only for continuous variables 
            initStep      = 0.05;
            stepsize     = initStep;
            stepSizeCoeff = 10;
            if n==1
                stepsize = initStep;
            else
                changeDirN = sum(abs(diff(accHist)));
                stepsize  = initStep .* (1./exp(changeDirN .^ 1/stepSizeCoeff));
            end
            if stepsize <= minimumOut
                stepsize = minimumOut;
            end
        end
        %-- control value change
        if correct(end) == 1
            if correct(end-1) == 1 && value(end) == value(end-1)
                out = value(end)- (stepsize);
                if out <= minimumOut %control for delta remaining above 0
                    out = minimumOut;
                end;
            else
                out = value(end);
            end
        elseif correct(end) == 0
            out = value(end)+ (errorCoeff .* stepsize);
        else out = value(end);
        end
end

return