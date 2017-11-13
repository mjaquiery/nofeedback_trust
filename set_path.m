%% set path 
switch 'MattHome'
    case 'MattHome'
        %cd('G:\Documents\University\Oxford\Work\matlabResults');
        my_path = 'G:\Documents\University\Programming\nofeedback_trust_matt'; % working directory
        if ~feedbackEnabled
            results_path = 'G:\Documents\University\Oxford\Work\matlabResults\';
        else
            results_path = 'G:\Documents\University\Oxford\Work\matlabResults\Feedback\';
        end
        stims_path    = 'G:\Documents\University\Programming\nofeedback_trust_matt\stims';
        addpath(my_path);
        addpath([my_path '\confidenceSlider']);
        im_path = 'G:\Documents\University\Programming\nofeedback_trust_matt\internalExternal';
end