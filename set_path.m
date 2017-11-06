%% set path 
switch 'MattHome'
%     case 'C141'
%         cd('C:/Users/acclab/Desktop/Niccolo/study3');
%         addpath(genpath('C:/Users/acclab/Desktop/Niccolo/myfunctions'));
%         my_path = 'C:/Users/acclab/Desktop/Niccolo/study3';
%         results_path = [my_path '/results/'];
%         stims_path    = 'C:/Users/acclab/Desktop/Niccolo/stims';
%         OS = 'Windows';
%     case 'niccolo'
%         cd('C:/Users/Niccolo/Dropbox/Oxford/study3');
%         addpath(genpath('C:/Users/Niccolo/Dropbox/Oxford/myfunctions'));
%         my_path = 'C:/Users/Niccolo/Dropbox/Oxford/study3';
%         results_path = [my_path '/results/'];
%         stims_path    = 'C:/Users/Niccolo/Dropbox/Oxford/asking_for_advice_project/stims';
%         OS = 'Windows';
%     case 'niccolo_ubuntu'
%         cd('/home/niccolo/Dropbox/Oxford/study3');
%         addpath(genpath('/home/niccolo/Dropbox/Oxford/myfunctions'));
% %         addpath(genpath('/home/niccolo/Dropbox/DOCUP/toolboxes/JR_toolbox'));
%         my_path = '/home/niccolo/Dropbox/Oxford/study3'; % working directory
%         results_path = [my_path '/results/'];
%         stims_path    = '/home/niccolo/Dropbox/Oxford/stims';
%         addpath(genpath(my_path));
%         OS = 'Linux';
%     case 'D145_ubuntu'
%         cd('/home/niccolo/Dropbox/Oxford/study3');
%         addpath(genpath('/home/niccolo/Dropbox/Oxford/myfunctions'));
%         addpath(genpath('/home/niccolo/Dropbox/Oxford/across_experiments_scripts'));
% %         addpath(genpath('/home/niccolo/Dropbox/DOCUP/toolboxes/JR_toolbox'));
%         my_path = '/home/niccolo/Dropbox/Oxford/study3'; % working directory
%         if ~feedback
%             results_path = ['/media/niccolo/Yupi/Oxford/study3/results/'];%'/media/niccolo/Yupi/study3';
%             im_path = '/home/niccolo/vboxshared/study3/';
%         else
%             results_path = ['/media/niccolo/Yupi/Oxford/study3/results_f/'];%'/media/niccolo/Yupi/study3_f';
%             im_path = '/home/niccolo/vboxshared/study3f/';
%         end
%         stims_path    = '/home/niccolo/Dropbox/Oxford/stims';
%         addpath(genpath(my_path));
%         OS = 'Linux';
%     case 'D145'
%         cd('C:/Users/npescetelli/Dropbox/Oxford/study3');
%         addpath(genpath('C:/Users/npescetelli/Dropbox/Oxford/myfunctions'));
%         addpath(genpath('C:/Users/npescetelli/Dropbox/DOCUP/toolboxes/JR_toolbox'));
%         my_path = 'C:/Users/npescetelli/Dropbox/Oxford/study3';
%         addpath(genpath(my_path));
%         results_path = ['F:/Oxford/study3/results/'];
%         OS = 'Windows';
%     case 'E118'
%         cd('C:\Documents and Settings\dschwartzman\Desktop\study3');
%         my_path = 'C:\Documents and Settings\dschwartzman\Desktop\study3';
%         OS = 'Windows';
%     case 'dropbox'
%         cd('/home/niccolo/Dropbox/Oxford/study3');
%         addpath(genpath('/home/niccolo/Dropbox/Oxford/myfunctions'));
%         my_path = '/home/niccolo/Dropbox/Oxford/study3'; % working directory
%         results_path = [pwd '/results/'];
%         if ~feedback
%             results_path = [pwd '/results/'];
%         else
%             results_path = [pwd '/results_f/'];
%         end
%         stims_path    = '/home/niccolo/Dropbox/Oxford/stims';
%         addpath(genpath(my_path));
%         im_path = '/home/niccolo/VBox_shared/internalExternal/';
    case 'MattHome'
        %cd('G:\Documents\University\Oxford\Work\matlabResults');
        my_path = 'G:\Documents\University\Programming\nofeedback_trust_matt'; % working directory
        if ~feedbackEnabled
            results_path = 'G:\Documents\University\Oxford\Work\matlabResults\';
        else
            results_path = 'G:\Documents\University\Oxford\Work\matlabResults\Feedback\';
        end
        stims_path    = 'G:\Documents\University\Programming\nofeedback_trust_matt\stims';
        addpath(genpath(my_path));
        addpath(genpath([my_path '\confidence_slider']));
        im_path = 'G:\Documents\University\Programming\nofeedback_trust_matt\internalExternal';
end