%% read in audio files
% You'll notice that the three output variables are customized to obsspeech.wav.
% This allows us to keep track of which audio variables belong to which audio track.
% For the most part, these output variables are meaningless unless
% you have a firm understanding of audio properties or driver playback

%introduction sentences
clear h i j gender
for obs = 1:cfg.advisors.count.all
    %observers' speeches
    [introSpeechFile, cfg.introSpeechFreq{obs}] = audioread([cfg.path.stims stim_folder num2str(obs) osSlash 'introduction.wav']);
    % The following lines of code are necessary for PsychPortAudio's handling of the wav file.
    cfg.introSpeechData{obs} = introSpeechFile';
    cfg.introSpeechChannels{obs} = size(cfg.introSpeechData{obs},1);       % Number of rows == number of channels.
    clear introSpeechFile
end

% voices recorded through audacity at http://www.oddcast.com/home/demos/tts/tts_example.php
for obs= 1:cfg.advisors.count.all % one is going to be used for practice, the others for the experiment
    for h=1:2 % 1=normal / 2=reverse order
        for i=1:2 %1=left / 2=right
            %observers' speeches
            [speechFile, cfg.speechFreq{obs,h,i}] = audioread([cfg.path.stims stim_folder num2str(cfg.advisor(obs).voice) osSlash num2str(h) num2str(i) '2.wav']);
            % The following lines of code are necessary for PsychPortAudio's handling of the wav file.
            cfg.speechData{obs,h,i} = speechFile';
            cfg.speechChannels{obs,h,i} = size(cfg.speechData{obs,h,i},1); % Number of rows == number of channels.
            clear speechFile
        end
    end
end
% Performing a basic initialization of the sound driver
InitializePsychSound;
