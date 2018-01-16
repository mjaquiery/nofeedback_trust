function playFeedback(type)
% Plays a feedback sound using PsychPortAudio
% - Matt Jaquiery, Jan 2018
% -- adapted from Psychtoolbox' BasicSoundOutputDemo
%
% usage: playFeedback([type])
%
% inputs: 
% type - type of feedback. 1(default)=negative; 2=positive

global cfg; % configuration object
global audio; % stores the beep audio for future use

% Running on PTB-3? Abort otherwise.
AssertOpenGL;

if nargin < 1, type = 1; end

% load the file the first time
if length(audio) < type || isempty(audio(type).wav)
    % Load the file:
    switch type
        case 2
            pth = cfg.audio.feedback.positive;
        otherwise
            pth = cfg.audio.feedback.negative;
    end
    [y, freq] = psychwavread(pth);
    audio(type).wav = y';
    audio(type).freq = freq;
    fprintf('Loaded file %s', pth);
end
% load the wav data
wavedata = audio(type).wav;
freq = audio(type).freq;
nrchannels = size(wavedata,1); % Number of rows == number of channels.

% Make sure we have always 2 channels stereo output.
% Why? Because some low-end and embedded soundcards
% only support 2 channels, not 1 channel, and we want
% to be robust.
if nrchannels < 2
    wavedata = [wavedata ; wavedata];
    nrchannels = 2;
end

% Perform basic initialization of the sound driver:
InitializePsychSound;

% Open the default audio device [], with default mode [] (==Only playback),
% and a required latencyclass of zero 0 == no low-latency mode, as well as
% a frequency of freq and nrchannels sound channels.
% This returns a handle to the audio device:
try
    % Try with the 'freq'uency we wanted:
    pahandle = PsychPortAudio('Open', [], [], 0, freq, nrchannels);
catch
    % Failed. Retry with default frequency as suggested by device:
    fprintf('\nCould not open device at wanted playback frequency of %i Hz. Will retry with device default frequency.\n', freq);
    fprintf('Sound may sound a bit out of tune, ...\n\n');

    psychlasterror('reset');
    pahandle = PsychPortAudio('Open', [], [], 0, [], nrchannels);
end

% Fill the audio playback buffer with the audio data 'wavedata':
PsychPortAudio('FillBuffer', pahandle, wavedata);

% Start audio playback start it immediately (0) 
% and wait for the playback to start
PsychPortAudio('Start', pahandle, 1, 0, 1);

% Stop playback:
PsychPortAudio('Stop', pahandle);

% Close the audio device:
PsychPortAudio('Close', pahandle);
