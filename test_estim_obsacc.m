clear all
close all
clc


set_path

cfg=[];
cfg.stims_path = stims_path;
cfg.observer.pic = randperm(4);
cfg.nobs =3;
cfg.positiony = .80;
cfg.instr.estimated_obsacc  = {'Your baseline accuracy (before any advice) was 71%' 'What do you think this person''s accuracy was?' 'In the next screen you will be prompted to enter a value' 'Press any button when you are ready' 'Enter a number between 0 and 100: '};

Sc = start_psychtb();

ListenChar(2)

trials(1).estim_obsacc = estimated_obsacc(Sc,cfg);

Screen('CloseAll');
ListenChar(0);


