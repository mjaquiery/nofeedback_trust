%-- add progression bar. Helps motivation
Start='start';
End='end';
startbounds = Screen('TextBounds', Sc.window, Start);
endbounds = Screen('TextBounds', Sc.window, End);
viscursorwidth=1;
viscursorheight=20;
subblock = t; 
prbarrect=CenterRectOnPoint([0 0 (length(trials) * viscursorwidth) (viscursorheight)],Sc.center(1),Sc.center(2)-250);
donerect=[prbarrect(1) prbarrect(2) (prbarrect(1)+ t * viscursorwidth) prbarrect(4)];

% continue instructions
Screen('TextSize', Sc.window, 16);
txtline = ['Please press any key to continue with block ' int2str(trials(t).block) ' out of ' int2str(cfg.nblocks+cfg.nblocksprac) '.'];
DrawFormattedText(Sc.window, txtline, 'center',  Sc.center(2)+0.2*Sc.size(2), 255, [], [], [], 1.5);
% feedback
if trials(t-1).block ==1
    txtline = ['Your performance in the last block has been ' num2str(round(nanmean([trials([trials.block]==trials(t).block-1).cor]).*100)) '%'];
else
    txtline = ['Your performance in the last block has been ' num2str(round(nanmean([trials([trials.block]==trials(t).block-1).cor2]).*100)) '%'];
end
DrawFormattedText(Sc.window, txtline, 'center',  Sc.center(2)-0.2*Sc.size(2), 255, [], [], [], 1.5);

%add progression bar
Screen('FrameRect',Sc.window,[.9 .9 .9],prbarrect);
Screen('FillRect', Sc.window, [.9 .9 .9],donerect);
DrawFormattedText(Sc.window,Start, prbarrect(1)-startbounds(3)-10,prbarrect(2)-viscursorheight);
DrawFormattedText(Sc.window,End, prbarrect(3)+10,prbarrect(2)-viscursorheight);
Screen('Flip',Sc.window);
WaitSecs(.5);
collect_response(cfg,inf)
add_fixation;
Screen('Flip',Sc.window);
WaitSecs(.5);

