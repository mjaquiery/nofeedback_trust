%-- add progression bar. Helps motivation
draw_progression_bar(Sc, cfg);

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

Screen('Flip',Sc.window);
WaitSecs(.5);
collect_response(cfg,inf)
add_fixation;
Screen('Flip',Sc.window);
WaitSecs(.5);

