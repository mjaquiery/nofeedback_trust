%-- add progression bar. Helps motivation
draw_static([1 1 0 0 0]);

% continue instructions
Screen('TextSize', Sc.window, cfg.instr.textSize.medium);
txtline = ['Please press any key to continue with block ' int2str(trials(t).block) ' out of ' int2str(cfg.block_count+cfg.practice.block_count) '.'];
DrawFormattedText(Sc.window, txtline, 'center',  Sc.center(2)+0.2*Sc.rect(4));
% feedback
if trials(t-1).block ==1
    txtline = ['Your performance in the last block was ' num2str(round(nanmean([trials([trials.block]==trials(t).block-1).cor]).*100)) '%'];
else
    txtline = ['Your performance in the last block was ' num2str(round(nanmean([trials([trials.block]==trials(t).block-1).cor2]).*100)) '%'];
end
DrawFormattedText(Sc.window, txtline, 'center',  Sc.center(2)-0.2*Sc.rect(4));

Screen('Flip',Sc.window);
WaitSecs(.5);
collect_response(inf)
Screen('Flip',Sc.window);
WaitSecs(.5);

