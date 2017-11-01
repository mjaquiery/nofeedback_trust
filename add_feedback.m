function [] = add_feedback(acc,Sc)
% Screen(Sc.window,'FrameOval', [255 255 255]/2, [Sc.center-(10) Sc.center+(10)],30,2);
if isnan(acc)
    Screen(Sc.window,'FrameOval', [1 1 1], [Sc.center-(5) Sc.center+(5)],30,2);
elseif acc == 0
    Screen(Sc.window,'FrameOval', [1 0 0], [Sc.center-(5) Sc.center+(5)],30,2);
elseif acc == 1
    Screen(Sc.window,'FrameOval', [0 1 0], [Sc.center-(5) Sc.center+(5)],30,2);
end
return