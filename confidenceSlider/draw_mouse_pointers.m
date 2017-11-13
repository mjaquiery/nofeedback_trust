function [] = draw_mouse_pointers(Sc,cfg,X,Y)
% Usage:
% [] = draw_mouse_pointers(Sc,cfg,X,Y)
%
% This function draws on buffer n mice pointers.
% It takes as input the pointer of an image, usually a mouse png
% and X and Y positions. X and Y are nx1 vectors where n is the number of 
% pointers to be drawn. 

dims        = cfg.pointer_dims;
for i=1:size(X,1)
    Screen('DrawTexture', Sc.window, cfg.pointer,[], ...
        [X(i) Y(i) (X(i)+dims(1)) (Y(i)+dims(2))]);
end

end