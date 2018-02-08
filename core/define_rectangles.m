% stimulus containers definition
cfg.rect.innerrect1 = [Sc.rect(3)/2-160,Sc.rect(3)/2-46;Sc.rect(4)/2-57,Sc.rect(4)/2+57];
cfg.rect.innerrect1outer = [Sc.rect(3)/2-167,Sc.rect(3)/2-39;Sc.rect(4)/2-64,Sc.rect(4)/2+64];
cfg.rect.innerrect1out = [Sc.rect(3)/2-167 Sc.rect(3)/2-39 ...
    Sc.rect(3)/2-167 Sc.rect(3)/2-39 ...
    Sc.rect(3)/2-167 Sc.rect(3)/2-167 ...
    Sc.rect(3)/2-39 Sc.rect(3)/2-39; ...
    Sc.rect(4)/2-64 Sc.rect(4)/2-64 ...
    Sc.rect(4)/2+64 Sc.rect(4)/2+64 ...
    Sc.rect(4)/2-64 Sc.rect(4)/2+64 ...
    Sc.rect(4)/2-64 Sc.rect(4)/2+64];
cfg.rect.innerrect2 = [Sc.rect(3)/2+46,Sc.rect(3)/2+160;Sc.rect(4)/2-57,Sc.rect(4)/2+57];
cfg.rect.innerrect2outer = [Sc.rect(3)/2+39,Sc.rect(3)/2+167;Sc.rect(4)/2-64,Sc.rect(4)/2+64];
cfg.rect.innerrect2out = [Sc.rect(3)/2+39 Sc.rect(3)/2+167 ...
    Sc.rect(3)/2+39 Sc.rect(3)/2+167 ...
    Sc.rect(3)/2+39 Sc.rect(3)/2+39 ...
    Sc.rect(3)/2+167 Sc.rect(3)/2+167; ...
    Sc.rect(4)/2-64 Sc.rect(4)/2-64 ...
    Sc.rect(4)/2+64 Sc.rect(4)/2+64 ...
    Sc.rect(4)/2-64 Sc.rect(4)/2+64 ...
    Sc.rect(4)/2-64 Sc.rect(4)/2+64];
cfg.rect.center1 = [round((cfg.rect.innerrect1(1,2)-cfg.rect.innerrect1(1,1))*0.5+cfg.rect.innerrect1(1,1))...
    round((cfg.rect.innerrect1(2,2)-cfg.rect.innerrect1(2,1))*0.5+cfg.rect.innerrect1(2,1))];
cfg.rect.center2 = [round((cfg.rect.innerrect2(1,2)-cfg.rect.innerrect2(1,1))*0.5+cfg.rect.innerrect2(1,1))...
    round((cfg.rect.innerrect2(2,2)-cfg.rect.innerrect2(2,1))*0.5+cfg.rect.innerrect2(2,1))];
