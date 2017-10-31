if isnan(trials(t).obstype)
    % load silouette
    sil_data = imread([stims_path '/silouette.jpg']);
    % make texture image out of image matrix 'silouette data'
    sil_tex = Screen('MakeTexture', Sc.window, sil_data);
else
    % load observer picture
    obs_data = imread([stims_path '/observer' num2str(trials(t).pic) '.jpg']);
    % make texture image out of image matrix 'observer data'
    obs_tex  = Screen('MakeTexture', Sc.window, obs_data);
end




