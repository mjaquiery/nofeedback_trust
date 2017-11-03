% prompt subject info
prompt = {'Subject ID:','Gender(m/f/o):','Age:',...
    'Experiment restarted? Yes=1 or No=0: '};
answer = inputdlg(prompt);

subject.id          = answer{1};
subject.gender      = answer{2};
subject.age         = str2num(answer{3});
subject.restarted   = str2num(answer{4});
subject.date        = date;
subject.start_time  = clock;
subject.name        = num2str(subject.id);  
subject.screen      = 0;

% testing mode
if isempty(subject.id) || isempty(subject.restarted)
    warning('TESTING MODE');
    subject.male            = NaN;
    subject.age             = NaN;
    subject.right_handed    = NaN;
    subject.screen          = 0; % small size screen:1
    subject.name            = 'test';
    subject.id              = 999;
    subject.restarted       = 0;
end
if isempty(subject.name)
    subject.name = 'test';
end


%%-- saving directory
if 0
    %non feedback
    subject.dir = subject.name;
else
    %feedback
    subject.dir = ['f' subject.name];
end
% create directory if does not already exist
if ~exist([results_path subject.dir '/behaviour'], 'dir'), 
    mkdir([results_path subject.dir '/behaviour']);
end

%-- Unique filename depending on computer clock (avoids overwriting)
subject.fileName = [num2str(round(datenum(subject.start_time)*100000)) '_' num2str(subject.id)];
