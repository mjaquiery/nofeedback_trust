if ~exist('SECSscore') || ~exist('cfg')
    [filename, pathname] = uigetfile('*.mat', 'Pick last saved file ');
    load([pathname filename]);
end

qs = fieldnames(SECSscore);
scores = struct();
scores(length(qs)).name = '';
for i = 1:length(qs)
    scores(i).name = qs{i};
    scores(i).participant = SECSscore.(qs{i}).score;
    for a = cfg.advisors.count.practice:cfg.advisors.count.real
        tag = ['advisor' int2str(a)];
        scores(i).(tag) = cfg.advisor(a).SECSscore.values(find(contains(cfg.advisor(a).SECSscore.names,scores(i).name)));
        scores(i).(tag) = round(scores(i).(tag));
    end
end

psum = sum([scores.participant])
pstd = std([scores.participant])
for a = cfg.advisors.count.practice:cfg.advisors.count.real
    tag = ['advisor' int2str(a)];
    tsum = [tag 'sum'];
    tstd = [tag 'std'];
    [tsum ' = ' int2str(sum([scores.(tag)]))]
    [tstd ' = ' num2str(std([scores.(tag)]))]
end
