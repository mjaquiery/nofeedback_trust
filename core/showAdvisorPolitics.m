function showAdvisorPolitics(advisorId)
%% Display one political opinion for an advisor
% - by Matt Jaquiery
%
% usage: showAdvisorPolitics(advisorId)
%
% Inputs: 
% advisorId - the ID of the advisor whose opinion to show
%
% Look at the advisor's political opinions, find one which is still unset,
% calculate an appropriate value using the participant's political
% orientation and the political agreement profile of the advisor, and
% display that new value. 
% If all values are taken, issue a warning and repeat a value at random.
%

if nargin < 1
    error('No advisor ID specified.');
end

global cfg;
global SECSscore;
sigma = 1.94; % derived from human variance information from SECS paper
qnum = NaN; % question number
qname = '';
ascore = NaN; % advisor score
pscore = NaN; % participant score

if ~isfield(cfg.advisor(advisorId), 'SECSscore') || ~length(cfg.advisor(advisorId).SECSscore)
    % If the advisor has no opinions, create the skeleton
    cfg.advisor(advisorId).SECSscore.names = fields(SECSscore);
    cfg.advisor(advisorId).SECSscore.values = NaN(1,length(cfg.advisor(advisorId).SECSscore.names));
end

if ~isempty(find(isnan(cfg.advisor(advisorId).SECSscore.values)))
    % Empty option available; calculate that and fill it in
    nans = find(isnan(cfg.advisor(advisorId).SECSscore.values));
    % get a random unfilled field
    qnum = randi([1, length(nans)]);
    qnum = nans(qnum)
    qname = cfg.advisor(advisorId).SECSscore.names{qnum}
    % find the participant's score for that field
    pscore = SECSscore.(qname).score;
    if cfg.advisor(advisorId).adviceType == 1
        % advisor answer is close to participant's
        ascore = normrnd(pscore, sigma);
    else
        % advisor answer is half a scale removed from participant's
        ascore = normrnd(abs(pscore - 5), sigma);
    end
    %round(ascore/10); % round score to the nearest 10
    cfg.advisor(advisorId).SECSscore.values(qnum) = ascore; 
else
    % No empty option, so issue a warning and repeat a value at random
    qnum = randi([1, length(cfg.advisor(advisorId).SECSscore.values)])
    qname = cfg.advisor(advisorId).SECSscore.names{qnum}
    % find the scores for that field
    pscore = SECSscore.(qname).score;
    ascore = cfg.advisor(advisorId).SECSscore.values(qnum);
end

% get the universal qnum rather than the advisor's shuffled version
qnum = find(contains(cfg.SECS.questions.name, qname));

drawSECS(qnum, pscore, ascore, advisorId);
