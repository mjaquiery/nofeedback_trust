function [qnum] = showAdvisorPolitics(advisorId)
%% Display one political opinion for an advisor
% - Matt Jaquiery, 2017
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

if ~isfield(cfg.advisor(advisorId), 'SECSscore') || isempty(cfg.advisor(advisorId).SECSscore)
    % If the advisor has no opinions, create the skeleton
    cfg.advisor(advisorId).SECSscore.names = fields(SECSscore);
    cfg.advisor(advisorId).SECSscore.values = NaN(1,length(cfg.advisor(advisorId).SECSscore.names));
end

if ~isempty(find(isnan(cfg.advisor(advisorId).SECSscore.values), 1))
    % Empty option available; calculate that and fill it in
    nans = find(isnan(cfg.advisor(advisorId).SECSscore.values));
    % get a random unfilled field
    qnum = randi([1, length(nans)]);
    qnum = nans(qnum);
    qname = cfg.advisor(advisorId).SECSscore.names{qnum};
    % find the participant's score for that field
    pscore = SECSscore.(qname).score;
    % handle reverse scored items correctly
    if SECSscore.(qname).reverse_scored
        pscore = abs(pscore - cfg.SECS.maxS);
    end
    switch cfg.advisor(advisorId).adviceType 
        case 1
            % advisor answer is close to participant's
            % min and max here cap the score to the scale limits (prevents
            % wrapping)
            ascore = min([max([normrnd(pscore, sigma) cfg.SECS.minS]) cfg.SECS.maxS]);
        case 3        
            ascore = min([max([normrnd(pscore, sigma*10) cfg.SECS.minS]) cfg.SECS.maxS]);
        otherwise
            % advisor answer is half a scale removed from participant's
            ascore = abs(min([max([normrnd(pscore, sigma) cfg.SECS.minS]) cfg.SECS.maxS]) - cfg.SECS.maxS);
    end
    %round(ascore/10); % round score to the nearest 10
    cfg.advisor(advisorId).SECSscore.values(qnum) = round(ascore); 
else
    % No empty option, so issue a warning and repeat a value at random
    qnum = randi([1, length(cfg.advisor(advisorId).SECSscore.values)]);
    qname = cfg.advisor(advisorId).SECSscore.names{qnum};
    % find the scores for that field
    pscore = SECSscore.(qname).score;
    % handle reverse scored items correctly
    if SECSscore.(qname).reverse_scored
        pscore = abs(pscore - cfg.SECS.maxS);
    end
    ascore = cfg.advisor(advisorId).SECSscore.values(qnum);
end

% get the universal qnum rather than the advisor's shuffled version
qnum = find(contains(cfg.SECS.questions.name, qname));

drawSECS(qnum, pscore, ascore, advisorId);
