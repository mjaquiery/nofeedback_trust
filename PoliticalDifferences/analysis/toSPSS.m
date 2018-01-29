function matrix = toSPSS(subject, trials, SECS)
%% return a participant's data formatted into a single matrix for .csv export into SPSS
% Matt Jaquiery, Jan 2018
% 
% usage: matrix = toSPSS(subject, cfg, trials, SECS[, id])
%
% inputs: 
% subject - subject variable object
% cfg - configuration variable object
% trials - trial object
% SECS - political stance questionnaire object
%
% outputs:
% matrix - a matrix where each row is a trial and contains each of the
% fields in subject, cfg, trials, and SECS (dropping those which are not
% suitable, e.g. trials.wheredots

%% column name 'label'. VALUES 0 'val0' /1 'val1' /2 'val2'.: 
% sID 'unique identifier for the subject'. 
% tID 'unique (within subject) identifier for the subject'. 
% tOrder 'presentation order of trial'.
% tBlock 'block trial is in'. 
% tPractice 'whether trial is a practice trial'. VALUES 0 'real' /1 'practice'.
% tResponse1 'initial decision. Ranges from -55 -> 55 (0 excluded) for high-confidence left -> high-confidence right'.
% tCorrect1 'whether initial decision was correct'. VALUES 0 'incorrect' /1 'correct'.
% tAdvice 'which direction did advisor suggest'. VALUES -999 'no advisor' /1 'left' /2 'right'. MISSING -999.
% tResponse2 'final decision. Range as for tResponse1'. MISSING -999.
% tCorrect2 'whether final decision was correct'. MISSING -999.
% tInfluence 'shift towards the direction suggested by advisor'. MISSING -999.
% tWhereLarger 'actual answer'. VALUES 1 'left' /2 'right'.
% tDotDifference '(half of the) difference between left and right boxes'.
% tStep 'subject initial confidence category'. VALUES -1 'unconfident' /0 'moderate' /1 'confident' /-999 'intial answer incorrect'. MISSING -999.
% tAdvisorID 'id of the advisor for this trial'. MISSING -999.
% tAdvisorAccuracy 'whether advisor is correct'. VALUES -999 'no advisor' /0 'incorrect' /1 'correct'. MISSING -999.
% tAgree 'whether advisor and subject agree'. VALUES -999 'no advisor' /0 'disagree' /1 'agree'. MISSING -999.
% tAdvisorPoliticsQ 'advisor political information question id'. VALUES -999 'no question' /1 'Abortion' /2 'Limited government' /3 'Military and national security' /4 'Religion' /5 'Welfare benefits' /6 'Traditional marriage' /7 'Traditional values' /8 'Fiscal responsibility' /9 'Business' /10 'The family unit' /11 'Patriotism'. MISSING -999.
% tStartTime 'trial start time'.
% tResponse1 'time of first response'.
% tResponse2 'time of second response'. MISSING -999.
% qA1Q1 'How accurate do you think this ADVISOR1 was when performing the task?'. MISSING -999.
% qA1Q2 'How much do you like ADVISOR1?'. MISSING -999.
% qA1Q3 'How trustworthy are the opinions of ADVISOR1?'. MISSING -999.
% qA1Q4 'How much are you influenced by the opinions of ADVISOR1?'. MISSING -999.
% qA2Q1 'How accurate do you think this ADVISOR2 was when performing the task?'. MISSING -999.
% qA2Q2 'How much do you like ADVISOR2?'. MISSING -999.
% qA2Q3 'How trustworthy are the opinions of ADVISOR2?'. MISSING -999.
% qA2Q4 'How much are you influenced by the opinions of ADVISOR2?'. MISSING -999.
% a1EstimatedAccuracy 'subject guess at accuracy of ADVISOR1'. MISSING -999.
% a2EstimatedAccuracy 'subject guess at accuracy of ADVISOR2'. MISSING -999.
% sGender 'subject gender'. VALUES 0 'female' /1 'male /2 'other'.
% sAge 'subject age'.
% SECSAbortion 'Abortion'.
% SECSGovernment 'Limited government'. 
% SECSSecurity 'Military and national security'.
% SECSReligion 'Religion'.
% SECSWelfare 'Welfare benefits'.
% SECSMarriage 'Traditional marriage'.
% SECSValues 'Traditional values'.
% SECSMoney 'Fiscal responsibility'.
% SECSBusiness 'Business'.
% SECSFamily 'The family unit'.
% SECSPatriotism 'Patriotism'.

%% Code
matrix = zeros(length(trials),44);
% set every value to missing and fill in existing values:
matrix(1:end) = -999;
for t = 1:length(trials)
    % sID 'unique identifier for the subject'. 
    matrix(t,1) = str2num(subject.id);
    % tID 'unique (within subject) identifier for the subject'. 
    matrix(t,2) = trials(t).id;
    % tOrder 'presentation order of trial'.
    matrix(t,3) = t;
    % tBlock 'block trial is in'. 
    matrix(t,4) = trials(t).block;
    % tPractice 'whether trial is a practice trial'. VALUES 0 'real' /1 'practice'.
    matrix(t,5) = trials(t).practice;
    % tResponse1 'initial decision. Ranges from -55 -> 55 (0 excluded) for high-confidence left -> high-confidence right'.
    matrix(t,6) = trials(t).cj1;
    % tCorrect1 'whether initial decision was correct'. VALUES 0 'incorrect' /1 'correct'.
    matrix(t,7) = trials(t).cor1;
    % tAdvice 'which direction did advisor suggest'. VALUES -999 'no advisor' /1 'left' /2 'right'. MISSING -999.
    if ~isnan(trials(t).advisorId)
       dir = 1;
       if trials(t).wherelarger == 2 && trials(t).obsacc == 1
           dir = 2;
       elseif trials(t).wherelarger == 1 && trials(t).obsacc == 0
           dir = 2;
       end
       matrix(t,8) = dir;
    end
    % tResponse2 'final decision. Range as for tResponse1'. MISSING -999.
    if ~isnan(trials(t).advisorId), matrix(t,9) = trials(t).cj2; end
    % tCorrect2 'whether final decision was correct'. MISSING -999.
    if ~isnan(trials(t).advisorId), matrix(t,10) = trials(t).cor2; end
    % tInfluence 'shift towards the direction suggested by advisor'. MISSING -999.
    if ~isnan(trials(t).advisorId)
        % this follows as written in Niccolo's thesis
        cj1 = trials(t).cj1;
        cj2 = trials(t).cj2;
        if cj1 < 0
            cj1 = abs(cj1);
            cj2 = cj2 * -1;
        end
        if trials(t).agree
            matrix(t,11) = cj2 - cj1; 
        else
            matrix(t,11) = (cj2 - cj1)*-1;
        end
    end
    % tWhereLarger 'actual answer'. VALUES 1 'left' /2 'right'.
    matrix(t,12) = trials(t).wherelarger;
    % tDotDifference '(half of the) difference between left and right boxes'.
    matrix(t,13) = trials(t).dotdifference;
    % tStep 'subject initial confidence category'. VALUES -1 'unconfident' /0 'moderate' /1 'confident' /-999 'intial answer incorrect'. MISSING -999.
    if ~isnan(trials(t).step), matrix(t,14) = trials(t).step; end
    % tAdvisorID 'id of the advisor for this trial'. MISSING -999.
    if ~isnan(trials(t).advisorId), matrix(t,15) = trials(t).advisorId; end    
    % tAdvisorAccuracy 'whether advisor is correct'. VALUES -999 'no advisor' /0 'incorrect' /1 'correct'. MISSING -999.
    if ~isnan(trials(t).advisorId), matrix(t,16) = trials(t).obsacc; end
    % tAgree 'whether advisor and subject agree'. VALUES -999 'no advisor' /0 'disagree' /1 'agree'. MISSING -999.
    if ~isnan(trials(t).advisorId), matrix(t,17) = trials(t).agree; end
    % tAdvisorPoliticsQ 'advisor political information question id'. VALUES -999 'no question' /1 'Abortion' /2 'Limited government' /3 'Military and national security' /4 'Religion' /5 'Welfare benefits' /6 'Traditional marriage' /7 'Traditional values' /8 'Fiscal responsibility' /9 'Business' /10 'The family unit' /11 'Patriotism'. MISSING -999.
    if ~isnan(trials(t).advisorId), matrix(t,18) = trials(t).advisorPoliticsQ; end
    % tStartTime 'trial start time'.
    matrix(t,19) = trials(t).time_starttrial;
    % tResponse1 'time of first response'.
    matrix(t,20) = trials(t).resp1_time;
    % tResponse2 'time of second response'. MISSING -999.
    if ~isnan(trials(t).advisorId), matrix(t,21) = trials(t).resp2_time; end
    % qA1Q1 'How accurate do you think this ADVISOR1 was when performing the task?'. MISSING -999.
    if ~isempty(trials(t).qanswers) && trials(t).qanswers(1).obs==1 
        matrix(t,22) = trials(t).qanswers(1).ans; 
    end
    % qA1Q2 'How much do you like ADVISOR1?'. MISSING -999.
    if ~isempty(trials(t).qanswers) && trials(t).qanswers(2).obs==1 
        matrix(t,23) = trials(t).qanswers(2).ans; 
    end
    % qA1Q3 'How trustworthy are the opinions of ADVISOR1?'. MISSING -999.
    if ~isempty(trials(t).qanswers) && trials(t).qanswers(3).obs==1 
        matrix(t,24) = trials(t).qanswers(3).ans; 
    end
    % qA1Q4 'How much are you influenced by the opinions of ADVISOR1?'. MISSING -999.
    if ~isempty(trials(t).qanswers) && trials(t).qanswers(4).obs==1 
        matrix(t,25) = trials(t).qanswers(4).ans; 
    end
    % qA2Q1 'How accurate do you think this ADVISOR2 was when performing the task?'. MISSING -999.
    if ~isempty(trials(t).qanswers) && trials(t).qanswers(1).obs==2 
        matrix(t,26) = trials(t).qanswers(1).ans; 
    end
    % qA2Q2 'How much do you like ADVISOR2?'. MISSING -999.
    if ~isempty(trials(t).qanswers) && trials(t).qanswers(2).obs==2 
        matrix(t,27) = trials(t).qanswers(2).ans; 
    end
    % qA2Q3 'How trustworthy are the opinions of ADVISOR2?'. MISSING -999.
    if ~isempty(trials(t).qanswers) && trials(t).qanswers(3).obs==2 
        matrix(t,28) = trials(t).qanswers(3).ans; 
    end
    % qA2Q4 'How much are you influenced by the opinions of ADVISOR2?'. MISSING -999.
    if ~isempty(trials(t).qanswers) && trials(t).qanswers(4).obs==2
        matrix(t,29) = trials(t).qanswers(4).ans; 
    end
    % a1EstimatedAccuracy 'subject guess at accuracy of ADVISOR1'. MISSING -999.
    if ~isempty(trials(t).estim_obsacc), matrix(t,30) = trials(t).estim_obsacc(1); end
    % a2EstimatedAccuracy 'subject guess at accuracy of ADVISOR2'. MISSING -999.
    if ~isempty(trials(t).estim_obsacc), matrix(t,31) = trials(t).estim_obsacc(2); end
    % sGender 'subject gender'. VALUES 0 'female' /1 'male /2 'other'.
    if subject.gender(1) == 'F' || subject.gender(1) == 'f'
        matrix(t,32) = 0;
    elseif subject.gender(1) == 'M' || subject.gender(1) == 'm'
        matrix(t,32) = 1;
    else
        matrix(t,32) = 2;
    end
    % sAge 'subject age'.
    matrix(t,33) = subject.age;
    % SECSAbortion 'Abortion'.
    matrix(t,34) = SECS.abortion.score;
    % SECSGovernment 'Limited government'. 
    matrix(t,35) = SECS.government.score;
    % SECSSecurity 'Military and national security'.
    matrix(t,36) = SECS.security.score;
    % SECSReligion 'Religion'.
    matrix(t,37) = SECS.religion.score;
    % SECSWelfare 'Welfare benefits'.
    matrix(t,38) = SECS.welfare.score;
    % SECSMarriage 'Traditional marriage'.
    matrix(t,39) = SECS.marriage.score;
    % SECSValues 'Traditional values'.
    matrix(t,40) = SECS.values.score;
    % SECSMoney 'Fiscal responsibility'.
    matrix(t,41) = SECS.money.score;
    % SECSBusiness 'Business'.
    matrix(t,42) = SECS.business.score;
    % SECSFamily 'The family unit'.
    matrix(t,43) = SECS.family.score;
    % SECSPatriotism 'Patriotism'.
    matrix(t,44) = SECS.patriotism.score;
end