%% post experiment diagnostics

%%
figure
hist([trials.cj1])

%%
figure
plot(nancumsum([trials.cor]) ./ [1:length(trials)])

%%
figure
c = 0;s=1;
for t = [find([trials.questionnaire]) length(trials)]
    c = c+1; % counter
    
    % trust answers: subject * observer * question * questionnaire
    Tanswers(s,:,:,c) = reshape([trials(t).qanswers.ans]',4,3)';
end
Tanswers=squeeze(Tanswers);

colors=['g' 'r' 'b'];
for q =1:4
    for o=1:3
        subplot(2,2,q)
        hold on 
        plot(squeeze(Tanswers(o,q,:)),'color',colors(o))
    end
end

%% proportions of steps
toi = [trials.cor]==1;
C= count([trials(toi).step]);
tot = sum(C(1:3,2));
props(s,:) = [sum(sum([trials.step]==-1))./tot;
    sum(sum([trials.step]==0))./tot;
    sum(sum([trials.step]==1))./tot]';
figure
bar(props)
ylabel('proportions of step selection')
set(gca,'xticklabel',{'step -1' 'step 0' 'step 1'})