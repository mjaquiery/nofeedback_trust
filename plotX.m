%% plot X
clear  theta_plot X_plot X_anova
% anova
for o=1:nobservers
    toi = obstype == o-1 ...
        & (step == 0) ...
        ;
    X_anova(:,o) = nansum(X .* toi,2) ./ sum(toi,2);
end

% plot
sem_errorbarbar(X_anova)
stars = {'***','**','*','ms','ns'};
ylim = get(gca,'ylim');
d = diff(get(gca,'ytick'));
d = d(1)/10;
[h pval] = ttest(X_anova(:,3)- X_anova(:,2));
[tilde, sig] = histc(pval,[0 .001 .01 .05 .1 1]);
% text(2.5, ylim(2)+d, num2str(floor(pval*100)/100), ...
%     'FontSize', 15, 'HorizontalAlignment', 'Center');
text(2.5, ylim(2)+d, num2str(stars{sig}), ...
    'FontSize', 15, 'HorizontalAlignment', 'Center');
ylabel(config.ylabel)
set(gcf,'position',[0 0 900 800])
set(gca,'xticklabel',{'baseline', 'Agree in Conf','Agree in Unc'})


% stats
disp('theta ANOVA:')
% disp(['Exp:' num2str(exp) ',fb:' num2str(fb-1) ',thetaMethod:' num2str(theta_method)]);
[Y GROUPS]=prepare_anovan(X_anova);
[p tab] = anovan(Y,GROUPS, ...
    'varnames', {'subject','advisor'}, ...
    'display','off','random',1,'model','full');
tab(:,[1 3 11 6 7])

% planned comparisons
clear yy
yy = X_anova;
[h p ci stats] = ttest(yy(:,2)-yy(:,3))
