clear X
C = count([trials.step]);
tot = sum(C(1:3,2));

[sum([trials.step]==-1)./tot;
sum([trials.step]==0)./tot;
sum([trials.step]==1)./tot];

hold on
colors={'gd','rs','bs'};
for o = 1:3
    toi = [trials.step]==-1 & [trials.obstype]==o-1;
    X(1) = mean([trials(toi).agree]);
    
    toi = [trials.step]==0 & [trials.obstype]==o-1;
    X(2) = mean([trials(toi).agree]);
    
    toi = [trials.step]==1 & [trials.obstype]==o-1;
    X(3) = mean([trials(toi).agree]);
    
    plot(X,colors{o},'LineWidth',3)
end

figure,hold on
h = histfit(abs([trials([trials.step]==-1).cj1]));set(h(1),'FaceColor','r'),set(h(2),'color','r')
h = histfit(abs([trials([trials.step]==0).cj1]));set(h(1),'FaceColor','b'),set(h(2),'color','b')
h = histfit(abs([trials([trials.step]==1).cj1]));set(h(1),'FaceColor','g'),set(h(2),'color','g')


%% Best fit to distribution
subplot(1,2,1),histfit(abs([trials.cj1]),11),xlim([0 50])

pdf_normmixture = @(x,p,mu1,mu2,sigma1,sigma2) ...
                         p*normpdf(x,mu1,sigma1) + (1-p)*normpdf(x,mu2,sigma2);

x = abs([trials.cj1]);
pStart = .5;
muStart = quantile(x,[.25 .75])
sigmaStart = sqrt(var(x) - .25*diff(muStart).^2)
start = [pStart muStart sigmaStart sigmaStart];

lb = [0 -Inf -Inf 0 0];
ub = [1 Inf Inf Inf Inf];

paramEsts = mle(x, 'pdf',pdf_normmixture, 'start',start, 'lower',lb, 'upper',ub);

options = statset('MaxIter',300, 'MaxFunEvals',600);
paramEsts = mle(x, 'pdf',pdf_normmixture, 'start',start, ...
                          'lower',lb, 'upper',ub, 'options',options)
                      
                      bins = 0:1:50;
subplot(1,2,2),h = bar(bins,histc(x,bins)/(length(x)*.5),'histc');
h.FaceColor = [.9 .9 .9];
xgrid = [1:50];%linspace(1.1*min(x),1.1*max(x),200);
pdfgrid = pdf_normmixture(xgrid,paramEsts(1),paramEsts(2),paramEsts(3),paramEsts(4),paramEsts(5));
hold on
plot(xgrid,pdfgrid,'-r')
hold off
xlabel('x')
ylabel('Probability Density')
