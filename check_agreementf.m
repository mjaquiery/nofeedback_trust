%%
addpath(genpath('/home/niccolo/Dropbox/Oxford/myfunctions'));
addpath(genpath('/media/niccolo/Yupi/Paris/toolboxes/JR_toolbox'));


%%
clear all
figure(1)
% generate confidence judgement
conf=25+5*randn(50,1);

for c=1:5000
    cj(c) = 25+5*randn;
    agree1(c) = agreementf(cj(c),1,conf,'stepwise');
    agree2(c) = agreementf(cj(c),2,conf,'stepwise');
    agree0(c) = agreementf(cj(c),0,conf,'stepwise');
end

hist(conf)

%%
clear X*
figure(2),clf
bins = [0 22 28 50];
for i =1:length(bins)-1 
    hold on
    clear toi
    toi = cj>bins(i) & cj<bins(i+1);
    X1(i) = mean(agree1(toi));
    X2(i) = mean(agree2(toi));
    X0(i) = mean(agree0(toi));
    plot(i,X1(i),'rd')
    plot(i,X2(i),'s')
    plot(i,X0(i),'gs')
end
plot(X1,'r')
plot(X2,'b')
plot(X0,'g')
    
% glmfit([0:5:45]',X1','binomial','link','logit')



