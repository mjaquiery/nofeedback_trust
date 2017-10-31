clear all
close all 

addpath(genpath('C:\Users\npescetelli\Documents\Paris\toolboxes'));

%% plot confidence distribution
clf
x1=[0:1:1000];
z = zscore(x1);
C = normpdf(z,0,1);

plot(z,C,'LineWidth',3)
xlim([-10 10])
ylim([0 1])

set(gcf, 'Position', get(0,'Screensize')); % Maximize figure.

%% plot conditional probability
hold on
Y1=sigmoid(z,1,2)*.5+.5; % agreeing in confidence
Y2=sigmoid(z,1,-2)*.5+.5; % agreeing in uncertainty
Y3=sigmoid(z,1,2)*.5+.5;% accurate agreeing
Y4=sigmoid(z,1,-2)*.5+.5;% accurate agreeing

h1 = line([0 0],[1 0],'LineWidth',2,'LineStyle','--','color','b');

h2 = plot(z,Y1,'LineWidth',3,'color','r')
h3 = plot(z,Y2,'LineWidth',3,'color','b')
h4 = plot(z,Y3,'LineWidth',3,'color','r','LineStyle','--')
h5 = plot(z,Y4,'LineWidth',3,'color','b','LineStyle','--')

set(gca,'FontSize',24)
xlim([-10 10]),xlabel('confidence')
ylim([0 1]),ylabel('p(agree)')

legend([h2 h3 h4 h5], 'high agreement in confidence','high agreement in uncertainty',...
    'low agreement in confidence','low agreement in uncertainty','Location','SouthWest')

