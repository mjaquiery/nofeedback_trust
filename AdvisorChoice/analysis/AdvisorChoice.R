## Analysis script for AdvisorChoice experiment data ##############################################
# Matt Jaquiery, Jan 2018 (matt.jaquiery@psy.ox.ac.uk)
#
# i) Get Data
# ii) Function definitions - could move to another file
# iii) Prepare data
# 0) Exclusions
# 1) Demographics
# 2) Is the agree-in-confidence advisor selected more often?
# 3) ANOVA investigating influence
# 4) Trust questionnaire answers
#   i. Trust for each advisor
# 5) Do participants simply prefer agreement?
# --NON-Preregistered stuff--
# 6) Descriptives

## Packages and citations:

# Bayes stuff
if(!require('BayesFactor')) {
  install.packages('BayesFactor')
  library(BayesFactor)
}
citation("BayesFactor")
# effect sizes for t.tests
if(!require('lsr')) {
  install.packages('lsr')
  library(lsr)
}
citation("lsr")
if(!require('tidyverse')) {
  install.packages('tidyverse')
  library(tidyverse)
}
source('splitViolin.R')
citation('tidyverse')
if(!require('modelr')) {
  install.packages('modelr')
  library(modelr)
}
citation('modelr')
if(!require('reshape2')) {
  install.packages('reshape2')
  library(reshape2)
}
citation('reshape2')
if(!require('ez')) {
  install.packages('ez')
  library(ez)
}
citation('ez')

# Used by mat2R to read the .mat files containing raw data
citation("R.matlab")

## i) Get Data ####################################################################################
print('Loading data')

# Load utility functions
source('utilityFunctions.R')

# save path for figures
figPath = 'G:/Documents/University/Google Drive/Project Documents/AdvisorChoice/figures/'

## Loading raw data from MATLAB; only needs to be done rarely. Most of the time we just load the 
## saved data using the load command (see below).
## Use mat2R function to acquire and translate the MATLAB data
# source("mat2R.R")
# acPth <- "G:/Documents/University/Google Drive/Results/AdvisorChoice"
# raw_study <- getMatlabData(acPth)  # get some data from the path defined for convenience in mat2R
# raw_study$participants$nominalParticipantId <- NULL    # Drop the ID that links to lab journal records
# raw_study$participants$name <- NULL    # Repeat of nominal Id
# raw_study$participants$date <- NULL    # Potentially allows identification through lab records
# raw_study$participants$start.time.year <- NULL
# raw_study$participants$start.time.month <- NULL
# raw_study$participants$start.time.day <- NULL
# raw_study$participants$start.time.hour <- NULL
# raw_study$participants$start.time.minute <- NULL
# raw_study$participants$start.time.second <- NULL
# save(raw_study, file = 'study_data.RData')    # Save this so we're working from here each time

# load the raw data into the workspace as 'raw_study'
load('study_data.RData')

# unpack
participants <- raw_study$participants
advisors <- raw_study$advisors
trials <- raw_study$trials
questionnaires <- raw_study$questionnaires

## ii) Reference vars #############################################################################

# all advisors agreement probability = .3 where initial judgement is incorrect
adviceTypes <- list(neutral=0, # neutral: agree|correct=.7
                    AiC=1, # agree-in-confidence: agree|correct+highConfidence=.8, agree|correct+medConfidence=.7, agree|correct+lowConfidence=.6
                    AiU=2) # agree-in-uncertainty: agree|correct+highConfidence=.6, agree|correct+medConfidence=.7, agree|correct+lowConfidence=.8

adviceTypeNames <- list('Agree-in-confidence', 'Agree-in-uncertainty')
adviceTypeShortNames <- list('AiC', 'AiU')

# the 'step' variable for a trial tells us whether the initial decision was high/med/low confidence with respect to recent scale usage
# NOTE: this is only avaiable for correct trials
confidenceTypes <- list(low=-1,
                        medium=0,
                        high=1)

# Advisor questionnaire dimensions
questionnaireDimensions <- list(accurate=1,
                                like=2,
                                trust=3,
                                influence=4)

# text of the question number qId.
# NOTE: questions at the first timepoint are presented to the participant prior to their 
# having interacted with the advisors, and are therefore worded prospectively 
# (e.g. 'how much will you like this advisor?')
getQuestionText <- function (qId, short=FALSE) {
  if(qId==questionnaireDimensions$accurate)
    return(ifelse(short,'Accurate','How accurate do you think this person was when performing the task?'))
  if(qId==questionnaireDimensions$like)
    return(ifelse(short,'Likeable','How much do you like this person?'))
  if(qId==questionnaireDimensions$trust)
    return(ifelse(short,'Trustworthy','How trustworthy are the opinions of this person?'))
  if(qId==questionnaireDimensions$influence)
    return(ifelse(short,'Influential','How much are you influenced by the opinions of this person?'))
  return(NA)
} 


## iii) Prepare data ##############################################################################
print('## Prepare data ##################################################################')
# We'll do most of the work with the 'participants' table.
# This table will need some derived stats such as means for trials under various conditions

# First, remove practice trials from the main trials table
all.trials <- trials
trials <- trials[trials$practice==0 & trials$block > 2,]
trials <- trials[which(trials$participantId %in% participants$participantId),] # discard trials for excluded participants

# we'll also get rid of the advisors, though this is less important
all.advisors <- advisors
advisors <- advisors[advisors$advice.type!=adviceTypes$neutral,] # neutral = even advice, used for the practice advisors only
advisors <- advisors[which(advisors$participantId %in% participants$participantId),]

# Some trial variables need to be coerced into their true numeric form
trials$advisorId <- as.numeric(trials$advisorId)
trials$cor1 <- as.numeric(trials$cor1)
trials$cor2 <- as.numeric(trials$cor2)
trials$cj1 <- as.numeric(trials$cj1)
trials$cj2 <- as.numeric(trials$cj2)
trials$agree <- as.numeric(trials$agree)
trials$block <- as.numeric(trials$block)

# Next we calculate utility variables for each trial to simplify things later.
for(i in 1:dim(trials)[1]) {
  # When checking for presence of a choice we can check for a sum of 3 since the
  # only real choice is advisor 2 vs advisor 1.
  trials$hasChoice[i] <- sum(trials[i,'choice'][[1]], na.rm = T)==3
  # We also want to store the advice type for each trial
  if (is.nan(as.numeric(trials$advisorId[i])) || trials$advisorId[i] < 1)
    trials$adviceType[i] <- NA
  else
    trials$adviceType[i] <- advisors$advice.type[which(advisors$participantId==trials$participantId[i] 
                                                       & advisors$id==trials$advisorId[i])]
}

# Calculate the influence of the advisor on each trial
trials$shift <- vector(length = dim(trials)[1]) #  amount the confidence changes
trials$influence <- trials$shift # amount the confidence changes in the direction of the advice
# also record influence capped by the maximum possible shift which could have
# occurred in agreement direction to counteract the role of the scale asymmetry
trials$cappedInfluence <- trials$shift 
for(t in 1:dim(trials)[1]) {
  if(is.na(trials$adviceType[t])) {
    # trials without an advisor are entered as NA
    trials$shift[t] <- NA
    trials$influence[t] <- NA
    trials$cappedInfluence[t] <- NA
  } else {
    # calculate the confidence change on this trial ('shift')
    # shift = Cpost - Cpre
    # C-values are *-1 if answer was 'left', so
    # Cpre [1,55]
    # Cpost [-55,-1]U[1,55]
    c.pre <- trials$cj1[t]
    c.post <- trials$cj2[t]
    max.shift <- 55 - abs(trials$cj1[t])
    if(c.pre<0) {
      # initial response is 'left'
      c.pre <- c.pre * -1
      c.post <- c.post * -1
    } 
    trials$shift[t] <- c.post - c.pre # +ve values indicate shift towards more confidence in initial response
    if(trials$agree[t]==1) {
      # on agreement trials shift towards initial response inidicates following advice
      trials$influence[t] <- trials$shift[t]
      trials$cappedInfluence[t] <- ifelse(abs(trials$shift[t])>max.shift,max.shift,trials$shift[t])
    } else {
      # on disagreement trials shift AWAY from initial response inidicates following advice
      trials$influence[t] <- trials$shift[t] * -1
      trials$cappedInfluence[t] <- ifelse(abs(trials$shift[t])>max.shift,max.shift,trials$shift[t]) * -1
    }
  }
}

# Now we can use the real trial list to get some by-participant values
for (p in 1:dim(participants)[1]) {
  set <- trials[which(trials$participantId == participants$participantId[p]),]
  total <- scanTrials(set)
  for(n in 1:length(names(total)))
    participants[p, names(total)[n]] <- total[,n]
  # also record values for each block
  for (b in 1:length(unique(set$block))) {
    block <- unique(set$block)[b]
    byBlock <- scanTrials(set[which(set$block==block),], paste0('block', block))
    for(n in 1:length(names(byBlock))) 
      participants[p, names(byBlock)[n]] <- byBlock[,n]
  }
  # It involves a lot of duplication of values we don't need, but coding-wise
  # it's quicker to run scanTrials again and feed it the cappedInfluence stuff
  set$influence <- set$cappedInfluence
  total <- scanTrials(set, 'capped')
  total <- total[,grep('Influence', names(total), fixed = T)]    # only save the influence scores
  for(n in 1:length(names(total)))
    participants[p, names(total)[n]] <- total[,n]
}

# We don't need to deal with nearly 1000 columns for most stuff, so we can archive the by-block stuff
participants.byBlock <- participants
participants <- participants[,grep('.block', names(participants), fixed = T, invert = T)]

# Finally we can come up with some reshaped lists which will be useful for ANOVAs
participants.influence <- melt(participants, id.vars = 'participantId', measure.vars = c('aicAgreeChoiceInfluence',
                                                                                         'aicAgreeForcedInfluence',
                                                                                         'aicDisagreeChoiceInfluence',
                                                                                         'aicDisagreeForcedInfluence',
                                                                                         'aiuAgreeChoiceInfluence',
                                                                                         'aiuAgreeForcedInfluence',
                                                                                         'aiuDisagreeChoiceInfluence',
                                                                                         'aiuDisagreeForcedInfluence'))
participants.influence$AiC <- participants.influence$variable %in% c('aicAgreeForcedInfluence',
                                                                     'aicAgreeChoiceInfluence',
                                                                     'aicDisagreeForcedInfluence',
                                                                     'aicDisagreeChoiceInfluence')
participants.influence$agree <- participants.influence$variable %in% c('aicAgreeForcedInfluence',
                                                                       'aiuAgreeForcedInfluence',
                                                                       'aicAgreeChoiceInfluence',
                                                                       'aiuAgreeChoiceInfluence')
participants.influence$hasChoice <- participants.influence$variable %in% c('aicAgreeChoiceInfluence',
                                                                           'aiuAgreeChoiceInfluence',
                                                                           'aicDisagreeChoiceInfluence',
                                                                           'aiuDisagreeChoiceInfluence')
participants.influence$AiC <- factor(participants.influence$AiC)
participants.influence$agree <- factor(participants.influence$agree)
participants.influence$hasChoice <- factor(participants.influence$hasChoice)
participants.influence$participantId <- factor(participants.influence$participantId)
participants.influence.medConf <- melt(participants, id.vars = 'participantId', 
                                       measure.vars = c('aicAgreeChoiceInfluence.medConf',
                                                        'aicAgreeForcedInfluence.medConf',
                                                        'aicDisagreeChoiceInfluence.medConf',
                                                        'aicDisagreeForcedInfluence.medConf',
                                                        'aiuAgreeChoiceInfluence.medConf',
                                                        'aiuAgreeForcedInfluence.medConf',
                                                        'aiuDisagreeChoiceInfluence.medConf',
                                                        'aiuDisagreeForcedInfluence.medConf'))
participants.influence.medConf$AiC <- participants.influence.medConf$variable %in% c('aicAgreeForcedInfluence.medConf',
                                                                                     'aicAgreeChoiceInfluence.medConf',
                                                                                     'aicDisagreeForcedInfluence.medConf',
                                                                                     'aicDisagreeChoiceInfluence.medConf')
participants.influence.medConf$agree <- participants.influence.medConf$variable %in% c('aicAgreeForcedInfluence.medConf',
                                                                                       'aiuAgreeForcedInfluence.medConf',
                                                                                       'aicAgreeChoiceInfluence.medConf',
                                                                                       'aiuAgreeChoiceInfluence.medConf')
participants.influence.medConf$hasChoice <- participants.influence.medConf$variable %in% c('aicAgreeChoiceInfluence.medConf',
                                                                                           'aiuAgreeChoiceInfluence.medConf',
                                                                                           'aicDisagreeChoiceInfluence.medConf',
                                                                                           'aiuDisagreeChoiceInfluence.medConf')
participants.influence.medConf$AiC <- factor(participants.influence.medConf$AiC)
participants.influence.medConf$agree <- factor(participants.influence.medConf$agree)
participants.influence.medConf$hasChoice <- factor(participants.influence.medConf$hasChoice)
participants.influence.medConf$participantId <- factor(participants.influence.medConf$participantId)

# And once more for the capped influence score. Probably a better way to do
# this, but not going to look into it right now
participants.influence.capped <- melt(participants, id.vars = 'participantId', 
                                      measure.vars = c('aicAgreeChoiceInfluence.capped',
                                                       'aicAgreeForcedInfluence.capped',
                                                       'aicDisagreeChoiceInfluence.capped',
                                                       'aicDisagreeForcedInfluence.capped',
                                                       'aiuAgreeChoiceInfluence.capped',
                                                       'aiuAgreeForcedInfluence.capped',
                                                       'aiuDisagreeChoiceInfluence.capped',
                                                       'aiuDisagreeForcedInfluence.capped'))
participants.influence.capped$AiC <- participants.influence.capped$variable %in% 
  c('aicAgreeForcedInfluence.capped', 'aicAgreeChoiceInfluence.capped', 
    'aicDisagreeForcedInfluence.capped', 'aicDisagreeChoiceInfluence.capped')
participants.influence.capped$agree <- participants.influence.capped$variable %in% 
  c('aicAgreeForcedInfluence.capped', 'aiuAgreeForcedInfluence.capped',
    'aicAgreeChoiceInfluence.capped', 'aiuAgreeChoiceInfluence.capped')
participants.influence.capped$hasChoice <- participants.influence.capped$variable %in% 
  c('aicAgreeChoiceInfluence.capped', 'aiuAgreeChoiceInfluence.capped',
    'aicDisagreeChoiceInfluence.capped', 'aiuDisagreeChoiceInfluence.capped')
participants.influence.capped$AiC <- factor(participants.influence.capped$AiC)
participants.influence.capped$agree <- factor(participants.influence.capped$agree)
participants.influence.capped$hasChoice <- factor(participants.influence.capped$hasChoice)
participants.influence.capped$participantId <- factor(participants.influence.capped$participantId)

## 0) Exclusions ##################################################################################
print('## 0) Running exclusions #########################################################')
# Exclusion rules:
# Proportion of correct initial judgements must be (.60 < cor1/n < .90) 
#NB:practice trials are INCLUDED in this since they are used in part for
#determining confidence calibration
participants$excluded <- participants$proportionCorrect < .6 || participants$proportionCorrect > .9
exclusions <- data.frame(participantId = participants$participantId[participants$excluded],
                         proportionCorrect = participants$proportionCorrect[participants$excluded])
exclusions

# Trim the dataset down to the first 24 participants as promised in the pre-registration
participants$excluded[25:dim(participants)[1]] = T
participants.byBlock$excluded <- participants$excluded

# Drop excluded participant data from the long table
all.participants.influence <- participants.influence
participants.influence <- participants.influence[which(!participants.influence$participantId 
                                                        %in% which(participants$excluded)),] 
all.participants.influence.medConf <- participants.influence.medConf
participants.influence.medConf <- participants.influence.medConf[which(!participants.influence.medConf$participantId
                                                                       %in% participants$participantId[
                                                                         which(participants$excluded)]),]
all.participants.influence.capped <- participants.influence.capped
participants.influence.capped <- participants.influence.capped[which(!participants.influence.capped$participantId
                                                                       %in% participants$participantId[
                                                                         which(participants$excluded)]),]
# Also drop excluded participants from trials and questionnaires
all.questionnaires <- questionnaires
questionnaires <- questionnaires[which(!questionnaires$participantId 
                                       %in% participants$participantId[which(participants$excluded)]),]
trials <- trials[which(!trials$participantId %in% participants$participantId[which(participants$excluded)]),]
# backup the full participant list and reframe the original to honour exclusions
all.participants <- participants
participants <- participants[participants$excluded==F,]
print(paste('>>Excluded participant count:', length(which(all.participants$excluded==T))))
all.participants.byBlock <- participants.byBlock
participants.byBlock <- participants.byBlock[participants.byBlock$excluded==F,]

## 1) Demographics ################################################################################
print('## 1) Demographics ###############################################################')

demographics <- data.frame('N'=dim(participants)[1],
                           'age_mean'=mean(participants$age),
                           'age_sd'=sd(participants$age),
                           'age_min'=min(participants$age),
                           'age_max'=max(participants$age),
                           'males'=length(which(tolower(participants$gender)=='m')),
                           'females'=length(which(tolower(participants$gender)=='f')),
                           'other_gender'=length(which(tolower(participants$gender)!='m' && tolower(participants$gender)!='f')))
demographics

## 2) Is the agree-in-confidence advisor selected more often? ###################################### 
print('## 2) TEST Preferential selection for agree-in-confidence advisor? ###############')

#We want to know whether the advisor who agrees with the participant when the
#participant expresses higher confidence is selected more often by the
#participant when a choice is offered.
#
#We will find this out by taking the number of times each participant selected
#the agree-in-confidence advisor and dividing by the total number of choice
#trials for that participant (should be the same for all participants). We can
#then take the mean of this proportion across participants and test it for
#significant versus the null hypothesis of random picking (0.5).

aic.selection <- data.frame('mean' = mean(participants$aicPickRate), 
                            'sd' = sd(participants$aicPickRate), 
                            row.names = 'proportion of Agree-in-Confidence picks')
aic.selection

aic.test <- t.test(participants$aicPickRate, mu=0.5) # testing the proportions versus the null hypothesis of 0.5 (chance selection)
aic.test.d <- cohensD(participants$aicPickRate, mu=0.5)
print('>>(aic.test) choice proportion Agree-in-Confidence vs. chance level (.5)')
prettyPrint(aic.test, aic.test.d)
print('>>(aic.test.b) bayesian examination of above (prior = mean of 0.5, sd as empirically observed)')
aic.test.b <- ttestBF(participants$aicPickRate, mu = 0.5)
print(aic.test.b)
print(paste0('Evidence strength for preferential AiC picking: BF=', round(exp(aic.test.b@bayesFactor$bf),3)))
# Present the result with a graph
tmp <- melt(participants[,c("participantId","aicPickRate.lowConf","aicPickRate.medConf","aicPickRate.highConf")],
            id.vars = 'participantId')
levels(tmp$variable) <- c('Low', 'Medium', 'High')
tmp$participantId <- as.factor(tmp$participantId)
graph.pickRate <- ggplot(tmp, aes(x = variable, y = value)) +
  geom_hline(linetype = "dashed", color = "black", yintercept = .5, size = 1) +
  geom_point(aes(color = participantId)) +
  geom_line(aes(group = factor(participantId), color = participantId)) +
  stat_summary(geom = "errorbar", fun.data = "mean_cl_boot", width = 0.1) +
  stat_summary(geom = "point", fun.y = "mean", shape = 23, fill = "black", size = 4) +
  geom_point(position = position_jitter(w=0.03, h=0),
             aes(x="Overall", y=aicPickRate, color = factor(participantId)), data = participants) +
  stat_summary(geom = "errorbar", fun.data = "mean_cl_boot", width = 0.1,
               aes(x="Overall", y=aicPickRate), data = participants) +
  stat_summary(geom = "point", fun.y = "mean", shape = 23, fill = "black", size = 4,
               aes(x="Overall", y=aicPickRate), data = participants) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_x_discrete(expand = c(0,1), limits = c('Low', 'Medium',
                                               'High', 'Overall')) +
  theme_light() +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Advisor preference",
       subtitle = paste(strwrap(paste("Proportion of the time each participant picked the",
                                      "agree-in-confidence advisor. Connected points of a colour indicate",
                                      "data from a single participant, while the diamond indicates the",
                                      "mean proportion across all participants. The dashed reference line indicates",
                                      "picking both advisors equally, as would be expected by chance.",
                                      "Error bars give 95% bootstrapped confidence intervals.", 
                                      sep = " "), 
                                width=115), collapse = "\n"),
       legend = NULL,
       x = "Confidence",
       y = "Proportion of the time the agree-in-confidence advisor is chosen") 
graph.pickRate
ggsave(paste0(figPath, "aicPickRate.png"), plot = graph.pickRate)

# Also look at advisor choice in the medium confidence trials
print('Agree-in-confidence pick rate on medium trials')
aic.selection.medConf <- data.frame('mean' = mean(participants$aicPickRate.medConf), 
                            'sd' = sd(participants$aicPickRate.medConf), 
                            row.names = 'proportion of Agree-in-Confidence picks')
aic.selection.medConf

aic.test.medConf <- t.test(participants$aicPickRate.medConf, mu=0.5) # testing the proportions versus the null hypothesis of 0.5 (chance selection)
aic.test.medConf.d <- cohensD(participants$aicPickRate.medConf, mu=0.5)
print('>>(aic.test.medConf) choice proportion Agree-in-Confidence on medium confidence trials vs. chance level (.5)')
prettyPrint(aic.test.medConf, aic.test.medConf.d)
print('>>(aic.test.medConf.b) bayesian examination of above (prior = mean of 0.5, sd as empirically observed)')
aic.test.medConf.b <- ttestBF(participants$aicPickRate.medConf, mu = 0.5)
print(aic.test.medConf.b)
print(paste0('Evidence strength for preferential AiC picking: BF=', round(exp(aic.test.medConf.b@bayesFactor$bf),3)))

## 3) ANOVA investigating influence ###############################################################
print('## 3) ANOVA investigating influence ##############################################')

#Influence is defined as the extent to which the judge's (participant's) final
#decision has moved from their initial decision in the direction of the advice
#received.

# 2x2x2 ANOVA investigating effects of advisor type
# (agree-in-confidence/uncertainty), choice (un/forced), and agreement
# (dis/agree) on influence. These are all within-subjects manipulations (meaning
# we include an error term of participantID and its child effects).
print('Running ANOVAs')
anova.influence <- ezANOVA(data = participants.influence,
                           dv = value, 
                           wid = participantId,
                           within = c('AiC', 'agree', 'hasChoice'),
                           return_aov = T)
print('>>(anova.influence)')
anova.influence$ANOVA
tmp <- participants.influence
levels(tmp$AiC) <- c('Agree-in-uncertainty', 'Agree-in-confidence') 
levels(tmp$agree) <- c('Disagree', 'Agree')
levels(tmp$hasChoice) <- c('Forced trials', 'Choice trials')
graph.anova.influence <- ggplot(tmp, aes(agree, value, color = AiC, fill = AiC)) +
  geom_point(position = position_jitter(w=0.05, h=0)) +
  stat_summary(geom = "errorbar",
               fun.data = "mean_cl_boot",
               width = 0.2) +
  stat_summary(geom = "point",
               fun.y = "mean",
               shape = 23, size = 5) +
  stat_summary(aes(group = AiC), fun.y=mean, geom="line") + 
  facet_grid(.~hasChoice) +
  scale_color_discrete(name = 'Advisor type') +
  scale_fill_discrete(name = 'Advisor type') +
  theme_light() +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Advice Influence",
       subtitle = paste(strwrap(paste("Influence of advice under varied conditions. Points indicate",
                                      "mean values for a participant, while diamonds indicate the mean",
                                      "of participant means, with error bars specifying 95% confidence intervals.",
                                      sep = " "), 
                                width=115), collapse = "\n"),
       legend = NULL,
       x = 'Agreement between advisor and judge',
       y = "Influence of the advice") 
graph.anova.influence
ggsave(paste0(figPath, "influence.png"), plot = graph.anova.influence)

print('ANOVA medium confidence trials only')
tmp <- participants.influence.medConf
tmp$value[which(is.nan(tmp$value))] <- 0.00
anova.influence.medConf <- ezANOVA(data = tmp,
                                   dv = value, 
                                   wid = participantId,
                                   within = c('AiC', 'agree', 'hasChoice'),
                                   return_aov = T)
print('>>(anova.influence.medConf)')
anova.influence.medConf$ANOVA

## Maybe a better approach is just drop choice trials
print('ANOVA medium confidence trials without hasChoice')
tmp <- melt(participants, id.vars = c('participantId'),
            measure.vars = c('aicAgreeForcedInfluence.medConf',
                             'aiuAgreeForcedInfluence.medConf',
                             'aicDisagreeForcedInfluence.medConf',
                             'aiuDisagreeForcedInfluence.medConf'))
tmp$AiC <- as.numeric(grepl('aic', tmp$variable, fixed = T))
tmp$agree <- as.numeric(grepl('Agree', tmp$variable, fixed = T))
anova.influence.medConf.forced <- ezANOVA(data = tmp,
                                          dv = value, 
                                          wid = participantId,
                                          within = c('AiC', 'agree'),
                                          return_aov = T)
print('>>(anova.influence.medConf.forced)')
anova.influence.medConf.forced$ANOVA

## 4) Trust questionnaire answers #################################################################
print('## 4) Trust questionnaire answers ################################################')
#   i. Trust for each advisor

# We want to know if trust for each advisor changes over time. 
# 
# Questions:
# 1) Advisor accuracy
# 2) Advisor likeability
# 3) Advisor trustworthiness
# 4) Advisor influence

lastTimePoint <- max(questionnaires$timePoint)
questionnaires$answer <- as.numeric(questionnaires$answer)
questionnaires$questionId <- as.numeric(questionnaires$questionId)
questionnaires$questionNumber <- as.numeric(questionnaires$questionNumber)
# Look up advisorType and put it in the table for ease of reference
questionnaires$adviceType <- vector(length=dim(questionnaires)[1])
for(q in 1:dim(questionnaires)[1]) {
  questionnaires$adviceType[q] <- advisors$advice.type[advisors$id==questionnaires$advisorId[q] 
                                                       & advisors$participantId==questionnaires$participantId[p]]
  questionnaires$questionText[q] <- getQuestionText(questionnaires$questionNumber[q])
  questionnaires$questionTextShort[q] <- getQuestionText(questionnaires$questionNumber[q], T)
}

# Were the advisors perceived differently to begin with?
questionnaireTests <- list()
for(qNum in 1:length(unique(questionnaires$questionNumber))) {
  q <- unique(questionnaires$questionNumber)[qNum]
  q.text <- getQuestionText(q)
  Qs <- questionnaires[which(questionnaires$questionNumber==q),]
  trust.test.t1 <- t.test(Qs[Qs$timePoint==1 & Qs$adviceType==adviceTypes$AiC,"answer"],
                          Qs[Qs$timePoint==1 & Qs$adviceType==adviceTypes$AiU,"answer"], 
                          paired=T)
  trust.test.t1.d <- cohensD(Qs[Qs$timePoint==1 & Qs$adviceType==adviceTypes$AiC,"answer"],
                             Qs[Qs$timePoint==1 & Qs$adviceType==adviceTypes$AiU,"answer"])
  print(paste0('>>Question: ', q.text))
  print('  Paired t-test for the BEGINNING of the experiment')
  prettyPrint(trust.test.t1, trust.test.t1.d)
  print('  bayesian examination of above')
  trust.test.t1.b <- ttestBF(Qs[Qs$timePoint==1 & Qs$adviceType==adviceTypes$AiC,"answer"],
                             Qs[Qs$timePoint==1 & Qs$adviceType==adviceTypes$AiU,"answer"], 
                             paired=T)
  #print(trust.test.t1.b)
  print(paste0('  Evidence strength for higher AiC answer: BF=', round(exp(trust.test.t1.b@bayesFactor$bf),3)))
  print('  Mean(AiC start)')
  printMean(Qs[which(Qs$timePoint==1 & Qs$adviceType==adviceTypes$AiC),"answer"])
  print('  Mean(AiU start)')
  printMean(Qs[which(Qs$timePoint==1 & Qs$adviceType==adviceTypes$AiU),"answer"])
  
  # Were they perceived differently from one another at the end?
  t <- lastTimePoint # find last time point
  trust.test.tLast <- t.test(Qs[Qs$timePoint==t & Qs$adviceType==adviceTypes$AiC,"answer"],
                             Qs[Qs$timePoint==t & Qs$adviceType==adviceTypes$AiU,"answer"], 
                             paired=T)
  trust.test.tLast.d <- cohensD(Qs[Qs$timePoint==t & Qs$adviceType==adviceTypes$AiC,"answer"],
                                Qs[Qs$timePoint==t & Qs$adviceType==adviceTypes$AiU,"answer"])
  print('  Paired t-test for the END of the experiment')
  prettyPrint(trust.test.tLast, trust.test.tLast.d)
  print('  bayesian examination of above')
  trust.test.tLast.b <- ttestBF(Qs[Qs$timePoint==t & Qs$adviceType==adviceTypes$AiC,"answer"],
                                Qs[Qs$timePoint==t & Qs$adviceType==adviceTypes$AiU,"answer"], 
                                paired=T)
  #print(trust.test.tLast.b)
  print(paste0('  Evidence strength for higher AiC answer: BF=', round(exp(trust.test.tLast.b@bayesFactor$bf),3)))
  print('  Mean(AiC end)')
  printMean(Qs[which(Qs$timePoint==t & Qs$adviceType==adviceTypes$AiC),"answer"])
  print('  Mean(AiU end)')
  printMean(Qs[which(Qs$timePoint==t & Qs$adviceType==adviceTypes$AiU),"answer"])
  questionnaireTests[[qNum]] <- list()
  questionnaireTests[[qNum]]$t1 <- trust.test.t1
  questionnaireTests[[qNum]]$tLast <- trust.test.tLast
  questionnaireTests[[qNum]]$text <- q.text
}
graph.questionnaire <- ggplot(questionnaires, aes(x=timePoint, y=answer, color = factor(adviceType))) +
  stat_summary(geom = "errorbar",
               fun.data = "mean_cl_boot",
               width = 0.2) +
  stat_summary(geom = "point",
               fun.y = "mean",
               shape = 23, size = 5) +
  stat_summary(aes(group = adviceType), fun.y=mean, geom="line") +
  scale_color_discrete(name = 'Advisor', labels=c('Agree-in-confidence', 'Agree-in-uncertainty')) +
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  theme_light() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(~factor(questionTextShort)) +
  labs(title = "Questionnaire responses",
       subtitle = paste(strwrap(paste("Subjective assessment of the advisors over the course of the experiment.",
                                      "Mean values across participants are indicated by open diamonds,",
                                      "with error bars showing 95% confidence intervals. Each panel shows the",
                                      "answers concerning a different attribute of the advisor.",
                                      sep = " "), 
                                width=115), collapse = "\n"),
       legend = NULL,
       x = 'Timepoint',
       y = "Advisor rating") 
graph.questionnaire  
ggsave(paste0(figPath, "questionnaires.png"), plot = graph.questionnaire)

# We should look at whether there's a subjective preference for the most picked
# advisor. Should also look at whether that advisor is more influential. But
# those are not preregistered, so look at them later


## 5) Do participants simply prefer agreement? ####################################################
print('## 5) Do participants simply prefer agreement? ###################################')

# If so, we should see that participants preferentially pick agree-in-confidence
# advisor when their initial confidence is high, and agreee-in-uncertainty when
# their initial confidence is low. We can t-test aic pick proportion in
# high-confidence vs aic pick proportion in low-confidence.

ttest.aic.byConf <- t.test(participants$aicPickRate.lowConf,
                           participants$aicPickRate.highConf,
                           paired=T) # do selection proportions differ by initial confidence?
ttest.aic.byConf.d <- cohensD(participants$aicPickRate.lowConf,
                              participants$aicPickRate.highConf)
print('>>(aic.byConf.test) choice proportion Agree-in-confidence in low vs. high inital confidence')
prettyPrint(ttest.aic.byConf, ttest.aic.byConf.d)
print('>>(aic.byConf.test.b) bayesian examination of above (prior = mean of 0.5, sd as empirically observed)')
ttest.aic.byConf.b <- ttestBF(participants$aicPickRate.lowConf,
                              participants$aicPickRate.highConf,
                              paired = T)
print(ttest.aic.byConf.b)
print(paste0('Evidence strength for differential AiC picking: BF=', round(exp(ttest.aic.byConf.b@bayesFactor$bf),3)))
print('Low-confidence:')
printMean(participants$aicPickRate.lowConf)
print('High-confidence:')
printMean(participants$aicPickRate.highConf)


# --NON-Preregistered stuff----------------------------------------------------------------------------------
print('--NON Preregistered stuff-----------------------------------------------------------------------------')

## 6) Initial agreement effect

# Perhaps the decision as to the better advisor is fairly intractable once made,
# and it's made early? 

# We can also use agreement rate differentials rather than simply AiC agreement
# count. This was not done originally because the rates weren't calculated by
# block. We also want to use the pick rate on the subsequent blocks rather than
# all blocks as the outcome, so that needs calculating.
for(b in unique(trials$block))
  participants.byBlock[,paste0('agreeRateDifference.block',b)] <- 
    participants.byBlock[,paste0('aicAgreeRate.block',b)] - participants.byBlock[,paste0('aiuAgreeRate.block',b)]
for(p in 1:dim(participants.byBlock)[1])
  participants.byBlock$aicPickRate.notBlock3[p] <- length(which(trials$participantId==
                                                                  participants.byBlock$participantId[p]
                                                             & trials$hasChoice
                                                             & trials$adviceType==adviceTypes$AiC
                                                             & trials$block!=3)) / 
  length(which(trials$participantId==participants.byBlock$participantId[p]
               & trials$hasChoice & trials$block!=3))
equation <- lm(aicPickRate.notBlock3 ~ agreeRateDifference.block3, data = participants.byBlock)
equationText <- lmToStr(equation, c('x'), roundTo = 2)
summary(equation)
graph.block1Agreement.diff <- ggplot(participants.byBlock, 
                                         aes(x=agreeRateDifference.block3, y=aicPickRate.notBlock3)) +
  geom_point() +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_x_continuous(limits = c(-0.5,0.5), expand = c(0,0)) +
  geom_smooth(method='lm', formula = y~x, fullrange = T, level = .99, 
              linetype="dashed", color="black", fill="#CCCCCC") +
  geom_text(label = equationText, x = 0.35, y = .45) +
  theme_light() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = '#CCCCCC', size = 0.5)) +
  labs(title = "High weighting of early experience of advisors",
       subtitle = paste(strwrap(paste("Relationship between the initial agreement rate of the agree-in-confidence",
                                      "advisor relative to the agree-in-uncertainty advisor, and the preference for", 
                                      "picking that advisor.",
                                      "Dashed line shows the best-fit regression line with shaded 99% confidence ",
                                      "intervals.", 
                                      sep = " "), 
                                width=115), collapse = "\n"),
       legend = NULL,
       x = "Block 3 agree-in-confidence agreement rate difference",
       y = "Agree-in-confidence pick proportion (excluding block 3)") 
graph.block1Agreement.diff
ggsave(paste0(figPath, "block1AgreementRateDifference.png"), plot = graph.block1Agreement.diff)

equation <- lm(aicPickRate.notBlock3 ~ agreeRateDifference.block3 * aicPickRate.block3, data = participants.byBlock)
summary(equation)

# Strange interaction result - time to plot it out so we can see what's going
# on: x-axis can be Agree-in-confidence agree rate, and we can plot block 3 pick
# rate in different colours. We'll need to make those variables binary using a
# mean split first.
tmp <- participants.byBlock[,c('participantId', 
                               'aicPickRate.notBlock3', 
                               'aicPickRate.block3',
                               'agreeRateDifference.block3')]
tmp$aicPickRate.block3.binary <- as.factor(tmp$aicPickRate.block3 > mean(tmp$aicPickRate.block3))
levels(tmp$aicPickRate.block3.binary) <- c('Agree-in-uncertainty preferred', 'Agree-in-confidence preferred')
tmp$agreeRateDifference.binary <- as.factor(tmp$agreeRateDifference.block3 > mean(tmp$agreeRateDifference.block3))
levels(tmp$agreeRateDifference.binary) <- c('Agree-in-uncertainty agrees more', 
                                            'Agree-in-confidence agrees more')
graph.block1Agreement.interact <- ggplot(tmp, aes(x = agreeRateDifference.binary,
                                                  y = aicPickRate.notBlock3,
                                                  color = aicPickRate.block3.binary)) +
  geom_point() +
  stat_summary(aes(group = aicPickRate.block3.binary), fun.y=mean, geom="line") + 
  stat_summary(geom = "errorbar",
               fun.data = "mean_cl_boot",
               width = 0.2) +
  stat_summary(geom = "point",
               fun.y = "mean",
               shape = 23, size = 5) +
  scale_color_discrete(name = 'Pick proportion (block 3)') +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "Advisor weight interactions",
       subtitle = paste(strwrap(paste("Marginal means for Agree-in-confidence advisor pick rate overall 
                                      (excluding block 3) given for initial preference and relative 
                                      advisor agreement.", 
                                      sep = " "), 
                                width=115), collapse = "\n"),
       legend = NULL,
       x = "Advisor agreement in block 3",
       y = "Agree-in-confidence pick proportion (excluding block 3)")
graph.block1Agreement.interact
ggsave(paste0(figPath, 'block1Interaction.png'), plot = graph.block1Agreement.interact)

# We should also analyse the relative weight of the agreement difference at each
# confidence level
participants.byBlock$agreeRateDifference.highConf.block3 <- 
  participants.byBlock$aicAgreeRate.highConf.block3 - participants.byBlock$aiuAgreeRate.highConf.block3
participants.byBlock$agreeRateDifference.medConf.block3 <- 
  participants.byBlock$aicAgreeRate.medConf.block3 - participants.byBlock$aiuAgreeRate.medConf.block3
participants.byBlock$agreeRateDifference.lowConf.block3 <- 
  participants.byBlock$aicAgreeRate.lowConf.block3 - participants.byBlock$aiuAgreeRate.lowConf.block3
participants.byBlock$agreeRateDifference.highConf.block3[which(is.nan(participants.byBlock$agreeRateDifference.highConf.block3))] <- 0
participants.byBlock$agreeRateDifference.medConf.block3[which(is.nan(participants.byBlock$agreeRateDifference.medConf.block3))] <- 0
participants.byBlock$agreeRateDifference.lowConf.block3[which(is.nan(participants.byBlock$agreeRateDifference.lowConf.block3))] <- 0
# Looking at the low/mid/high conf trials separately
equation <- lm(aicPickRate.notBlock3 ~ agreeRateDifference.highConf.block3 + 
                 agreeRateDifference.medConf.block3 + 
                 agreeRateDifference.lowConf.block3, 
               data = participants.byBlock)
equationText <- lmToStr(equation, c('high', 'med', 'low'), roundTo = 2)
summary(equation)

# Does predictive power of agreement count drop off over time?
agreementCoefs <- data.frame(block=integer(), coef=double(), coef.se = double())
for(b in unique(trials$block)) {
  eq <- as.formula(paste0('aicPickRate ~ agreeRateDifference.block',b))
  mdl <- summary(lm(eq, data = participants.byBlock))    # we can rip the necessary stats from the summary
  agreementCoefs <- rbind(agreementCoefs,
                          data.frame(block = b,
                                     coef=mdl$coef[2,1],
                                     se=mdl$coef[2,2]))
}
equation <- lm(coef ~ block, data = agreementCoefs)
equationText <- lmToStr(equation, c('x'), 3)
summary(equation)
agreementCoefs$block <- factor(agreementCoefs$block)
graph.agreementPickRate <- ggplot(agreementCoefs, aes(x = block, y = coef)) +
  geom_errorbar(aes(x = block, ymin = coef-se, ymax = coef+se), size = 0.01) +
  geom_point(shape = 23, fill = 'white', color = 'black', size = 5) +
  geom_smooth(method = 'lm', aes(x = as.numeric(block)), alpha = 0.2) +
  annotate(geom = 'text', x = 9, y = 0.45, label = equationText) +
  theme_light() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank()) +
  labs(title = "Agreement-Pick rate correlation coefficients by block",
       subtitle = paste(strwrap(paste("Diamonds give the correlation coefficient for extent to which the ",
                                      "agree-in-confidence advisor agrees with the judge more within a block,",
                                      "and the extent to judge's overall preference for choosing the",
                                      "agree-in-confidence advisor over the agree-in-uncertainty advisor.",
                                      "Error bars indicate +/- 1 standard error.",
                                      "The solid line gives the best fit, and the shaded area the 95%",
                                      "confidence intervals for the fit line.",
                                      sep = " "), 
                                width=115), collapse = "\n"),
       legend = NULL,
       x = "Block",
       y = "Agreement trials - Pick proportion correlation beta") 
graph.agreementPickRate
ggsave(paste0(figPath, "agreementPickRate-byBlock.png"), plot = graph.agreementPickRate)

## 7) Subjective assessment of preferred advisor
# Correlate pickrate for favourite advisor against questionnaire scores
lastTimePoint <- max(questionnaires$timePoint)
tmp <- participants[,c('participantId', 'aicPickRate')]
tmp$pickRateBias <- abs(0.5-tmp$aicPickRate)
tmp$favouriteAdvisor <- ifelse(tmp$aicPickRate>0.5, adviceTypes$AiC, adviceTypes$AiU)
questionnaires$pickRateBias <- sapply(questionnaires$participantId, function(y) {
  tmp$pickRateBias[which(tmp$participantId==y)]
})
questionnaires$aicPickRate <- sapply(questionnaires$participantId, function(y) {
  as.numeric(tmp$aicPickRate[which(tmp$participantId==y)])
})
questionnaires$favouriteAdvisor <- sapply(questionnaires$participantId, function(y) {
  as.numeric(tmp$favouriteAdvisor[which(tmp$participantId==y)])
})

graph.Q.aicPickRate <- ggplot(questionnaires[which(questionnaires$adviceType==adviceTypes$AiC),], 
                             aes(x = aicPickRate, y = answer)) +
  geom_point() + 
  geom_smooth(method = 'lm') +
  facet_grid(timePoint~questionTextShort)
graph.Q.aicPickRate
ggsave(paste0(figPath, "pickRate-Questionnaire.png"), plot = graph.Q.aicPickRate)
# These look similar enough that we can just use the last timepoint (4)
coefs <- vector('list', length = length(unique(questionnaires$questionTextShort)))
for(q in 1:length(coefs)) {
  qTxt <- unique(questionnaires$questionTextShort)[q]
  coefs[[q]] <- lm(answer ~ aicPickRate, 
                   data = questionnaires[which(questionnaires$questionTextShort==qTxt 
                                               & questionnaires$timePoint==4
                                               & questionnaires$adviceType==adviceTypes$AiC),])
  print(qTxt)
  print(summary(coefs[[q]]))
}
names(coefs) <- unique(questionnaires$questionTextShort)
questionnaires$equation <- sapply(questionnaires$questionTextShort, function(x){lmToStr(coefs[[x]],c('x'),2)})

graph.Q.aicPickRate.combine <- ggplot(questionnaires[which(questionnaires$timePoint==4
                                                           & questionnaires$adviceType==adviceTypes$AiC),], 
                                      aes(x = aicPickRate, y = answer)) +
  geom_point() + 
  geom_smooth(method = 'lm') + 
  geom_text(aes(label = equation, x = 0.3, y = 95)) +
  facet_wrap(~questionTextShort) +
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "Behavioural and self-report consistency",
       subtitle = paste(strwrap(paste("Scatter plot of questionnaire response for the agree-in-confidence",
                                      "advisor versus the overall pick rate for that advisor. Different panels",
                                      "pertain to the different questions asked in the questionnaire. Lines give",
                                      "the best-fit linear model given by the formula, with shaded areas indicating",
                                      "95% confidence intervals.",
                                      sep = " "), 
                                width=115), collapse = "\n"),
       legend = NULL,
       x = 'Overall pick proportions for agree-in-confidence advisor',
       y = "Questionnaire response") 
graph.Q.aicPickRate.combine
ggsave(paste0(figPath, "pickRate-Answer.png"), plot = graph.Q.aicPickRate.combine)

equation <- lm(answer ~ aicPickRate, data = questionnaires[which(questionnaires$timePoint==4
                                                                 & questionnaires$adviceType==adviceTypes$AiC),])
summary(equation)
ggplot(questionnaires[which(questionnaires$adviceType==adviceTypes$AiC & questionnaires$timePoint==4),],
       aes(x=aicPickRate, y=answer)) + geom_point()

# So the more often they pick the agree-in-confidence advisor, the higher they
# rate that advisor. But picking is zero-sum, so what about the difference in
# ratings between the advisors?
tmp <- questionnaires[which(questionnaires$adviceType==adviceTypes$AiC),]
for(i in 1:dim(tmp)[1]) {
  tmp$difference[i] <- tmp$answer[i] - questionnaires[which(questionnaires$timePoint==tmp$timePoint[i]
                                                            & questionnaires$questionNumber==tmp$questionNumber[i]
                                                            & questionnaires$adviceType==adviceTypes$AiU
                                                            & questionnaires$participantId==tmp$participantId[i]),
                                                      "answer"]
}
# recalculate coefficients
coefs <- vector('list', length = length(unique(tmp$questionTextShort)))
for(q in 1:length(coefs)) {
  qTxt <- unique(tmp$questionTextShort)[q]
  coefs[[q]] <- lm(difference ~ aicPickRate, 
                   data = tmp[which(tmp$questionTextShort==qTxt 
                                    & tmp$timePoint==4
                                    & tmp$adviceType==adviceTypes$AiC),])
  print(qTxt)
  print(summary(coefs[[q]]))
}
names(coefs) <- unique(tmp$questionTextShort)
tmp$equation <- sapply(tmp$questionTextShort, function(x){lmToStr(coefs[[x]],c('x'),2)})

graph.Q.aicPickRate.difference <- ggplot(tmp[which(tmp$timePoint==4),],
                                         aes(x = aicPickRate, y = difference)) +
  geom_point() + 
  geom_smooth(method = 'lm') + 
  geom_text(aes(label = equation, x = 0.3, y = 90)) +
  facet_wrap(~questionTextShort) +
  scale_y_continuous(limits = c(-100,100), expand = c(0,0)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "Behavioural and self-report consistency",
       subtitle = paste(strwrap(paste("Scatter plot of questionnaire response difference between the advisors.",
                                      "Positive values indicate higher scores for the agree-in-confidence",
                                      "advisor than the agree-in-uncertainty advisor. The horizontal axis gives",
                                      "the overall pick rate for the agree-in-confidence advisor. Different panels",
                                      "pertain to the different questions asked in the questionnaire. Lines give",
                                      "the best-fit linear model given by the formula, with shaded areas indicating",
                                      "95% confidence intervals.",
                                      sep = " "), 
                                width=115), collapse = "\n"),
       legend = NULL,
       x = 'Overall pick proportions for agree-in-confidence advisor',
       y = "Questionnaire response difference") 
graph.Q.aicPickRate.difference
ggsave(paste0(figPath, "pickRate-AnswerDifference.png"), plot = graph.Q.aicPickRate.difference)

## 8) Agreement effect under capped influence ###############################################################

print('## 8) ANOVA investigating capped influence ##############################################')

# The asymmetry in the scale (arising from the fact the initial decision is
# never on the midpoint so that there is always more potential for shifting away
# from the inital answer than shifting more in that direction) means that the
# effect of agreement found earlier (disagree>agree) is trivial. We calculated a
# capped measure of influence earlier, so we can use that measure to rerun the
# ANOVA and test whether the agreement effect is an artefact of the scale.

# First some descriptives about how much the capping process changes the data
influenceCapEffects <- data.frame(participantId = integer(), cappedTrials = integer())
for(p in 1:dim(participants)[1]) {
  participantId <- participants$participantId[p]
  cappedTrials <- length(which(trials$influence!=trials$cappedInfluence
                               & trials$participantId==participantId))
  influenceCapEffects <- rbind(influenceCapEffects, data.frame(participantId,
                                                               cappedTrials))
}
summary(influenceCapEffects)
hist(influenceCapEffects$cappedTrials, breaks = 20)
print(paste(sum(influenceCapEffects$cappedTrials), 'of', length(which(!is.na(trials$influence))),
      '[', round(sum(influenceCapEffects$cappedTrials)/length(which(!is.na(trials$influence)))*100,2),'%]', 
      'trials capped.'))

# 2x2x2 ANOVA investigating effects of advisor type
# (agree-in-confidence/uncertainty), choice (un/forced), and agreement
# (dis/agree) on influence. These are all within-subjects manipulations (meaning
# we include an error term of participantID and its child effects).
anova.influence.capped <- ezANOVA(data = participants.influence.capped,
                                  dv = value, 
                                  wid = participantId,
                                  within = c('AiC', 'agree', 'hasChoice'),
                                  return_aov = T)
print('>>(anova.influence.capped)')
anova.influence.capped$ANOVA
print('Advisor type = AiC')
printMean(participants.influence.capped$value[which(participants.influence.capped$AiC==T)])
print('Advisor type = AiU')
printMean(participants.influence.capped$value[which(participants.influence.capped$AiC==F)])
print('Agree')
printMean(participants.influence.capped$value[which(participants.influence.capped$agree==T)])
print('Disagree')
printMean(participants.influence.capped$value[which(participants.influence.capped$agree==F)])
print('Choice')
printMean(participants.influence.capped$value[which(participants.influence.capped$hasChoice==T)])
print('Forced')
printMean(participants.influence.capped$value[which(participants.influence.capped$hasChoice==F)])
tmp <- participants.influence.capped
levels(tmp$AiC) <- c('Agree-in-uncertainty', 'Agree-in-confidence') 
levels(tmp$agree) <- c('Disagree', 'Agree')
levels(tmp$hasChoice) <- c('Forced trials', 'Choice trials')
graph.anova.influence.capped <- ggplot(tmp, aes(agree, value, color = AiC, fill = AiC)) +
  geom_point(position = position_jitter(w=0.05, h=0)) +
  stat_summary(geom = "errorbar",
               fun.data = "mean_cl_boot",
               width = 0.2) +
  stat_summary(geom = "point",
               fun.y = "mean",
               shape = 23, size = 5) +
  stat_summary(aes(group = AiC), fun.y=mean, geom="line") + 
  facet_grid(.~hasChoice) +
  scale_color_discrete(name = 'Advisor type') +
  scale_fill_discrete(name = 'Advisor type') +
  theme_light() +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Advice Influence",
       subtitle = paste(strwrap(paste("Influence of advice under varied conditions. Points indicate",
                                      "mean values for a participant, while diamonds indicate the mean",
                                      "of participant means, with error bars specifying 95% confidence intervals.",
                                      sep = " "), 
                                width=115), collapse = "\n"),
       legend = NULL,
       x = 'Agreement between advisor and judge',
       y = "Influence of the advice") 
graph.anova.influence.capped
ggsave(paste0(figPath, "cappedInfluence.png"), plot = graph.anova.influence.capped)

## 9) Descriptives
print('## 9) Descriptives ###############################################################')

# Means etc.
# Proportion correct
print('Proportion Correct')
printMean(participants$proportionCorrect)
print('Proportion Correct AiC')
printMean(participants$aicProportionCorrect)
print('Proportion Correct AiU')
printMean(participants$aiuProportionCorrect)

print('Proportion Correct Final')
printMean(participants$proportionCorrectFinal)
print('Proportion Correct Final AiC')
printMean(participants$aicProportionCorrectFinal)
print('Proportion Correct Final AiU')
printMean(participants$aiuProportionCorrectFinal)

# Agreement rates
print('AiC agreement (all trials)')
printMean(participants$aicAgreeRate)
print('AiU agreement (all trials)')
printMean(participants$aiuAgreeRate)

print('AiC agreement (correct trials)')
printMean(participants$aicCorrectAgreeRate)
print('AiU agreement (correct trials)')
printMean(participants$aiuCorrectAgreeRate)

print('AiC agreement (incorrect trials)')
printMean(participants$aicIncorrectAgreeRate)
print('AiU agreement (incorrect trials)')
printMean(participants$aiuIncorrectAgreeRate)

print('AiC agreement high confidence')
printMean(participants$aicAgreeRate.highConf)
print('AiU agreement high confidence')
printMean(participants$aiuAgreeRate.highConf)
print('AiC agreement medium confidence')
printMean(participants$aicAgreeRate.medConf)
print('AiU agreement medium confidence')
printMean(participants$aiuAgreeRate.medConf)
print('AiC agreement low confidence')
printMean(participants$aicAgreeRate.lowConf)
print('AiU agreement low confidence')
printMean(participants$aiuAgreeRate.lowConf)

# Participant confidence
# Calculate the figures
for(p in 1:dim(participants)[1]) {
  set <- trials[which(trials$participantId==participants$participantId[p]),]
  participants$conf[p] <- mean(abs(set$cj1))
  participants$confFinal[p] <- mean(abs(set$cj2), na.rm = T)
  participants$correctConf[p] <- mean(abs(set$cj1[which(set$cor1==1)]))
  participants$correctConfFinal[p] <- mean(abs(set$cj2[which(set$cor2==1)]), na.rm = T)
  participants$incorrectConf[p] <- mean(abs(set$cj1[which(set$cor1==0)]))
  participants$incorrectConfFinal[p] <- mean(abs(set$cj2[which(set$cor2==0)]), na.rm = T)
  participants$agreeConf[p] <- mean(abs(set$cj2[which(set$agree==1)]))
  participants$disagreeConf[p] <- mean(abs(set$cj2[which(set$agree==0)]))
  participants$aicAgreeConf[p] <- mean(abs(set$cj2[which(set$agree==1 & set$adviceType==adviceTypes$AiC)]))
  participants$aiuAgreeConf[p] <- mean(abs(set$cj2[which(set$agree==1 & set$adviceType==adviceTypes$AiU)]))
  participants$aicDisagreeConf[p] <- mean(abs(set$cj2[which(set$agree==0 & set$adviceType==adviceTypes$AiC)]))
  participants$aiuDisagreeConf[p] <- mean(abs(set$cj2[which(set$agree==0 & set$adviceType==adviceTypes$AiU)]))
}
print('Mean confidence')
printMean(participants$conf)
print('Mean final confidence')
printMean(participants$confFinal)
# Mean initial confidence on correct trials
print('Mean confidence on correct trials')
printMean(participants$correctConf)
# Mean initial confidence on incorrect trials
print('Mean confidence on incorrect trials')
printMean(participants$incorrectConf)
# Mean final confidence on correct trials
print('Mean final confidence on correct trials')
printMean(participants$correctConfFinal)
# Mean final confidence on incorrect trials
print('Mean final confidence on incorrect trials')
printMean(participants$incorrectConfFinal)

print('Mean confidence on agreement trials')
printMean(participants$agreeConf)
print('Mean confidence on disagreement trials')
printMean(participants$disagreeConf)
print('Mean confidence on AiC agreement trials')
printMean(participants$aicAgreeConf)
print('Mean confidence on AiU agreement trials')
printMean(participants$aiuAgreeConf)
print('Mean confidence on AiC disagreement trials')
printMean(participants$aicDisagreeConf)
print('Mean confidence on AiU disagreement trials')
printMean(participants$aiuDisagreeConf)

## 10) Improvement with practice?
print('## 10) Improvement with practice? ################################################')

# Do participants become more confident in their initial answers on average over
# time? Specifically, is block 3 confidence lower than average confidence in the
# remaining blocks?
for(p in 1:dim(participants.byBlock)[1]) {
  set <- trials[which(trials$participantId==participants.byBlock$participantId[p]),]
  participants.byBlock$confidence.block3[p] <- mean(set$cj1[which(set$block==3)])
  participants.byBlock$confidence.notBlock3[p] <- mean(set$cj1[which(set$block>3)])
}

t.test(participants.byBlock$confidence.block3, participants.byBlock$confidence.notBlock3, paired = T)
ttestBF(participants.byBlock$confidence.block3, participants.byBlock$confidence.notBlock3, paired = T)


## 11) Initial and final accuracy
print('## 11) Initial and final accuracy ################################################')

# reformat the data so we have a nice comparison before vs after
participants.accuracy <- melt(participants[,c("participantId", "proportionCorrect", "proportionCorrectFinal")],
                              id.vars = 'participantId')
levels(participants.accuracy$variable) <- c('initial', 'final')
graph.accuracy <- ggplot(participants.accuracy, aes(variable, value)) +
  # draw the density violin
  geom_violin(colour = NA, fill = "#DDDDDD", alpha = .5) +
  # reference line for staircasing value
  geom_hline(yintercept = 0.71, linetype = 'dashed', size = 1) +
  # link participant observations with a line
  geom_line(aes(group = participantId), alpha = .2) +
  # 95% bootstrapped confidence interval error bars
  stat_summary(geom = "errorbar",
               fun.data = "mean_cl_boot",
               width = 0.1) +
  # mean diamonds
  stat_summary(geom = "point",
               fun.y = "mean",
               shape = 23, fill = "white", size = 4) +
  # individual data points
  geom_point(alpha = .2, shape = 16) +
  # rescale y axis and remove padding
  scale_y_continuous(limits = c(0.65, 0.75), expand = c(0,0)) + 
  # Nice x labels
  scale_x_discrete(labels=c('Initial decision', 'Final decision')) +
  # clean background
  theme_light() +
  # remove vertical gridlines
  theme(panel.grid.major.x = element_blank()) +
  # labels
  labs(title = "Decision accuracy details",
       subtitle = paste(strwrap(paste("Accuracy of participants' initial and final decisions.",
                                      "Points connected by faint lines indicate mean values for a single participant.",
                                      "Diamonds mark the mean of all participant means, with error bars indicating",
                                      "bootstrapped 95% confidence intervals.",
                                      "Shaded areas indicate density.",
                                      "Dashed line represents staircase target; chance performance is 0.5.", 
                                      sep = " "), 
                                width=115), collapse = "\n"),
       legend = NULL,
       x = NULL,
       y = "Proportion correct") 
graph.accuracy
ggsave(paste0(figPath, "decision accuracy detail.png"), plot = graph.accuracy)

graph.accuracy <- ggplot(participants.accuracy, aes(variable, value)) +
  # geom_jitter(position=position_jitter(width=.1, height=0), alpha = .2) +
  geom_violin(colour = NA, fill = "#DDDDDD", alpha = .5) +
  geom_hline(yintercept = 0.71, linetype = 'dashed', size = 1) +
  # geom_line(aes(group = participantId), alpha = .2) +
  # geom_point(colour = "black", shape = 16) +
  stat_summary(geom = "errorbar",
               fun.data = "mean_cl_boot",
               width = 0.1) +
  stat_summary(geom = "point",
               fun.y = "mean",
               shape = 23, fill = "white", size = 4) +
  geom_point(alpha = .2, shape = 16) +
  scale_y_continuous(limits = c(0.5, 1), expand = c(0,0)) + 
  scale_x_discrete(labels=c('Initial decision', 'Final decision')) +
  theme_light() +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Decision accuracy details",
       subtitle = paste(strwrap(paste("Accuracy of participants' initial and final decisions.",
                                      "Points indicate mean values for a single participant.",
                                      "Diamonds mark the mean of all participant means, with error bars indicating",
                                      "bootstrapped 95% confidence intervals.",
                                      "Shaded areas indicate density.",
                                      "Dashed line represents staircase target; chance performance is 0.5.", 
                                      sep = " "), 
                                width=115), collapse = "\n"),
       legend = NULL,
       x = NULL,
       y = "Proportion correct") 
graph.accuracy
ggsave(paste0(figPath, "decision accuracy.png"), plot = graph.accuracy)

## 12) Histogram of influence
print('## 12) Histogram of influence ####################################################')

# histograms of advisor influence by participant
graph.influence.byParticipant <- ggplot(trials[which(is.finite(trials$influence)),], aes(x=influence)) +
  geom_histogram(bins = 30, alpha = .75, aes(color = "Both", fill = "Both")) +
  geom_histogram(bins = 30, 
                 data = trials[which(trials$advisorId==adviceTypes$AiC),], 
                 alpha = 0.3, 
                 aes(color = "Agree in Confidence",
                     fill = "Agree in Confidence")) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.spacing = unit(0.75, "lines"), # pad the panels slightly
        legend.position = c(0.91, 0.07)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +                        
  scale_fill_manual(name = 'Advisor', values = c('red', 'blue')) +
  scale_color_manual(name = 'Advisor', values = c('red', 'blue')) +
  geom_vline(xintercept = 0, linetype="dashed", color = "black") +
  facet_wrap(~participantId) +
  labs(title = "Advisor influence by participant",
       subtitle = paste(strwrap(paste("Influence of the advisors on the participants' responses.",
                                      "Each graph shows the influence distribution for a single participant.",
                                      "The blue histogram shows the distribution of influence for both advisors combined,",
                                      "while the red histogram shows the distribution of the influence for the agree-in-confidence",
                                      "advisor alone.",
                                      "The blue areas uncovered by red shading indicate the frequency of responses for the",
                                      "agree-in-uncertainty advisor.",
                                      "The dashed line marks 0 influence: the participant's intial answer was unchanged by advice.",
                                      sep = " "), 
                                width=140), collapse = "\n"),
       x = "Influence (binned)",
       y = "Trial count") 
graph.influence.byParticipant
ggsave(paste0(figPath, "influence by participant.png"), 
       plot = graph.influence.byParticipant, 
       width = 9.05, height = 5.98)

## 13) Initial confidence and influence
print('## 13) Initial confidence and influence ##########################################')

# Relationship between initial confidence and influence
equation <- lm(abs(cj1) ~ influence, trials[which(is.finite(trials$influence)),])
equation.text <- paste0('y = ', round(coef(equation)[[1]],2), ' + ', round(coef(equation)[[2]],2), 'x')
graph.influence.byConfidence <- ggplot(trials[which(is.finite(trials$influence)),], aes(x=abs(cj1), y=influence)) +
  geom_point(alpha = 0.1) +
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0)) + 
  geom_smooth(method='lm', formula = y~x, level = .99, linetype="dashed", color="black", fill="#CCCCCC") +
  geom_text(label = equation.text, x = 50, y = -15) +
  theme_light() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = '#CCCCCC', size = 0.5)) +
  labs(title = "Advice influence by confidence",
       subtitle = paste(strwrap(paste("Effect of initial decision confidence on the influence of advice.",
                                      "Points indicate data from a single trial. Points are semi-transparent, so darker",
                                      "points indicate multiple observations.",
                                      "Dashed line shows the best-fit regression line with shaded 99% confidence intervals.", 
                                      sep = " "), 
                                width=115), collapse = "\n"),
       legend = NULL,
       x = "Initial decision confidence",
       y = "Influence of advice") 
graph.influence.byConfidence
ggsave(paste0(figPath, "influence by confidence.png"), plot = graph.influence.byConfidence)

## 14) Picking by block
print('## 14) Picking by block ##########################################################')

# There were many experimental blocks. We can look at pick proportion as a function of block across participants
trials$block <- as.numeric(trials$block)
participants.picks <- data.frame(participantId=integer(),
                                 block=integer(),
                                 aic.pick.proportion=double())
for(p in 1:dim(participants)[1]) {
  set <- trials[which(trials$participantId==participants$participantId[p]
                & !is.nan(trials$advisorId)
                & trials$hasChoice),]
  for(b in 1:length(unique(set$block))) {
    block <- unique(set$block)[[b]]
    aic.pick.proportion <- length(which(set$block==block & set$advisorId==adviceTypes$AiC)) /
      length(which(set$block == block))
    participants.picks <- rbind(participants.picks, data.frame(participantId=participants$participantId[p],
                                                               block,
                                                               aic.pick.proportion))
  }
}
participants.picks$block <- factor(participants.picks$block)
participants.picks$participantId <- factor(participants.picks$participantId)

graph.pickRate.byBlock <- ggplot(participants.picks, aes(x = block, y = aic.pick.proportion)) +
  geom_violin(color = NA, fill='#EEEEEE') +
  geom_hline(linetype = "dashed", color = "black", yintercept = .5) +
  geom_point(alpha = 0.5) +
  geom_line(aes(group = participantId, color = participantId), alpha = 0.3, size=0.3) +
  stat_summary(geom = "errorbar",
               fun.data = "mean_cl_boot",
               width = 0.1) +
  stat_summary(geom = "point",
               fun.y = "mean",
               shape = 23, fill = "white", size = 4) +
  scale_y_continuous(limits = c(0.000,1.000)) +
  scale_x_discrete(label = unique(participants.picks$block)) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_color_manual(name="Participant", values = c('#A81B21','#EE1C25','#F47252','#007236',
                                                      '#45BB7E','#4E191F','#6F3D24','#CE9222',
                                                      '#FEDC01','#FEF783','#7C0A6A','#BD569F',
                                                      '#DB485B','#EAA39D','#FCD4CC','#054189',
                                                      '#005BAA','#669AD4','#F37020','#FBA51A',
                                                      '#252122','#59585D','#7F8386','#D0D4D7')) +
  labs(title = "Advisor preference over time",
       subtitle = paste(strwrap(paste("Proportion of the time each participant picked the agree-in-confidence advisor",
                                      "for each block.",
                                      "Points indicate data from a single participant, with darker points indicating superimposed",
                                      "observations from separate participants. Each participant has a different coloured line",
                                      "connecting their observations.",
                                      "The diamonds indicate the mean proportion across all participants.", 
                                      "Error bars give 95% bootstrapped confidence intervals.",
                                      "\n\nThe messiness of this graph indicates the absence of an clear pattern in advisor selection.",
                                      sep = " "), 
                                width=115), collapse = "\n"),
       legend = NULL,
       x = "Block",
       y = "Proportion of the time the agree-in-confidence advisor is chosen") 
graph.pickRate.byBlock
ggsave(paste0(figPath, "pick rate by block.png"), plot = graph.pickRate.byBlock)

# What if we calculate deviance from .5 (i.e. having a preference, agnostic to which way)?
# Absolute:
participants.picks$deviance <- abs(.5 - participants.picks$aic.pick.proportion)
for(p in 1:dim(participants)[1]) {
  participants$deviance[p] <- mean(
    participants.picks$deviance[which(participants.picks$participantId==participants$participantId[p])])
  participants$deviance.sd[p] <- sd(
    participants.picks$deviance[which(participants.picks$participantId==participants$participantId[p])])
}
  
graph.pickRate.deviance <- ggplot(participants, aes(x = "", y = deviance)) +
  geom_point(alpha = 0.5) +
  stat_summary(geom = "errorbar",
               fun.data = "mean_cl_boot",
               width = 0.1) +
  stat_summary(geom = "point",
               fun.y = "mean",
               shape = 23, fill = "white", size = 4) +
  scale_y_continuous(limits = c(0,0.5)) +
  scale_x_discrete(expand = c(0,0)) +
  theme_light() +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Choice deviance",
       subtitle = paste(strwrap(paste("Bias for one advisor over the other, irrespective of which advisor is preferred.",
                                      "Points indicate data from a single participant, while the diamond indicates the",
                                      "mean proportion across all participants.", 
                                      "Error bars give 95% bootstrapped confidence intervals.", 
                                      sep = " "), 
                                width=115), collapse = "\n"),
       legend = NULL,
       x = NULL,
       y = "Deviation from even selection rate") 
graph.pickRate.deviance
ggsave(paste0(figPath, "pick deviance.png"), plot = graph.pickRate.deviance)

# now look at deviance by block
graph.pickRate.deviance.byBlock <- ggplot(participants.picks, aes(x = block, y = deviance)) +
  geom_violin(color = NA, fill='#EEEEEE') +
  geom_hline(linetype = "dashed", color = "black", yintercept = .5) +
  geom_point(alpha = 0.5) +
  geom_line(aes(group = participantId, color = participantId), alpha = 0.3, size=0.3) +
  stat_summary(geom = "errorbar",
               fun.data = "mean_cl_boot",
               width = 0.1) +
  stat_summary(geom = "point",
               fun.y = "mean",
               shape = 23, fill = "white", size = 4) +
  scale_y_continuous(limits = c(0.000,0.5)) +
  scale_x_discrete(label = unique(participants.picks$block)) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_color_manual(name="Participant", values = c('#A81B21','#EE1C25','#F47252','#007236',
                                                    '#45BB7E','#4E191F','#6F3D24','#CE9222',
                                                    '#FEDC01','#FEF783','#7C0A6A','#BD569F',
                                                    '#DB485B','#EAA39D','#FCD4CC','#054189',
                                                    '#005BAA','#669AD4','#F37020','#FBA51A',
                                                    '#252122','#59585D','#7F8386','#D0D4D7')) +
  labs(title = "Choice deviance over time",
       subtitle = paste(strwrap(paste("Bias expressed for one advisor over the other (irrespective of which advisor is",
                                      "preferred) for each block.",
                                      "Points indicate data from a single participant, with darker points indicating superimposed",
                                      "observations from separate participants. Each participant has a different coloured line",
                                      "connecting their observations.",
                                      "The diamonds indicate the mean proportion across all participants.", 
                                      "Error bars give 95% bootstrapped confidence intervals.",
                                      "\nThe messiness of this graph indicates the absence of an clear pattern in advisor selection.",
                                      sep = " "), 
                                width=115), collapse = "\n"),
       legend = NULL,
       x = "Block",
       y = "Deviation from even selection rate") 
graph.pickRate.deviance.byBlock
ggsave(paste0(figPath, "pick deviance by block.png"), plot = graph.pickRate.deviance.byBlock)

# So it looks like participants tend to explore the advisors by modifying their picking bias quite widely, though the average
# bias for all participants is pretty even over time at around 0.2, or picking one advisor 40% more often than the other

# Perhaps participants have different exploration styles, such that some participants sample widely-but-wildly, picking 
# one advisor a great deal in some blocks, and picking one advisor much less frequently in others, while other 
# participants sample more cautiously-and-consistently, picking one advisor slightly more often than the other, but
# always keeping close to even whichever is preferred. 
# If that's true, there should be a correlation between mean deviance and standard deviation of deviance, such that
# the relationship should look like signal-dependant noise
equation <- lm(deviance.sd ~ deviance, data = participants)
equation.text <- paste0('y = ', round(coef(equation)[[1]],2), ' + ', round(coef(equation)[[2]],2), 'x')
graph.pickRate.deviance.style <- ggplot(participants, aes(x = deviance, y = deviance.sd)) +
  geom_point() + 
  geom_smooth(method='lm', formula = y~x, level = .95, linetype="dashed", color="black", fill="#CCCCCC") +
  geom_text(label = equation.text, x = 0.38, y = 0.14) +
  scale_y_continuous(limits = c(0,0.25), expand = c(0,0)) +
  scale_x_continuous(expand = c(0.01,0)) +
  theme_light() +
  theme(panel.grid = element_blank()) +
  scale_color_manual(name="Participant", values = c('#A81B21','#EE1C25','#F47252','#007236',
                                                    '#45BB7E','#4E191F','#6F3D24','#CE9222',
                                                    '#FEDC01','#FEF783','#7C0A6A','#BD569F',
                                                    '#DB485B','#EAA39D','#FCD4CC','#054189',
                                                    '#005BAA','#669AD4','#F37020','#FBA51A',
                                                    '#252122','#59585D','#7F8386','#D0D4D7')) +
  labs(title = "Choice deviance over time",
       subtitle = paste(strwrap(paste("Variability in choice deviance from even as a function of mean choice deviance.",
                                      "Participants with higher mean deviation also have a wider range of deviations,",
                                      "suggesting these participants explore more widely and more adventurously.",
                                      sep = " "), 
                                width=115), collapse = "\n"),
       legend = NULL,
       x = "Mean choice deviance",
       y = "Standard deviation of choice deviance") 
graph.pickRate.deviance.style
ggsave(paste0(figPath, "pick deviance mean x SD.png"), plot = graph.pickRate.deviance.style)

## 15) Marginal means for influence
print('## 15) Marginal means for influence ##############################################')

# Can look at some marginal means graphs supporting the influence ANOVA
tmp <- melt(participants, id.vars = c('participantId'), 
            measure.vars = c('aicInfluence', 'aiuInfluence'))
graph.influence.advisor <- ggplot(tmp, aes(x = variable, y = value)) +
  geom_violin(color = NA, fill = '#EEEEEE') +
  geom_point(alpha = 0.5, aes(color = as.factor(participantId))) +
  geom_line(aes(group = participantId, color = as.factor(participantId)), alpha = .5) +
  stat_summary(geom = "errorbar",
               fun.data = "mean_cl_boot",
               width = 0.1) +
  stat_summary(geom = "point",
               fun.y = "mean",
               shape = 23, fill = "white", size = 4) +
  scale_x_discrete(labels = c('Agree in confidence', 'Agree in uncertainty')) +
  scale_color_discrete(name = 'Participant') +
  theme_light() +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Advisor influence",
       subtitle = paste(strwrap(paste("Influence of the advisors by advice type.",
                                      "Points indicate data from a single participant, while the diamond indicates the",
                                      "mean proportion across all participants.", 
                                      "Error bars give 95% bootstrapped confidence intervals.", 
                                      sep = " "), 
                                width=115), collapse = "\n"),
       legend = NULL,
       x = NULL,
       y = "Influence") 
graph.influence.advisor
ggsave(paste0(figPath, "advisor influence.png"), plot = graph.influence.advisor)

# We can also compare forced trials to choice trials in the same way
tmp <- melt(participants, id.vars = c('participantId'),
            measure.vars = c('forcedInfluence', 'choiceInfluence'))
graph.influence.trialType <- ggplot(tmp, aes(x = variable, y = value)) +
  geom_violin(color = NA, fill = '#EEEEEE') +
  geom_point(alpha = 0.5, aes(color = as.factor(participantId))) +
  geom_line(aes(group = participantId, color = as.factor(participantId)), alpha = .5) +
  stat_summary(geom = "errorbar",
               fun.data = "mean_cl_boot",
               width = 0.1) +
  stat_summary(geom = "point",
               fun.y = "mean",
               shape = 23, fill = "white", size = 4) +
  scale_x_discrete(labels = c('Forced trials', 'Choice trials')) +
  scale_color_discrete(name = 'Participant') +
  theme_light() +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Advisor influence",
       subtitle = paste(strwrap(paste("Influence of the advisors by trial type.",
                                      "Points indicate data from a single participant, while the diamond indicates the",
                                      "mean proportion across all participants.", 
                                      "Error bars give 95% bootstrapped confidence intervals.", 
                                      sep = " "), 
                                width=115), collapse = "\n"),
       legend = NULL,
       x = NULL,
       y = "Influence") 
graph.influence.trialType
ggsave(paste0(figPath, "trial type influence.png"), plot = graph.influence.trialType)

# We can also inspect potential interactions by plotting forced and choice trials separately for the advisors
graph.influence.advisorXtrialType <- ggplot(participants.influence[complete.cases(participants.influence),],
                                            aes(x = AiC, y = value)) +
  geom_split_violin(aes(fill = hasChoice), color = NA, alpha = 0.2) +
  geom_point(alpha = 0.5, aes(fill = hasChoice, color = hasChoice)) +
  geom_line(data = participants.influence[which(participants.influence$hasChoice==T),],
            aes(group = participantId, color = hasChoice), alpha = .5) +
  geom_line(data = participants.influence[which(participants.influence$hasChoice==F),],
            aes(group = participantId, color = hasChoice), alpha = .5) +
  stat_summary(geom = "errorbar",
               fun.data = "mean_cl_boot",
               width = 0.1) +
  stat_summary(geom = "point",
               aes(fill = hasChoice),
               fun.y = "mean",
               shape = 23, size = 4) +
  scale_x_discrete(labels = c('Agree in confidence', 'Agree in uncertainty')) +
  scale_color_discrete(name = 'Forced') +
  scale_fill_discrete(name = 'Forced') +
  theme_light() +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Effect of trial type on advisor influence",
       subtitle = paste(strwrap(paste("Influence of the advisors by advice type for forced and choice trials.",
                                      "Points indicate data from a single participant, while the diamonds indicate the",
                                      "mean proportion across all participants for a given trial type.",
                                      "Error bars give 95% bootstrapped confidence intervals.",
                                      sep = " "),
                                width=115), collapse = "\n"),
       legend = NULL,
       x = NULL,
       y = "Influence")
graph.influence.advisorXtrialType
ggsave(paste0(figPath, "advisor influence by trialType.png"), plot = graph.influence.advisorXtrialType)

## 16) CJ1/CJ2 plots
print('## 16) CJ1/CJ2 plots #############################################################')

# Plot cj1 vs cj2 faceted by dis/agreement
df.poly1 <- data.frame(    # These polygon points define a parellelogram marking the limits for the capped influence
  x=c(-55, 0, 0),
  y=c(-55, -55, 55)
)
df.poly2 <- df.poly1 * -1
graph.confidence <- ggplot(trials[which(!is.nan(trials$agree)),], aes(x = cj1, y = cj2)) +
  geom_polygon(data = df.poly1, aes(x,y), fill = 'grey', alpha = 0.2) +
  geom_polygon(data = df.poly2, aes(x,y), fill = 'grey', alpha = 0.2) +
  geom_point(alpha = 0.2, aes(color = factor(cor2))) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1, color = 'black') +
  scale_color_discrete(name = 'Final judgement', labels = c('Incorrect', 'Correct')) +
  scale_x_continuous(limits = c(-55,55), expand = c(0,0)) +
  scale_y_continuous(limits = c(-55,55), expand = c(0,0)) +
  theme_light() +
  theme(panel.spacing = unit(2, 'lines'), legend.position = 'bottom') +
  coord_fixed() +
  facet_grid(~agree, labeller = as_labeller(c('0'='Disagree', '1'='Agree'))) +
  labs(title = "Initial vs final confidence",
       subtitle = paste(strwrap(paste("Influence of the advisors is evident in the deviation from the dashed y=x",
                                      "line. Points lying below the line indicate a",
                                      "more leftward response from initial to final judgement. Points above",
                                      "the line indicate a more rightward response in the final judgement.",
                                      "The further away from the y=x line, the greater the change from initial",
                                      "to final judgement. Separate plots show agreement vs disagreement trials",
                                      "(between the advisor and judge), and separate colours indicate whether the",
                                      "judge's final decision was correct or incorrect. 
                                      The shaded area indicates the boundary for the symmetrical influence measure. 
                                      Points outside this area are truncated by moving them vertically until they 
                                      meet the grey area.",
                                      sep = " "),
                                width=115), collapse = "\n"),
       legend = NULL,
       x = 'Initial confidence',
       y = "Final confidence")
graph.confidence
ggsave(paste0(figPath, "confidence autocorrelation.png"), plot = graph.confidence)

# Now produce that plot for each individual participant
# capConf <- function (x) {
#   ifelse(x<0.0,2*x+55,2*x-55)
# }
for(p in 1:dim(participants)[1]) {
  set <- trials[which(!is.nan(trials$agree) & trials$participantId==participants$participantId[p]),]
  graph.confidence.byP <- ggplot(set, aes(x = cj1, y = cj2)) +
    geom_polygon(data = df.poly1, aes(x,y), fill = 'grey', alpha = 0.2) +
    geom_polygon(data = df.poly2, aes(x,y), fill = 'grey', alpha = 0.2) +
    geom_point(alpha = 0.5, aes(color = factor(cor2))) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1, color = 'black') +
    scale_color_discrete(name = 'Final judgement', labels = c('Incorrect', 'Correct')) +
    scale_x_continuous(limits = c(-55,55), expand = c(0,0)) +
    scale_y_continuous(limits = c(-55,55), expand = c(0,0)) +
    theme_light() +
    theme(panel.spacing = unit(2, 'lines'), legend.position = 'bottom') +
    coord_fixed() +
    facet_grid(~agree, labeller = as_labeller(c('0'='Disagree', '1'='Agree'))) +
    labs(title = paste0("Confidence-Confidence (Participant ", p, ")"),
         subtitle = 'Points outside the shaded area are truncated in the symmetrical influence measure.',
         legend = NULL,
         x = 'Initial confidence',
         y = "Final confidence")
  graph.confidence.byP
  ggsave(paste0(figPath, "confidence-confidenceParticipant", p, ".png"), 
         width = 8, height = 5, units = 'in', plot = graph.confidence.byP)
}

## 17) Subjective/Objective influence
print('## 17) Subject/Objective influence ###############################################')

# We can correlate subjective and objective influence 
tmp <- participants[,c('participantId', 'aicInfluence', 'aiuInfluence')]
tmp <- melt(tmp, id.vars = c('participantId'), value.name = 'influence', variable.name = 'advisor')
levels(tmp$advisor) <- c(adviceTypes$AiC, adviceTypes$AiU)
for(p in 1:dim(tmp)[1]) {
  tmp$influence.Q[p] <- questionnaires$answer[which(questionnaires$participantId==tmp$participantId[p]
                                                    & questionnaires$questionNumber==questionnaireDimensions$influence
                                                    & questionnaires$adviceType==tmp$advisor[p]
                                                    & questionnaires$timePoint==4)]
}

equations <- data.frame(str=character())
for(i in 1:2) {
  equation <- lm(influence.Q ~ influence, data = tmp[which(tmp$advisor==i),])
  equationText <- lmToStr(equation, c('x'), roundTo = 2)
  equations <- rbind(equations, data.frame(str=equationText))
  print(paste0('Advice type ', i))
  print(summary(equation))
}

graph.influence.correlation <- ggplot(tmp, aes(x = influence, y = influence.Q, color = advisor)) +
  geom_point() +
  geom_smooth(method = 'lm', aes(fill = advisor), alpha = 0.1) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_color_discrete(name = 'Advisor', labels = c('Agree-in-confidence', 'Agree-in-uncertainty')) +
  scale_fill_discrete(name = 'Advisor', labels = c('Agree-in-confidence', 'Agree-in-uncertainty')) +
  labs(title = 'Correlation between behaviour and self-report influence',
       subtitle = paste(strwrap(paste("Correlation between self-report measures of advisor influence 
                                      and behavioural measures of advisor influence for each advisor. 
                                      Solid lines are best-fit lines and shaded areas give 95% confidence 
                                      intervals.",
                                      sep = " "),
                                width=115), collapse = "\n"),
       x = 'Behavioural influence',
       y = '"How much are you influenced by the opinions of this person?"') +
  annotate(geom = 'text', x = 10, y = 40, color = '#F8766D', label = equations$str[1]) +
  annotate(geom = 'text', x = 10, y = 35, color = '#00BFC4', label = equations$str[2])
graph.influence.correlation
ggsave(paste0(figPath, "behaviour-SelfReportCorrelation.png"), plot = graph.influence.correlation)
  
# We should check that influence is the best-correlated questionnaire question
# with the behavioural influence measure...
for(q in unique(questionnaires$questionTextShort)) {
  print(q)
  tmp <- questionnaires[which(questionnaires$questionTextShort==q
                              & questionnaires$timePoint==4
                              & questionnaires$adviceType==1),
                        c('participantId', 'answer')]
  for(i in 1:dim(tmp)[1])
    tmp$influence[i] <- participants$aicInfluence[which(participants$participantId==tmp$participantId[i])]
  equation <- lm(answer ~ influence, data = tmp)
  equationText <- lmToStr(equation, c(q), 2)
  print(summary(equation))
  print(equationText)
}

# We should also do what we just did, but using capped influence:
tmp <- participants[,c('participantId', 'aicInfluence.capped', 'aiuInfluence.capped')]
tmp <- melt(tmp, id.vars = c('participantId'), value.name = 'influence.capped', variable.name = 'advisor')
levels(tmp$advisor) <- c(adviceTypes$AiC, adviceTypes$AiU)
for(p in 1:dim(tmp)[1]) {
  tmp$influence.Q[p] <- questionnaires$answer[which(questionnaires$participantId==tmp$participantId[p]
                                                    & questionnaires$questionNumber==questionnaireDimensions$influence
                                                    & questionnaires$adviceType==tmp$advisor[p]
                                                    & questionnaires$timePoint==4)]
}

equations <- data.frame(str=character())
for(i in 1:2) {
  equation <- lm(influence.Q ~ influence.capped, data = tmp[which(tmp$advisor==i),])
  equationText <- lmToStr(equation, c('x'), roundTo = 2)
  equations <- rbind(equations, data.frame(str=equationText))
  print(paste0('Advice type ', adviceTypeNames[i]))
  print(summary(equation))
}
graph.influence.correlation.capped <- ggplot(tmp, aes(x = influence.capped, y = influence.Q, color = advisor)) +
  geom_point() +
  geom_smooth(method = 'lm', aes(fill = advisor), alpha = 0.1) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_color_discrete(name = 'Advisor', labels = c('Agree-in-confidence', 'Agree-in-uncertainty')) +
  scale_fill_discrete(name = 'Advisor', labels = c('Agree-in-confidence', 'Agree-in-uncertainty')) +
  labs(title = 'Correlation between behaviour and self-report influence',
       subtitle = paste(strwrap(paste("Correlation between self-report measures of advisor influence 
                                      and behavioural measures of advisor influence for each advisor. 
                                      Solid lines are best-fit lines and shaded areas give 95% confidence 
                                      intervals.",
                                      sep = " "),
                                width=115), collapse = "\n"),
       x = 'Behavioural influence (capped)',
       y = '"How much are you influenced by the opinions of this person?"') +
  annotate(geom = 'text', x = 10, y = 40, color = '#F8766D', label = equations$str[1]) +
  annotate(geom = 'text', x = 10, y = 35, color = '#00BFC4', label = equations$str[2])
graph.influence.correlation.capped
ggsave(paste0(figPath, "behaviour-SelfReportCorrelationCapped.png"), plot = graph.influence.correlation.capped)

## 18) Missing ANOVA values
print('## 18) Missing ANOVA values ######################################################')

# Investigating:
# "one participant had zero mid-confidence choice trials in which the
# agree-in-confidence advisor agreed with them; and the other had zero
# mid-confidence choice trials in which the agree-in-uncertainty advisor agreed
# with them"

# How many mid-confidence trials with a given advisor did each participant have?
for(p in 1:dim(participants)[1]) {
  set <- trials[which(trials$participantId==participants$participantId[p]),]
  participants$medConf[p] <- length(which(set$step==confidenceTypes$medium))
  participants$aic.medConf[p] <- length(which(set$step==confidenceTypes$medium
                                              & set$adviceType==adviceTypes$AiC))
  participants$aiu.medConf[p] <- length(which(set$step==confidenceTypes$medium
                                              & set$adviceType==adviceTypes$AiU))
  participants$disagreeChoice.medConf <- participants$aicDisagree.medConf + participants$aiuDisagreeChoice.medConf
}

tmp <- participants[,c('participantId', 'medConf', 'disagreeChoice.medConf', 'aic.medConf', 'aiu.medConf', 
                       'aicDisagree.medConf', 'aiuDisagree.medConf',
                       'aicDisagreeChoice.medConf', 'aiuDisagreeChoice.medConf')]

## 19) Questionnaire correlations
print('## 19) Questionnaire correlations ################################################')

# How much of the variance in advisor choice explained by the various
# questionnaire dimensions is unique to dimensions?
tmp <- participants[,c('participantId', 'aicPickRate')]
for(p in 1:dim(tmp)[1]) {
  # these are difference scores: AiC - AiU
  tmp$accurate[p] <- questionnaires$answer[which(questionnaires$participantId==tmp$participantId[p]
                                                 & questionnaires$questionNumber==questionnaireDimensions$accurate
                                                 & questionnaires$timePoint==4
                                                 & questionnaires$adviceType==adviceTypes$AiC)] -
    questionnaires$answer[which(questionnaires$participantId==tmp$participantId[p]
                                & questionnaires$questionNumber==questionnaireDimensions$accurate
                                & questionnaires$timePoint==4
                                & questionnaires$adviceType==adviceTypes$AiU)]
  tmp$influential[p] <- questionnaires$answer[which(questionnaires$participantId==tmp$participantId[p]
                                                    & questionnaires$questionNumber==questionnaireDimensions$influence
                                                    & questionnaires$timePoint==4
                                                    & questionnaires$adviceType==adviceTypes$AiC)] -
    questionnaires$answer[which(questionnaires$participantId==tmp$participantId[p]
                                & questionnaires$questionNumber==questionnaireDimensions$influence
                                & questionnaires$timePoint==4
                                & questionnaires$adviceType==adviceTypes$AiU)]
  tmp$trustworthy[p] <- questionnaires$answer[which(questionnaires$participantId==tmp$participantId[p]
                                                    & questionnaires$questionNumber==questionnaireDimensions$trust
                                                    & questionnaires$timePoint==4
                                                    & questionnaires$adviceType==adviceTypes$AiC)] -
    questionnaires$answer[which(questionnaires$participantId==tmp$participantId[p]
                                & questionnaires$questionNumber==questionnaireDimensions$trust
                                & questionnaires$timePoint==4
                                & questionnaires$adviceType==adviceTypes$AiU)]
  tmp$likeable[p] <- questionnaires$answer[which(questionnaires$participantId==tmp$participantId[p]
                                                 & questionnaires$questionNumber==questionnaireDimensions$like
                                                 & questionnaires$timePoint==4
                                                 & questionnaires$adviceType==adviceTypes$AiC)] -
    questionnaires$answer[which(questionnaires$participantId==tmp$participantId[p]
                                & questionnaires$questionNumber==questionnaireDimensions$like
                                & questionnaires$timePoint==4
                                & questionnaires$adviceType==adviceTypes$AiU)]
}
equation.1 <- lm(aicPickRate ~ accurate, data = tmp)
summary(equation.1)
equation.2 <- lm(aicPickRate ~ accurate + trustworthy, data = tmp)
summary(equation.2)
equation.3 <- lm(aicPickRate ~ accurate + trustworthy + influential, data = tmp)
summary(equation.3)
equation.4 <- lm(aicPickRate ~ accurate + trustworthy + influential + likeable, data = tmp)
summary(equation.4)

anova(equation.1, equation.2, equation.3, equation.4)

## 20) ANCOVA for initial experience
print('## 20) ANCOVA for inital experience ##############################################')

## We can run the main ANOVA as an ANCOVA and control for initial agreement rate difference
for(pId in unique(participants.influence.capped$participantId)) {
  participants.influence.capped[which(participants.influence.capped$participantId==pId),'agreeRateDifference.block3'] <-
    participants.byBlock[which(participants.byBlock$participantId==pId),'agreeRateDifference.block3']
}
ancova.influence <- ezANOVA(data = participants.influence.capped,
                           dv = value, 
                           wid = participantId,
                           within = c('AiC', 'agree', 'hasChoice'),
                           between_covariates = c('agreeRateDifference.block3'),
                           return_aov = T)
ancova.influence$ANOVA

## 21) Changes of mind
print('## 21) Changes of mind ###########################################################')

## Looking at changes of mind

# First we build a data frame with processed values for trials on which the
# participant did/n't change their mind
participants.changeMind <- participants
for(p in 1:dim(participants)[1]) {
  set <- trials[which(trials$participantId==participants$participantId[p]),]
  tmp <- scanTrials(set[which(sign(set$cj1)==sign(set$cj2)),],'noChange')
  tmp <- c(tmp, scanTrials(set[which(sign(set$cj1)!=sign(set$cj2) & !is.nan(set$cj2)),],'change'))
  for(n in names(tmp))
    participants.changeMind[p, n] <- tmp[n]
}
tmp <- participants.changeMind
tmp$changeMindRate <- tmp$trialCount.change / tmp$trialCount
tmp$forcedChangeMindRate <- tmp$forcedCount.change / tmp$forcedCount
tmp$choiceChangeMindRate <- tmp$choiceCount.change / tmp$choiceCount
tmp <- melt(tmp, id.vars = c('participantId'), 
            measure.vars = c('changeMindRate', 'forcedChangeMindRate', 'choiceChangeMindRate'))
tmp$participantId <- as.factor(tmp$participantId)
# let's plot the mean number of trials where people change their mind
graph.changeMind.count <- ggplot(tmp, aes(variable, value)) +
  geom_point(aes(color = participantId)) +
  geom_line(aes(group = participantId, color = participantId)) +
  stat_summary(fun.y = mean, geom = 'point', size = 5, shape = 23) +
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', size = 0.1, width = 0.25) +
  geom_violin(alpha = 0.1, fill = 'blue') +
  theme_light() +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = 'Changes of mind')

graph.changeMind.count


write.csv(participants, 'participants.csv')
# write.csv(trials, 'trials.csv')
write.csv(participants.influence, 'participants-influence.csv')
