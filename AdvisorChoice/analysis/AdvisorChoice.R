## Analysis script for AdvisorChoice experiment data ##############################################
# Matt Jaquiery, Jan 2018 (matt.jaquiery@psy.ox.ac.uk)
#
# i) Get Data
# ii) Function definitions - could move to another file
# 0) Exclusions
# 1) Demographics
# 2) Is the agree-in-confidence advisor selected more often?
# 3) ANOVA investigating influence
# 4) Trust questionnaire answers
#   i. Trust for each advisor
# 5) Do participants simply prefer agreement?

## Citations 

# Bayes stuff
if(!require('BayesFactor')) {
  install.packages('BayesFactor')
}
library(BayesFactor)
citation("BayesFactor")
# effect sizes for anova
if(!require('DescTools')) {
  install.packages('DescTools')
}
library(DescTools)
citation("DescTools")
# effect sizes for t.tests
if(!require('lsr')) {
  install.packages('lsr')
}
library(lsr)
citation("lsr")

#(Called in mat2R)
#Henrik Bengtsson (2016). R.matlab: Read and Write MAT Files and Call MATLAB from
#Within R. R package version 3.6.0-9000. https://github.com/HenrikBengtsson/R.matlab
citation("R.matlab")

## i) Get Data ####################################################################################
print('Loading data')

# Use the mat2R functions to get hold of some data and do some manipulations
if(!exists("getMatlabData", mode="function")) {
  #oldwd <- getwd()
  #setwd(dirname(sys.frame(1)$ofile))
  #source("mat2R.R")
  #setwd(oldwd)
  source('G:/Documents/University/Programming/nofeedback_trust_matt/AdvisorChoice/analysis/mat2r.R')
} 

#acPth <- "C:/Users/mj221/Filr/My Files/Results/AdvisorChoice"
acPth <- "G:/Documents/University/Google Drive/Results/AdvisorChoice"

raw_study <- getMatlabData(acPth)  # get some data from the path defined for convenience in mat2R
# unpack
participants <- raw_study$participants
advisors <- raw_study$advisors
trials <- raw_study$trials
questionnaires <- raw_study$questionnaires

## ii) Some functions #############################################################################
print('Defining functions')

# all advisors agreement probability = .3 where initial judgement is incorrect
adviceTypes <- list(neutral=0, # neutral: agree|correct=.7
                    AiC=1, # agree-in-confidence: agree|correct+highConfidence=.8, agree|correct+medConfidence=.7, agree|correct+lowConfidence=.6
                    AiU=2) # agree-in-uncertainty: agree|correct+highConfidence=.6, agree|correct+medConfidence=.7, agree|correct+lowConfidence=.8

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
getQuestionText <- function (qId) {
  if(qId==questionnaireDimensions$accurate)
    return('How accurate do you think this person was when performing the task?')
  if(qId==questionnaireDimensions$like)
    return('How much do you like this person?')
  if(qId==questionnaireDimensions$trust)
    return('How trustworthy are the opinions of this person?')
  if(qId==questionnaireDimensions$influence)
    return('How much are you influenced by the opinions of this person?')
  return(NA)
} 

# Print the results of a t-test as we would like to see them reported in a paper
prettyPrint <- function(results, d) {
  if(length(results$estimate)==2) 
    mu <- paste0('Mdiff=', round(results$estimate[[1]]-results$estimate[[2]],2),
                 ' [',round(attr(results$conf.int, "conf.level")*100),'%CI: ',
                 round(results$conf.int[[1]],2), ', ', round(results$conf.int[[2]],2),']')
  else
    mu <- paste0('Mean=', round(results$estimate[[1]],2), 
                 ' [',round(attr(results$conf.int, "conf.level")*100),'%CI: ',
                 round(results$conf.int[[1]],2), ', ', round(results$conf.int[[2]],2),']',
                 ' vs ', results$null.value)
  print(paste0('t(',results$parameter,')=',round(results$statistic,2),
               ', ', mu,
               ', p=', round(results$p.value,3),
               ', d=', round(d,2)))
}

# Generate tables of marginal means 
marginal.means <- function(dependant, factors, data, ci.level = 0.95, na.rm = T) {
  #print(paste('marginal means:', paste(factors, collapse = ', ')))
  #assign("debug.input", data, envir=globalenv())
  out <- vector('list', length=length(factors))
  names(out) <- factors
  for(f in 1:length(out)) {
    fact <- factors[[f]]
    lvls <- levels(factor(data[,fact]))
    if(!length(lvls))
      next
    out[[f]] <- list()
    others <- factors[factors!=fact] # used for tracking interactions
    out[[f]] <- vector('list', length=length(lvls))
    df <- data.frame(level=character(), mean=double(), sd=double(), ci.95.low=double(), ci.95.high=double())
    for(L in 1:length(lvls)) {
      name <- lvls[[L]]
      set <- data[which(data[,fact]==name), dependant]
      if (na.rm)
        set <- set[!is.na(set)]
      mu <- mean(set)
      s <- sd(set)
      n <- dim(data)[1]
      error <- qnorm(1-(1-ci.level)/2)*s/sqrt(n) # 95% confidence interval width
      ci.95.low <- mu - error
      ci.95.high <- mu + error
      df <- rbind(df, data.frame(level=name, mean=mu, sd=s, ci.95.low, ci.95.high))
      # handle interactions using recursion
      if(length(others) && length(data[which(data[,fact]==name),])) {
        recur <- marginal.means(dependant, others,  data[which(data[,fact]==name),], ci.level,  na.rm)
        if(length(recur))
          out[[f]][paste0(fact,'.',name)] <- recur
        #assign("debug", recur, envir=globalenv())
      }
    }
    out[[f]]$means <- df
    names(out[[f]])[names(out[[f]])=='means'] <- paste0(fact,'.total')
  }
  return(out)
}

# Return the proportion of trials on which participant pId picked advisor with specified adviceType
getPickProportion <- function(pId, trials, adviceType) {
  trials <- trials[!is.na(trials$participantId),]
  myTrials <- trials[trials$participantId==pId,]
  choiceTrials <- myTrials[myTrials$hasChoice==T,]
  advisorPicks <- choiceTrials[choiceTrials$adviceType==adviceType,]
  #print(paste0('pId: ', pId, '; choiceTrials: ', dim(choiceTrials)[1]))
  return(dim(advisorPicks)[1] / dim(choiceTrials)[1])
}




## 0) Exclusions ##################################################################################
print('## 0) Running exclusions #########################################################')
# Exclusion rules:
# Proportion of correct initial judgements must be (.60 < cor1/n < .90) 
#NB:practice trials are INCLUDED in this since they are used in part for
#determining confidence calibration
participants$proportionCorrect <- vector(length=dim(participants)[1])
participants$excluded <- sample(F, dim(participants)[1], replace=T) # as with proportionCorrect, fill with False
exclusions <- list()
for(p in 1:dim(participants)[1]) {
  pId <- participants$participantId[p]
  n <- which(trials$participantId==pId)
  cor1 <- which(trials[n,"cor1"]==1)
  proportion <- length(cor1)/length(n)
  participants$proportionCorrect[p] <- proportion
  if(proportion < .6 || proportion > .9) {
    print(paste("Excluding participant",p,"with proportion correct of", round(proportion,2)))
    participants$excluded[p] <- T
  }
}

# Trim the dataset down to the first 24 participants as promised in the pre-registration
participants$excluded[25:dim(participants)[1]] = T

# backup the full participant list and reframe the original to honour exclusions
all.participants <- participants
participants <- participants[participants$excluded==F,]
print(paste('>>Excluded participant count:', length(which(all.participants$excluded==T))))

# While we're here, remove practice trials from the main trials table
all.trials <- trials
trials <- trials[trials$practice==0,]
trials <- trials[which(trials$participantId %in% participants$participantId),] # discard trials for excluded participants

# we'll also get rid of the advisors, though this is less important
all.advisors <- advisors
advisors <- advisors[advisors$advice.type!=adviceTypes$neutral,] # neutral = even advice, used for the practice advisors only
advisors <- advisors[which(advisors$participantId %in% participants$participantId),]

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

# first we calculate whether each trial had a choice to simplify things later
# Since the only real choice is advisor 2 vs advisor 1 we can check for a sum of 3.
# we also want to store the advice type for each trial since we'll probably use that a lot...
trials$hasChoice <- vector(length = dim(trials)[1])
trials$adviceType <- trials$hasChoice
trials$advisorId <- as.numeric(trials$advisorId)
for(i in 1:dim(trials)[1]) {
  trials$hasChoice[i] <- sum(trials[i,'choice'][[1]], na.rm = T)==3
  if (is.nan(as.numeric(trials$advisorId[i])) || trials$advisorId[i] < 1)
    trials$adviceType[i] <- NA
  else
    trials$adviceType[i] <- advisors$advice.type[which(advisors$participantId==trials$participantId[i] & advisors$id==trials$advisorId[i])]
}
  

participants$aic.pick.proportion <- vector(length=dim(participants)[1])
participants$aic.pick.proportion.initial.correct <- participants$aic.pick.proportion
participants$aic.pick.proportion.medium.confidence <- participants$aic.pick.proportion
trials$cor1 <- as.numeric(trials$cor1)
for(p in 1:dim(participants)[1]) {
  pId <- participants$participantId[p]
  participants$aic.pick.proportion[p] <- getPickProportion(pId, trials, adviceTypes$AiC)
  # We also check the above using only the trials where the [correct] initial decision was made with middling confidence (step==0)
  # To interpret the result we first want to know what the proportion was when correct at any confidence
  participants$aic.pick.proportion.initial.correct[p] <- getPickProportion(pId, 
                                                                           trials[trials$cor1==1,], 
                                                                           adviceTypes$AiC)
  # Finally we can try just those trials where confidence is medium
  participants$aic.pick.proportion.medium.confidence[p] <- getPickProportion(pId,
                                                                             trials[trials$cor1==1 & trials$step==confidenceTypes$medium,],
                                                                             adviceTypes$AiC)
}

aic.selection <- data.frame('mean' = mean(participants$aic.pick.proportion), 
                            'sd' = sd(participants$aic.pick.proportion), 
                            row.names = 'proportion of Agree-in-Confidence picks')
aic.selection

aic.test <- t.test(participants$aic.pick.proportion, mu=0.5) # testing the proportions versus the null hypothesis of 0.5 (chance selection)
aic.test.d <- cohensD(participants$aic.pick.proportion, mu=0.5)
print('>>(aic.test) choice proportion Agree-in-Confidence vs. chance level (.5)')
prettyPrint(aic.test, aic.test.d)
print('>>(aic.test.b) bayesian examination of above (prior = mean of 0.5, sd as empirically observed)')
aic.test.b <- ttestBF(participants$aic.pick.proportion, mu = 0.5)
print(aic.test.b)
print(paste0('Evidence strength for preferential AiC picking: BF=', round(exp(aic.test.b@bayesFactor$bf),3)))


## 3) ANOVA investigating influence ###############################################################
print('## 3) ANOVA investigating influence ##############################################')

#Influence is defined as the extent to which the judge's (participant's) final
#decision has moved from their initial decision in the direction of the advice
#received.

# We begin by calculating influence for all trials and saving that information
# since it will come in handy for looking at influence on subsets of trials
# later. Below, we run an ANOVA using the influence data.

print('Calculating influence on each trial')
# Calculate the influence of the advisor on each trial
trials$shift <- vector(length = dim(trials)[1]) #  amount the confidence changes
trials$influence <- trials$shift # amount the confidence changes in the direction of the advice
trials$cj1 <- as.numeric(trials$cj1)
trials$cj2 <- as.numeric(trials$cj2)
trials$agree <- as.numeric(trials$agree)
for(t in 1:dim(trials)[1]) {
  if(is.na(trials$adviceType[t])) {
    # trials without an advisor are entered as NA
    trials$shift[t] <- NA
    trials$influence[t] <- NA
  } else {
    # calculate the confidence change on this trial ('shift')
    # shift = Cpost - Cpre
    # C-values are *-1 if answer was 'left', so
    # Cpre [1,55]
    # Cpost [-55,-1]U[1,55]
    c.pre <- trials$cj1[t]
    c.post <- trials$cj2[t]
    if(c.pre<0) {
      # initial response is 'left'
      c.pre <- c.pre * -1
      c.post <- c.post * -1
    } 
    trials$shift[t] <- c.post - c.pre # +ve values indicate shift towards more confidence in initial response
    if(trials$agree[t]==1) {
      # on agreement trials shift towards initial response inidicates following advice
      trials$influence[t] <- trials$shift[t]
    } else {
      # on disagreement trials shift AWAY from initial response inidicates following advice
      trials$influence[t] <- trials$shift[t] * -1
    }
  }
}

# 2x2x2 ANOVA investigating effects of advisor type
# (agree-in-confidence/uncertainty), choice (un/forced), and agreement
# (dis/agree) on influence. These are all within-subjects manipulations.
print('Running ANOVAs')
anova.output <- aov(formula = influence ~ adviceType * hasChoice * agree + Error(participantId), data=trials)
print('>>(anova.output)')
print(summary(anova.output))
EtaSq(anova.output, type = 1)
# Bayesian version
# print('Bayesian ANOVA')
# anova.output.b <- anovaBF(formula = influence ~ adviceType * hasChoice * agree + Error(participantId), 
#                           data=trials[!is.nan(trials$advisorId),])
# print(anova.output.b)
print('Marginal means')
marginal.means('influence', c('adviceType', 'hasChoice', 'agree'), trials[!is.nan(trials$advisorId),])

# The bias-sharing advisor and anti-bias advisors differ in their frequency with
# which they agree with the participant as a  function of participant confidence
# (by design). To control for background effects where people are influenced
# different amounts depending on their own initial confidence, we also look at
# only those trials where participant confidence was in the mid-range (i.e.
# where both advisors agree 70% of the time, and thus where agreement and
# confidence balance out). This subset only includes trials on which the
# participant was CORRECT since the step information is not recorded for
# incorrect trials (where all advisors agree 30% of the time).
anova.output.70 <- aov(formula = influence ~ adviceType * hasChoice * agree + Error(participantId), 
                       data=trials[trials$step==confidenceTypes$medium,])
print('>>(anova.output.70) Looking at only trials where intial decision was correct and made with middle confidence:')
print(summary(anova.output.70))
EtaSq(anova.output.70, type = 1)
# Bayesian version
# print('Bayesian ANOVA')
# anova.output.70.b <- anovaBF(formula = influence ~ adviceType * hasChoice * agree + Error(participantId),
#                              data=trials[!is.nan(trials$advisorId) & trials$step==confidenceTypes$medium,])
# print(anova.output.70.b)
print('Marginal means')
marginal.means('influence', c('adviceType', 'hasChoice', 'agree'), trials[!is.nan(trials$advisorId) & trials$step==confidenceTypes$medium,])

## 4) Trust questionnaire answers #################################################################
#   i. Trust for each advisor

# We want to know if trust for each advisor changes over time. 
# 
# Questions:
# 1) Advisor accuracy
# 2) Advisor likeability
# 3) Advisor trustworthiness
# 4) Advisor influence

questionnaires$answer <- as.numeric(questionnaires$answer)
questionnaires$questionId <- as.numeric(questionnaires$questionId)
# Look up advisorType and put it in the table for ease of reference
questionnaires$adviceType <- vector(length=dim(questionnaires)[1])
for(q in 1:dim(questionnaires)[1]) {
  questionnaires$adviceType[q] <- advisors$advice.type[advisors$id==questionnaires$advisorId[q] 
                                                       & advisors$participantId==questionnaires$participantId[p]]
}

# We can get a quick overview from an anova looking for main effect of advisor
# and interactions of timepoint and advisor
print('>>(trust.test) ANOVA exploring trust questionnaire responses')
trust.test <- aov(formula = answer ~ adviceType * timePoint * questionId + Error(participantId),
                  data = questionnaires)
summary(trust.test)
EtaSq(trust.test, type = 1)
# Bayesian version
# print('Bayesian ANOVA')
# trust.test.b <- anovaBF(formula = answer ~ adviceType * timePoint * questionId + Error(participantId),
#                         data = questionnaires)
# print(trust.test.b)
print('Marginal means')
marginal.means('answer', c('adviceType', 'timePoint', 'questionId'), questionnaires)

# Were the advisors perceived differently to begin with?
trust.test.t1 <- t.test(questionnaires[questionnaires$timePoint==1 & questionnaires$adviceType==adviceTypes$AiC,"answer"],
                        questionnaires[questionnaires$timePoint==1 & questionnaires$adviceType==adviceTypes$AiU,"answer"], 
                        paired=T)
trust.test.t1.d <- cohensD(questionnaires[questionnaires$timePoint==1 & questionnaires$adviceType==adviceTypes$AiC,"answer"],
                           questionnaires[questionnaires$timePoint==1 & questionnaires$adviceType==adviceTypes$AiU,"answer"])
print('>>(trust.test.t1) Paired t-test for whether advisors are trusted differentially at the beginning of the experiment')
prettyPrint(trust.test.t1, trust.test.t1.d)
print('>>(trust.test.t1.b) bayesian examination of above')
trust.test.t1.b <- ttestBF(questionnaires[questionnaires$timePoint==1 & questionnaires$adviceType==adviceTypes$AiC,"answer"],
                           questionnaires[questionnaires$timePoint==1 & questionnaires$adviceType==adviceTypes$AiU,"answer"],
                           paired=T)
print(trust.test.t1.b)
print(paste0('Evidence strength for higher AiC trust: BF=', round(exp(trust.test.t1.b@bayesFactor$bf),3)))

# Were they perceived differently from one another at the end?
t <- max(questionnaires$timePoint) # find last time point
trust.test.tLast <- t.test(questionnaires[questionnaires$timePoint==t & questionnaires$adviceType==adviceTypes$AiC,"answer"],
                           questionnaires[questionnaires$timePoint==t & questionnaires$adviceType==adviceTypes$AiU,"answer"],
                           paired=T)
trust.test.tLast.d <- cohensD(questionnaires[questionnaires$timePoint==t & questionnaires$adviceType==adviceTypes$AiC,"answer"],
                              questionnaires[questionnaires$timePoint==t & questionnaires$adviceType==adviceTypes$AiU,"answer"])
print('>>(trust.test.tLast) Paired t-test for whether advisors are trusted differentially at the end of the experiment')
prettyPrint(trust.test.tLast, trust.test.tLast.d)
print('>>(trust.test.tLast.b) bayesian examination of above')
trust.test.tLast.b <- ttestBF(questionnaires[questionnaires$timePoint==t & questionnaires$adviceType==adviceTypes$AiC,"answer"],
                           questionnaires[questionnaires$timePoint==t & questionnaires$adviceType==adviceTypes$AiU,"answer"],
                           paired=T)
print(trust.test.tLast.b)
print(paste0('Evidence strength for higher AiC trust: BF=', round(exp(trust.test.tLast.b@bayesFactor$bf),3)))

# We should look at some specifics in the above, such as trust

# Exploratory: influence shows as significantly different between advisors, so check questionnaire answers on influence

## 5) Do participants simply prefer agreement? ####################################################

# If so, we should see that participants preferentially pick agree-in-confidence
# advisor when their initial confidence is high, and agreee-in-uncertainty when
# their initial confidence is low. We can t-test aic pick proportion in
# high-confidence vs aic pick proportion in low-confidence.
participants$aic.pick.proportion.low.confidence <- vector(length=dim(participants)[1])
participants$aic.pick.proportion.high.confidence <- participants$aic.pick.proportion.low.confidence
for(p in 1:dim(participants)[1]) {
  pId <- participants$participantId[p]
  # low confidence
  participants$aic.pick.proportion.low.confidence[p] <- getPickProportion(pId, 
                                                                          trials[trials$cor1==1 & trials$step==confidenceTypes$low,], 
                                                                          adviceTypes$AiC)
  # high confidence
  participants$aic.pick.proportion.high.confidence[p] <- getPickProportion(pId, 
                                                                           trials[trials$cor1==1 & trials$step==confidenceTypes$high,],
                                                                           adviceTypes$AiC)
}


aic.byConf.test <- t.test(participants$aic.pick.proportion.low.confidence, 
                          participants$aic.pick.proportion.high.confidence,
                          paired=T) # do selection proportions differ by initial confidence?
aic.byConf.test.d <- cohensD(participants$aic.pick.proportion.low.confidence,
                             participants$aic.pick.proportion.high.confidence)
print('>>(aic.byConf.test) choice proportion Agree-in-confidence in low vs. high inital confidence')
prettyPrint(aic.byConf.test, aic.byConf.test.d)
print('>>(aic.byConf.test.b) bayesian examination of above (prior = mean of 0.5, sd as empirically observed)')
aic.byConf.test.b <- ttestBF(participants$aic.pick.proportion.low.confidence, 
                             participants$aic.pick.proportion.high.confidence,
                             paired = T)
print(aic.byConf.test.b)
print(paste0('Evidence strength for differential AiC picking: BF=', round(exp(aic.byConf.test.b@bayesFactor$bf),3)))
