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

#(Called in mat2R)
#Henrik Bengtsson (2016). R.matlab: Read and Write MAT Files and Call MATLAB from
#Within R. R package version 3.6.0-9000. https://github.com/HenrikBengtsson/R.matlab
citation("R.matlab")

## i) Get Data ####################################################################################
print('Loading data')

# Use the mat2R functions to get hold of some data and do some manipulations
if(!exists("getMatlabData", mode="function")) {
  source("mat2R.R")
} 

#acPth <- "C:/Users/mj221/Filr/My Files/Results/AdvisorChoice"
acPth <- "G:/Documents/University/Google Drive/Results/AdvisorChoice"

raw_study <- getMatlabData(acPth)  # get some data from the path defined for convenience in mat2R
# unpack
participants <- raw_study$participants
advisors <- raw_study$advisors
trials <- raw_study$trials
questionnaires <- raw_study$questionnaires

# the raw study data is quite large, so drop it
rm('raw_study')

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

printMean <- function(vector, conf.int = .95, na.rm = F) {
  mu <- mean(vector, na.rm = na.rm)
  s <- sd(vector, na.rm = na.rm)
  n <- length(vector)
  error <- qnorm(1-(1-conf.int)/2)*s/sqrt(n) # 95% confidence interval width
  ci.low <- mu - error
  ci.high <- mu + error
  print(paste0('Mean=', round(mu,2), ' [', round(conf.int,2)*100, '%CI: ',
               round(ci.low,2), ', ', round(ci.high,2),']'))
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

# Generate a variety of statistics pertaining to the trials in trialSet and
# return them in a data frame with colnames ending in .suffix
scanTrials <- function(trialSet, suffix = NULL) {
  df <- data.frame(trialCount = dim(trialSet)[1])
  # The medium-confidence trials come in handy, so cache them here
  medSet <- trialSet[which(trialSet$step==confidenceTypes$medium),]
  # Proportion of trials in which the initial decision was correct
  df$proportionCorrect <- length(which(trialSet$cor1==1)) / dim(trialSet)[1]
  # Proportion of trials in which the intiial decision was incorrect
  df$proportionCorrectFinal <- length(which(trialSet$cor2==1)) / length(trialSet)
  # Number of choice trials on which the agree-in-confidence advisor was selected
  df$aicPickCount <- length(which(trialSet$hasChoice & trialSet$adviceType==adviceTypes$AiC))
  # Number of choice trials on which the agree-in-uncertainty advisor was selected
  df$aiuPickCount <- length(which(trialSet$hasChoice & trialSet$adviceType==adviceTypes$AiU))
  # Proportion of choice trials on which the agree-in-confidence advisor was selelcted
  df$aicPickRate <- length(which(trialSet$hasChoice & trialSet$adviceType==adviceTypes$AiC)) / 
    length(which(trialSet$hasChoice))
  # Number of choice trials on which the initial decision was correct and the
  # agree-in-confidence advisor was selected
  df$aicPickCount.correct <- length(which(trialSet$hasChoice
                                       & trialSet$adviceType==adviceTypes$AiC
                                       & trialSet$cor1==1))
  # Number of choice trials on which the initial decision was correct and the
  # agree-in-uncertainty advisor was selected
  df$aiuPickCount.correct <- length(which(trialSet$hasChoice
                                       & trialSet$adviceType==adviceTypes$AiU
                                       & trialSet$cor1==1))
  # Proportion of choice trials on which the initial decision was correct and
  # the agree-in-confidence advisor was selected
  df$aicPickRate.correct <- length(which(trialSet$hasChoice 
                                      & trialSet$adviceType==adviceTypes$AiC 
                                      & trialSet$cor1==1)) / 
    length(which(trialSet$hasChoice & trialSet$cor1==1))
  # NB: all confidence-category selections require the initial decision to have
  # been correct. 
  
  # Number of low-confidence choice trials on which the agree-in-confidence
  # advisor was selected
  df$aicPickCount.lowConf <- length(which(trialSet$hasChoice 
                                       & trialSet$adviceType==adviceTypes$AiC 
                                       & trialSet$step == -1))
  # Number of low-confidence choice trials on which the agree-in-uncertainty
  # advisor was selected
  df$aiuPickCount.lowConf <- length(which(trialSet$hasChoice 
                                       & trialSet$adviceType==adviceTypes$AiU 
                                       & trialSet$step == -1))
  # Proportion of choice trials on which confidence in the initial
  # decision was low, and the agree-in-confidence advisor was selected
  df$aicPickRate.lowConf <- length(which(trialSet$hasChoice 
                                      & trialSet$adviceType==adviceTypes$AiC 
                                      & trialSet$step == -1)) / 
    length(which(trialSet$hasChoice & trialSet$step == -1))
  # Number of medium confidence choice trials on which the agree-in-confidence advisor was selected
  df$aicPickCount.medConf <- length(which(medSet$hasChoice & medSet$adviceType==adviceTypes$AiC))
  # Number of medium choice trials on which the agree-in-uncertainty advisor was selected
  df$aiuPickCount.medConf <- length(which(medSet$hasChoice & medSet$adviceType==adviceTypes$AiU))
  # Proportion of choice trials on which the initial decision was correct,
  # confidence in the initial decision was moderate, and the agree-in-confidence
  # advisor was selected
  df$aicPickRate.medConf <- length(which(trialSet$hasChoice 
                                      & trialSet$adviceType==adviceTypes$AiC 
                                      & trialSet$step == 0)) / 
    length(which(trialSet$hasChoice & trialSet$step == 0))
  # Number of high confidence choice trials on which the agree-in-confidence advisor was selected
  df$aicPickCount.highConf <- length(which(trialSet$hasChoice 
                                        & trialSet$adviceType==adviceTypes$AiC 
                                        & trialSet$step == 1))
  # Number of high choice trials on which the agree-in-uncertainty advisor was selected
  df$aiuPickCount.highConf <- length(which(trialSet$hasChoice 
                                        & trialSet$adviceType==adviceTypes$AiU 
                                        & trialSet$step == 1))
  # Proportion of choice trials on which the initial decision was correct,
  # confidence in the initial decision was high, and the agree-in-confidence
  # advisor was selected
  df$aicPickRate.highConf <- length(which(trialSet$hasChoice 
                                       & trialSet$adviceType==adviceTypes$AiC 
                                       & trialSet$step == 1)) / 
    length(which(trialSet$hasChoice & trialSet$step == 1))
  ## Everything below is influence, so drop catch trials
  trialSet <- trialSet[which(!is.na(trialSet$cor2)),]
  medSet <- medSet[which(!is.na(medSet$cor2)),]
  # Number of agreement trials
  df$agreeCount <- length(which(trialSet$agree==1))
  # Number of disagreement trials
  df$disagreeCount <- length(which(trialSet$agree==0))
  # Number of trials on which the agree-in-confidence advisor agreed
  df$aicAgreeCount <- length(which(trialSet$agree==1 
                                   & trialSet$adviceType==adviceTypes$AiC))
  # Number of trials on which the agree-in-uncertainty advisor agreed
  df$aiuAgreeCount <- length(which(trialSet$agree==1
                                   & trialSet$adviceType==adviceTypes$AiU))
  # Number of choice trials where the advisor agreed
  df$agreeChoice <- length(which(trialSet$agree==1 & trialSet$hasChoice))
  # Number of forced trials where advisor agreed
  df$agreeForced <- length(which(trialSet$agree==1 & !trialSet$hasChoice))
  # Number of choice trials where the agree-in-confidence advisor agreed
  df$aicAgreeChoice <- length(which(trialSet$agree==1 
                                    & trialSet$adviceType==adviceTypes$AiC
                                    & trialSet$hasChoice))
  # Number of choice trials where the agree-in-uncertainty advisor agreed
  df$aiuAgreeChoice <- length(which(trialSet$agree==1
                                    & trialSet$adviceType==adviceTypes$AiU
                                    & trialSet$hasChoice))
  # Number of forced trials where the agree-in-confidence advisor agreed
  df$aicAgreeForced <- length(which(trialSet$agree==1 
                                    & trialSet$adviceType==adviceTypes$AiC
                                    & !trialSet$hasChoice))
  # Number of forced trials where the agree-in-uncertainty advisor agreed
  df$aiuAgreeForced <- length(which(trialSet$agree==1 
                                    & trialSet$adviceType==adviceTypes$AiU
                                    & !trialSet$hasChoice))
  # Number of low-confidence trials where the agree-in-confidence advisor agreed
  df$aicAgree.lowConf <- length(which(trialSet$agree==1
                                      & trialSet$adviceType==adviceTypes$AiC
                                      & trialSet$step == -1))
  # Number of low-confidence trials where the agree-in-uncertainty advisor agreed
  df$aiuAgree.lowConf <- length(which(trialSet$agree==1
                                      & trialSet$adviceType==adviceTypes$AiU
                                      & trialSet$step == -1))
  # Number of med-confidence trials where the agree-in-confidence advisor agreed
  df$aicAgree.medConf <- length(which(trialSet$agree==1
                                      & trialSet$adviceType==adviceTypes$AiC
                                      & trialSet$step == 0))
  # Number of med-confidence trials where the agree-in-uncertainty advisor agreed
  df$aiuAgree.medConf <- length(which(trialSet$agree==1
                                      & trialSet$adviceType==adviceTypes$AiU
                                      & trialSet$step == 0))
  # Number of high-confidence trials where the agree-in-confidence advisor agreed
  df$aicAgree.highConf <- length(which(trialSet$agree==1
                                       & trialSet$adviceType==adviceTypes$AiC
                                       & trialSet$step == 1))
  # Number of high-confidence trials where the agree-in-uncertainty advisor agreed
  df$aiuAgree.highConf <- length(which(trialSet$agree==1
                                       & trialSet$adviceType==adviceTypes$AiU
                                       & trialSet$step == 1))
  # Influence of both advisors combined on all trials
  df$influence <- mean(trialSet$influence)
  df$influence.sd <- sd(trialSet$influence)
  # Influence of the agree-in-confidence advisor on all trials
  df$aicInfluence <- mean(trialSet$influence[which(trialSet$adviceType==adviceTypes$AiC)])
  df$aicInfluence.sd <- sd(trialSet$influence[which(trialSet$adviceType==adviceTypes$AiC)])
  # Influence of the agree-in-uncertainty advisor on all trials
  df$aiuInfluence <- mean(trialSet$influence[which(trialSet$adviceType==adviceTypes$AiU)])
  df$aiuInfluence.sd <- sd(trialSet$influence[which(trialSet$adviceType==adviceTypes$AiU)])
  # Influence of both advisors combined on forced trials
  df$forcedInfluence <- mean(trialSet$influence[which(!trialSet$hasChoice)])
  df$forcedInfluence.sd <- sd(trialSet$influence[which(!trialSet$hasChoice)])
  # Influence of both advisors combined on choice trials
  df$choiceInfluence <- mean(trialSet$influence[which(trialSet$hasChoice)])
  df$choiceInfluence.sd <- sd(trialSet$influence[which(trialSet$hasChoice)])
  # Influence of both advisors combined on agreement trials
  df$agreeInfluence <- mean(trialSet$influence[which(trialSet$agree==1)])
  df$agreeInfluence.sd <- sd(trialSet$influence[which(trialSet$agree==1)])
  # Influence of both advisors combined on disagreement trials
  df$disagreeInfluence <- mean(trialSet$influence[which(trialSet$agree==0)])
  df$disagreeInfluence.sd <- sd(trialSet$influence[which(trialSet$agree==0)])
  # Influence of agree-in-confidence advisor on forced trials
  df$aicForcedInfluence <- mean(trialSet$influence[which(trialSet$adviceType==adviceTypes$AiC
                                                      & !trialSet$hasChoice)])
  df$aicForcedInfluence.sd <- sd(trialSet$influence[which(trialSet$adviceType==adviceTypes$AiC
                                                       & !trialSet$hasChoice)])
  # Influence of the agree-in-uncertinaty advisor on forced trials
  df$aiuForcedInfluence <- mean(trialSet$influence[which(trialSet$adviceType==adviceTypes$AiU
                                                      & !trialSet$hasChoice)])
  df$aiuForcedInfluence.sd <- sd(trialSet$influence[which(trialSet$adviceType==adviceTypes$AiU
                                                       & !trialSet$hasChoice)])
  # Influence of the agree-in-confidence advisor on choice trials
  df$aicChoiceInfluence <- mean(trialSet$influence[which(trialSet$adviceType==adviceTypes$AiU
                                                                   & trialSet$hasChoice)])
  df$aicChoiceInfluence.sd <- sd(trialSet$influence[which(trialSet$adviceType==adviceTypes$AiU
                                                       & trialSet$hasChoice)])
  # Influence of the agree-in-uncertainty advisor on choice trials
  df$aiuChoiceInfluence <- mean(trialSet$influence[which(trialSet$adviceType==adviceTypes$AiU
                                                                   & trialSet$hasChoice)])
  df$aiuChoiceInfluence.sd <- sd(trialSet$influence[which(trialSet$adviceType==adviceTypes$AiU
                                                       & trialSet$hasChoice)])
  # Influence of the agree-in-confidence advisor on agreement trials
  df$aicAgreeInfluence <- mean(trialSet$influence[which(trialSet$agree==1
                                                        & trialSet$adviceType==adviceTypes$AiC)])
  df$aicAgreeInfluence.sd <- sd(trialSet$influence[which(trialSet$agree==1
                                                        & trialSet$adviceType==adviceTypes$AiC)])
  # Influence of the agree-in-uncertainty advisor on agreement trials
  df$aiuAgreeInfluence <- mean(trialSet$influence[which(trialSet$agree==1
                                                        & trialSet$adviceType==adviceTypes$AiU)])
  df$aiuAgreeInfluence.sd <- sd(trialSet$influence[which(trialSet$agree==1
                                                        & trialSet$adviceType==adviceTypes$AiU)])
  # Influence of the agree-in-confidence advisor on choice agreement trials
  df$aicAgreeChoiceInfluence <- mean(trialSet$influence[which(trialSet$agree==1
                                                              & trialSet$adviceType==adviceTypes$AiC
                                                              & trialSet$hasChoice)])
  df$aicAgreeChoiceInfluence.sd <- sd(trialSet$influence[which(trialSet$agree==1
                                                              & trialSet$adviceType==adviceTypes$AiC
                                                              & trialSet$hasChoice)])
  # Influence of the agree-in-uncertainty advisor on choice agreement trials
  df$aiuAgreeChoiceInfluence <- mean(trialSet$influence[which(trialSet$agree==1
                                                              & trialSet$adviceType==adviceTypes$AiU
                                                              & trialSet$hasChoice)])
  df$aiuAgreeChoiceInfluence.sd <- sd(trialSet$influence[which(trialSet$agree==1
                                                              & trialSet$adviceType==adviceTypes$AiU
                                                              & trialSet$hasChoice)])
  # Influence of the agree-in-confidence advisor on forced agreement trials
  df$aicAgreeForcedInfluence <- mean(trialSet$influence[which(trialSet$agree==1
                                                              & trialSet$adviceType==adviceTypes$AiC
                                                              & !trialSet$hasChoice)])
  df$aicAgreeForcedInfluence.sd <- sd(trialSet$influence[which(trialSet$agree==1
                                                              & trialSet$adviceType==adviceTypes$AiC
                                                              & !trialSet$hasChoice)])
  # Influence of the agree-in-uncertainty advisor on choice agreement trials
  df$aiuAgreeForcedInfluence <- mean(trialSet$influence[which(trialSet$agree==1
                                                              & trialSet$adviceType==adviceTypes$AiU
                                                              & !trialSet$hasChoice)])
  df$aiuAgreeForcedInfluence.sd <- sd(trialSet$influence[which(trialSet$agree==1
                                                              & trialSet$adviceType==adviceTypes$AiU
                                                              & !trialSet$hasChoice)])
  # Influence of the agree-in-confidence advisor on disagreement trials
  df$aicDisagreeInfluence <- mean(trialSet$influence[which(trialSet$agree==0
                                                           & trialSet$adviceType==adviceTypes$AiC)])
  df$aicDisagreeInfluence.sd <- sd(trialSet$influence[which(trialSet$agree==0
                                                           & trialSet$adviceType==adviceTypes$AiC)])
  # Influence of the agree-in-uncertainty advisor on disagreement trials
  df$aiuDisagreeInfluence <- mean(trialSet$influence[which(trialSet$agree==0
                                                           & trialSet$adviceType==adviceTypes$AiU)])
  df$aiuDisagreeInfluence.sd <- sd(trialSet$influence[which(trialSet$agree==0
                                                           & trialSet$adviceType==adviceTypes$AiU)])
  # Influence of the agree-in-confidence advisor on choice disagreement trials
  df$aicDisagreeChoiceInfluence <- mean(trialSet$influence[which(trialSet$agree==0
                                                                 & trialSet$adviceType==adviceTypes$AiC
                                                                 & trialSet$hasChoice)])
  df$aicDisagreeChoiceInfluence.sd <- sd(trialSet$influence[which(trialSet$agree==0
                                                                 & trialSet$adviceType==adviceTypes$AiC
                                                                 & trialSet$hasChoice)])
  # Influence of the agree-in-uncertainty advisor on choice disagreement trials
  df$aiuDisagreeChoiceInfluence <- mean(trialSet$influence[which(trialSet$agree==0
                                                                 & trialSet$adviceType==adviceTypes$AiU
                                                                 & trialSet$hasChoice)])
  df$aiuDisagreeChoiceInfluence.sd <- sd(trialSet$influence[which(trialSet$agree==0
                                                                 & trialSet$adviceType==adviceTypes$AiU
                                                                 & trialSet$hasChoice)])
  # Influence of the agree-in-confidence advisor on forced disagreement trials
  df$aicDisagreeForcedInfluence <- mean(trialSet$influence[which(trialSet$agree==0
                                                                 & trialSet$adviceType==adviceTypes$AiC
                                                                 & !trialSet$hasChoice)])
  df$aicDisagreeForcedInfluence.sd <- sd(trialSet$influence[which(trialSet$agree==0
                                                                 & trialSet$adviceType==adviceTypes$AiC
                                                                 & !trialSet$hasChoice)])
  # Influence of the agree-in-uncertainty advisor on choice disagreement trials
  df$aiuDisagreeForcedInfluence <- mean(trialSet$influence[which(trialSet$agree==0
                                                                 & trialSet$adviceType==adviceTypes$AiU
                                                                 & !trialSet$hasChoice)])
  df$aiuDisagreeForcedInfluence.sd <- sd(trialSet$influence[which(trialSet$agree==0
                                                                 & trialSet$adviceType==adviceTypes$AiU
                                                                 & !trialSet$hasChoice)])
  # Influence of both advisors combined on medium confidence trials
  df$influence.medConf <- mean(medSet$influence)
  df$influence.medConf.sd <- sd(medSet$influence)
  # Influence of the agree-in-confidence advisor on medium confidence trials
  df$aicInfluence.medConf <- mean(medSet$influence[which(medSet$adviceType==adviceTypes$AiC)])
  df$aicInfluence.medConf.sd <- sd(medSet$influence[which(medSet$adviceType==adviceTypes$AiC)])
  # Influence of the agree-in-uncertainty advisor on medium confidence trials
  df$aiuInfluence.medConf <- mean(medSet$influence[which(medSet$adviceType==adviceTypes$AiU)])
  df$aiuInfluence.medConf.sd <- sd(medSet$influence[which(medSet$adviceType==adviceTypes$AiU)])
  # Influence of both advisors combined on medium confidence forced trials
  df$forcedInfluence.medConf <- mean(medSet$influence[which(!medSet$hasChoice)])
  df$forcedInfluence.medConf.sd <- sd(medSet$influence[which(!medSet$hasChoice)])
  # Influence of both advisors combined on medium confidence choice trials
  df$choiceInfluence.medConf <- mean(medSet$influence[which(medSet$hasChoice)])
  df$choiceInfluence.medConf.sd <- sd(medSet$influence[which(medSet$hasChoice)])
  # Influence of agree-in-confidence advisor on medium confidence forced trials
  df$aicForcedInfluence.medConf <- mean(medSet$influence[which(medSet$adviceType==adviceTypes$AiC
                                                      & !medSet$hasChoice)])
  df$aicForcedInfluence.medConf.sd <- sd(medSet$influence[which(medSet$adviceType==adviceTypes$AiC
                                                       & !medSet$hasChoice)])
  # Influence of the agree-in-uncertinaty advisor on medium confidence forced trials
  df$aiuForcedInfluence.medConf <- mean(medSet$influence[which(medSet$adviceType==adviceTypes$AiU
                                                      & !medSet$hasChoice)])
  df$aiuForcedInfluence.medConf.sd <- sd(medSet$influence[which(medSet$adviceType==adviceTypes$AiU
                                                       & !medSet$hasChoice)])
  # Influence of the agree-in-confidence advisor on medium confidence choice trials
  df$aicChoiceInfluence.medConf <- mean(medSet$influence[which(medSet$adviceType==adviceTypes$AiU
                                                      & medSet$hasChoice)])
  df$aicChoiceInfluence.medConf.sd <- sd(medSet$influence[which(medSet$adviceType==adviceTypes$AiU
                                                       & medSet$hasChoice)])
  # Influence of the agree-in-uncertainty advisor on medium confidence choice trials
  df$aiuChoiceInfluence.medConf <- mean(medSet$influence[which(medSet$adviceType==adviceTypes$AiU
                                                      & medSet$hasChoice)])
  df$aiuChoiceInfluence.medConf.sd <- sd(medSet$influence[which(medSet$adviceType==adviceTypes$AiU
                                                       & medSet$hasChoice)])
  # Influence of the agree-in-confidence advisor on medium confidence agreement trials
  df$aicAgreeInfluence.medConf <- mean(medSet$influence[which(medSet$agree==1
                                                        & medSet$adviceType==adviceTypes$AiC)])
  df$aicAgreeInfluence.medConf.sd <- sd(medSet$influence[which(medSet$agree==1
                                                         & medSet$adviceType==adviceTypes$AiC)])
  # Influence of the agree-in-uncertainty advisor on medium confidence agreement trials
  df$aiuAgreeInfluence.medConf <- mean(medSet$influence[which(medSet$agree==1
                                                        & medSet$adviceType==adviceTypes$AiU)])
  df$aiuAgreeInfluence.medConf.sd <- sd(medSet$influence[which(medSet$agree==1
                                                         & medSet$adviceType==adviceTypes$AiU)])
  # Influence of the agree-in-confidence advisor on choice medium confidence agreement trials
  df$aicAgreeChoiceInfluence.medConf <- mean(medSet$influence[which(medSet$agree==1
                                                              & medSet$adviceType==adviceTypes$AiC
                                                              & medSet$hasChoice)])
  df$aicAgreeChoiceInfluence.medConf.sd <- sd(medSet$influence[which(medSet$agree==1
                                                               & medSet$adviceType==adviceTypes$AiC
                                                               & medSet$hasChoice)])
  # Influence of the agree-in-uncertainty advisor on choice medium confidence agreement trials
  df$aiuAgreeChoiceInfluence.medConf <- mean(medSet$influence[which(medSet$agree==1
                                                              & medSet$adviceType==adviceTypes$AiU
                                                              & medSet$hasChoice)])
  df$aiuAgreeChoiceInfluence.medConf.sd <- sd(medSet$influence[which(medSet$agree==1
                                                               & medSet$adviceType==adviceTypes$AiU
                                                               & medSet$hasChoice)])
  # Influence of the agree-in-confidence advisor on forced medium confidence agreement trials
  df$aicAgreeForcedInfluence.medConf <- mean(medSet$influence[which(medSet$agree==1
                                                              & medSet$adviceType==adviceTypes$AiC
                                                              & !medSet$hasChoice)])
  df$aicAgreeForcedInfluence.medConf.sd <- sd(medSet$influence[which(medSet$agree==1
                                                               & medSet$adviceType==adviceTypes$AiC
                                                               & !medSet$hasChoice)])
  # Influence of the agree-in-uncertainty advisor on choice agreement trials
  df$aiuAgreeForcedInfluence.medConf <- mean(medSet$influence[which(medSet$agree==1
                                                              & medSet$adviceType==adviceTypes$AiU
                                                              & !medSet$hasChoice)])
  df$aiuAgreeForcedInfluence.medConf.sd <- sd(medSet$influence[which(medSet$agree==1
                                                               & medSet$adviceType==adviceTypes$AiU
                                                               & !medSet$hasChoice)])
  # Influence of the agree-in-confidence advisor on medium confidence disagreement trials
  df$aicDisagreeInfluence.medConf <- mean(medSet$influence[which(medSet$agree==0
                                                           & medSet$adviceType==adviceTypes$AiC)])
  df$aicDisagreeInfluence.medConf.sd <- sd(medSet$influence[which(medSet$agree==0
                                                            & medSet$adviceType==adviceTypes$AiC)])
  # Influence of the agree-in-uncertainty advisor on medium confidence disagreement trials
  df$aiuDisagreeInfluence.medConf <- mean(medSet$influence[which(medSet$agree==0
                                                           & medSet$adviceType==adviceTypes$AiU)])
  df$aiuDisagreeInfluence.medConf.sd <- sd(medSet$influence[which(medSet$agree==0
                                                            & medSet$adviceType==adviceTypes$AiU)])
  # Influence of the agree-in-confidence advisor on choice medium confidence disagreement trials
  df$aicDisagreeChoiceInfluence.medConf <- mean(medSet$influence[which(medSet$agree==0
                                                                 & medSet$adviceType==adviceTypes$AiC
                                                                 & medSet$hasChoice)])
  df$aicDisagreeChoiceInfluence.medConf.sd <- sd(medSet$influence[which(medSet$agree==0
                                                                  & medSet$adviceType==adviceTypes$AiC
                                                                  & medSet$hasChoice)])
  # Influence of the agree-in-uncertainty advisor on choice medium confidence disagreement trials
  df$aiuDisagreeChoiceInfluence.medConf <- mean(medSet$influence[which(medSet$agree==0
                                                                 & medSet$adviceType==adviceTypes$AiU
                                                                 & medSet$hasChoice)])
  df$aiuDisagreeChoiceInfluence.medConf.sd <- sd(medSet$influence[which(medSet$agree==0
                                                                  & medSet$adviceType==adviceTypes$AiU
                                                                  & medSet$hasChoice)])
  # Influence of the agree-in-confidence advisor on forced medium confidence disagreement trials
  df$aicDisagreeForcedInfluence.medConf <- mean(medSet$influence[which(medSet$agree==0
                                                                 & medSet$adviceType==adviceTypes$AiC
                                                                 & !medSet$hasChoice)])
  df$aicDisagreeForcedInfluence.medConf.sd <- sd(medSet$influence[which(medSet$agree==0
                                                                  & medSet$adviceType==adviceTypes$AiC
                                                                  & !medSet$hasChoice)])
  # Influence of the agree-in-uncertainty advisor on choice medium confidence disagreement trials
  df$aiuDisagreeForcedInfluence.medConf <- mean(medSet$influence[which(medSet$agree==0
                                                                 & medSet$adviceType==adviceTypes$AiU
                                                                 & !medSet$hasChoice)])
  df$aiuDisagreeForcedInfluence.medConf.sd <- sd(medSet$influence[which(medSet$agree==0
                                                                  & medSet$adviceType==adviceTypes$AiU
                                                                  & !medSet$hasChoice)])
  if(is.null(suffix))
    return(df)
  suffix <- paste0('.', suffix)
  names(df) <- paste0(names(df), suffix)
  return(df)
}

## iii) Prepare data ##############################################################################
print('## Prepare data ##################################################################')
# We'll do most of the work with the 'participants' table.
# This table will need some derived stats such as means for trials under various conditions

# First, remove practice trials from the main trials table
all.trials <- trials
trials <- trials[trials$practice==0,]
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


# backup the full participant list and reframe the original to honour exclusions
all.participants <- participants
participants <- participants[participants$excluded==F,]
# Drop excluded participant data from the long table
all.participants.influence <- participants.influence
participants.influence <- participants.influence[which(!participants.influence$participantId 
                                                        %in% which(participants$excluded)),] 
all.participants.influence.medConf <- participants.influence.medConf
participants.influence.medConf <- participants.influence.medConf[which(!participants.influence.medConf$participantId
                                                                        %in% which(participants$excluded)),]
print(paste('>>Excluded participant count:', length(which(all.participants$excluded==T))))

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

print('ANOVA by-participants (not pre-registered) medium confidence trials only')
tmp <- participants.influence.medConf
tmp$value[which(is.nan(tmp$value))] <- 0.00
anova.influence.medConf <- ezANOVA(data = participants.influence.medConf[
  which(!is.nan(participants.influence.medConf$value)),],
                                   dv = value, 
                                   wid = participantId,
                                   within = c('AiC', 'agree', 'hasChoice'),
                                   return_aov = T)
print('>>(anova.influence.medConf)')
anova.influence.medConf$ANOVA


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

questionnaires$answer <- as.numeric(questionnaires$answer)
questionnaires$questionId <- as.numeric(questionnaires$questionId)
questionnaires$questionNumber <- as.numeric(questionnaires$questionNumber)
# Look up advisorType and put it in the table for ease of reference
questionnaires$adviceType <- vector(length=dim(questionnaires)[1])
for(q in 1:dim(questionnaires)[1]) {
  questionnaires$adviceType[q] <- advisors$advice.type[advisors$id==questionnaires$advisorId[q] 
                                                       & advisors$participantId==questionnaires$participantId[p]]
}

# We can get a quick overview from an anova looking for main effect of advisor
# and interactions of timepoint and advisor
print('>>(trust.test) ANOVA exploring trust questionnaire responses')
trust.test <- aov(formula = answer ~ adviceType * timePoint * questionNumber + Error(participantId),
                  data = questionnaires)
summary(trust.test)
EtaSq(trust.test, type = 1)
# Bayesian version
# print('Bayesian ANOVA')
# trust.test.b <- anovaBF(formula = answer ~ adviceType * timePoint * questionId + Error(participantId),
#                         data = questionnaires)
# print(trust.test.b)
print('Marginal means')
marginal.means('answer', c('adviceType', 'timePoint', 'questionNumber'), questionnaires)

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
print('## 5) Do participants simply prefer agreement? ###################################')

# If so, we should see that participants preferentially pick agree-in-confidence
# advisor when their initial confidence is high, and agreee-in-uncertainty when
# their initial confidence is low. We can t-test aic pick proportion in
# high-confidence vs aic pick proportion in low-confidence.

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


# --NON-Preregistered stuff----------------------------------------------------------------------------------
print('--NON Preregistered stuff-----------------------------------------------------------------------------')


## 6) Descriptives
print('## 6) Descriptives ###############################################################')

figPath = 'G:/Documents/University/Google Drive/Project Documents/AdvisorChoice/figures/'

# Participant accuracy
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

# which advisor did they pick?
# This matches pre-registered analysis 1: differential pick rates
graph.pickRate <- ggplot(participants, aes(x = "", y = aic.pick.proportion)) +
  geom_hline(linetype = "dashed", color = "black", yintercept = .5) +
  geom_point(alpha = 0.5) +
  stat_summary(geom = "errorbar",
               fun.data = "mean_cl_boot",
               width = 0.1) +
  stat_summary(geom = "point",
               fun.y = "mean",
               shape = 23, fill = "white", size = 4) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0)) +
  theme_light() +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Advisor preference",
       subtitle = paste(strwrap(paste("Proportion of the time each participant picked the agree-in-confidence advisor.",
                                      "Points indicate data from a single participant, while the diamond indicates the",
                                      "mean proportion across all participants.", 
                                      "Error bars give 95% bootstrapped confidence intervals.", 
                                      sep = " "), 
                                width=115), collapse = "\n"),
       legend = NULL,
       x = NULL,
       y = "Proportion of the time the agree-in-confidence advisor is chosen") 
graph.pickRate
ggsave(paste0(figPath, "basic pick rate.png"), plot = graph.pickRate)

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

# Can look at some marginal means graphs supporting the influence ANOVA
graph.influence.advisor <- ggplot(
  participants.influence[which(participants.influence$variable %in% c('aic.influence', 'aiu.influence')),], 
  aes(x = variable, y = value)
  ) +
  geom_violin(color = NA, fill = '#EEEEEE') +
  geom_point(alpha = 0.5) +
  geom_line(aes(group = participantId), alpha = .5) +
  stat_summary(geom = "errorbar",
               fun.data = "mean_cl_boot",
               width = 0.1) +
  stat_summary(geom = "point",
               fun.y = "mean",
               shape = 23, fill = "white", size = 4) +
  scale_x_discrete(labels = c('Agree in confidence', 'Agree in uncertainty')) +
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
graph.influence.trialType <- ggplot(
  participants.influence[which(participants.influence$variable %in% c('forced.influence', 'choice.influence')),], 
  aes(x = variable, y = value)
  ) +
  geom_violin(color = NA, fill = '#EEEEEE') +
  geom_point(alpha = 0.5) +
  geom_line(aes(group = participantId), alpha = .5) +
  stat_summary(geom = "errorbar",
               fun.data = "mean_cl_boot",
               width = 0.1) +
  stat_summary(geom = "point",
               fun.y = "mean",
               shape = 23, fill = "white", size = 4) +
  scale_x_discrete(labels = c('Forced trials', 'Choice trials')) +
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
  geom_split_violin(aes(fill = forced), color = NA, alpha = 0.2) +
  geom_point(alpha = 0.5, aes(fill = forced, color = forced)) +
  geom_line(data = participants.influence[which(participants.influence$forced==T),],
            aes(group = participantId, color = forced), alpha = .5) +
  geom_line(data = participants.influence[which(participants.influence$forced==F),],
            aes(group = participantId, color = forced), alpha = .5) +
  stat_summary(geom = "errorbar",
               fun.data = "mean_cl_boot",
               width = 0.1) +
  stat_summary(geom = "point",
               aes(fill = forced),
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

# Plot cj1 vs cj2 faceted by dis/agreement
graph.confidence <- ggplot(trials[which(!is.nan(trials$agree)),], aes(x = cj1, y = cj2)) +
  geom_point(alpha = 0.2, aes(color = factor(cor2))) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1, color = 'black') +
  scale_color_discrete(name = 'Final judgement', labels = c('Incorrect', 'Correct')) +
  theme_light() +
  theme(panel.spacing = unit(2, 'lines')) +
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
                                      "judge's final decision was correct or incorrect.",
                                      sep = " "),
                                width=115), collapse = "\n"),
       legend = NULL,
       x = 'Initial confidence',
       y = "Final confidence")
graph.confidence
ggsave(paste0(figPath, "confidence autocorrelation.png"), plot = graph.confidence)

# Does early experience of agreement colour judgement for the rest of the experiment?
for(p in 1:dim(participants)[1]) {
  participants$aic.agreement.block1[p] <- length(which(trials$agree==1
                                                       & trials$participantId==participants$participantId[p]
                                                       & trials$block==3
                                                       & trials$advisorId==adviceTypes$AiC)) / 
    length(which(trials$block==3 & trials$participantId==participants$participantId[p] 
                 & trials$advisorId==adviceTypes$AiC))
  participants$aiu.agreement.block1[p] <- length(which(trials$agree==1
                                                       & trials$participantId==participants$participantId[p]
                                                       & trials$block==3
                                                       & trials$advisorId==adviceTypes$AiU)) / 
    length(which(trials$block==3 & trials$participantId==participants$participantId[p] 
                 & trials$advisorId==adviceTypes$AiU))
}
participants$aic.agreement.block1.superiority <- participants$aic.agreement.block1 - participants$aiu.agreement.block1

equation <- lm(aic.pick.proportion ~ aic.agreement.block1.superiority, data = participants)
equation.text <- paste0('y = ', round(coef(equation)[[1]],2), ' + ', round(coef(equation)[[2]],2), 'x')
graph.block1Agreement.preference <- ggplot(participants, 
                                           aes(x=aic.agreement.block1.superiority, y=aic.pick.proportion)) +
  geom_point(alpha = 0.2) +
  scale_y_continuous(limits = c(0,1)) + 
  scale_x_continuous(limits = c(-0.5,0.5), expand = c(0,0)) + 
  geom_smooth(method='lm', formula = y~x, fullrange = T, level = .99, 
              linetype="dashed", color="black", fill="#CCCCCC") +
  geom_text(label = equation.text, x = .38, y = .54) +
  theme_light() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = '#CCCCCC', size = 0.5)) +
  labs(title = "High weighting of early experience of advisors",
       subtitle = paste(strwrap(paste("Relationship between the initial agreement rate of the agree-in-confidence",
                                      "advisor relative to the agree-in-uncertainty advisor and the preference ",
                                      "for picking the agree-in-confidence advisor.",
                                      "Dashed line shows the best-fit regression line with shaded 99% confidence ",
                                      "intervals.", 
                                      sep = " "), 
                                width=115), collapse = "\n"),
       legend = NULL,
       x = "Block 1 agree-in-confidence agreement superiority",
       y = "Agree-in-confidence pick proportion") 
graph.block1Agreement.preference
ggsave(paste0(figPath, "preference by block1 agreement.png"), plot = graph.block1Agreement.preference)

# Interestingly Agree-in-confidence agreement at block 1 is a better predictor of agree-in-confidence
# preference overall than Agree-in-confidence agreement superiority
equation <- lm(aic.pick.proportion ~ aic.agreement.block1, data = participants)
equation.text <- paste0('y = ', round(coef(equation)[[1]],2), ' + ', round(coef(equation)[[2]],2), 'x')
graph.block1Agreement.preference.aic <- ggplot(participants, 
                                           aes(x=aic.agreement.block1, y=aic.pick.proportion)) +
  geom_point(alpha = 0.2) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) + 
  scale_x_continuous(limits = c(0,1), expand = c(0,0)) + 
  geom_smooth(method='lm', formula = y~x, fullrange = T,
              level = .99, linetype="dashed", color="black", fill="#CCCCCC") +
  geom_text(label = equation.text, x = .85, y = .6) +
  theme_light() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = '#CCCCCC', size = 0.5)) +
  labs(title = "High weighting of early experience of advisors",
       subtitle = paste(strwrap(paste("Relationship between the initial agreement rate of the agree-in-confidence",
                                      "advisor and the pick proportion of that advisor.",
                                      "Dashed line shows the best-fit regression line with shaded 99% confidence ",
                                      "intervals.", 
                                      sep = " "), 
                                width=115), collapse = "\n"),
       legend = NULL,
       x = "Block 1 agree-in-confidence agreement",
       y = "Agree-in-confidence pick proportion") 
graph.block1Agreement.preference.aic
ggsave(paste0(figPath, "preference by raw block1 agreement.png"), plot = graph.block1Agreement.preference.aic)

# Third side to the triangle is that AiU agreement in block 1 doesn't predict 1-aic.pick.rate
# This is very unusual since picking aic vs aiu is a 0-sum game. This suggests that the AiC advisor
# has a much higher weighting on potential picking, and that the performance of the AiU advisor
# is essentially ignored.
equation <- lm((1-aic.pick.proportion) ~ aiu.agreement.block1, data = participants)
equation.text <- paste0('y = ', round(coef(equation)[[1]],2), ' + ', round(coef(equation)[[2]],2), 'x')
graph.block1Agreement.preference.aiu <- ggplot(participants, 
                                               aes(x=aiu.agreement.block1, y=(1-aic.pick.proportion))) +
  geom_point(alpha = 0.2) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) + 
  scale_x_continuous(limits = c(0,1), expand = c(0,0)) + 
  geom_smooth(method='lm', formula = y~x, fullrange = T,
              level = .99, linetype="dashed", color="black", fill="#CCCCCC") +
  geom_text(label = equation.text, x = .85, y = .6) +
  theme_light() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = '#CCCCCC', size = 0.5)) +
  labs(title = "Negligible weighting of early experience of agree-in-uncertainty advisor",
       subtitle = paste(strwrap(paste("Relationship between the initial agreement rate of the agree-in-uncertainty",
                                      "advisor and the pick proportion of that advisor.",
                                      "Dashed line shows the best-fit regression line with shaded 99% confidence ",
                                      "intervals.", 
                                      sep = " "), 
                                width=115), collapse = "\n"),
       legend = NULL,
       x = "Block 1 agree-in-uncertainty agreement",
       y = "(1 - Agree-in-confidence pick proportion)") 
graph.block1Agreement.preference.aiu
ggsave(paste0(figPath, "preference by raw block1 agreement aiu.png"), plot = graph.block1Agreement.preference.aiu)

write.csv(participants, 'participants.csv')
# write.csv(trials, 'trials.csv')
write.csv(participants.influence, 'participants-influence.csv')
