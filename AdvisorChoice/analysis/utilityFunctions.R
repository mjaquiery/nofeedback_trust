# Utility functions for analysing AdvisorChoice data

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

printMean <- function(vector, conf.int = .95, na.rm = F, decimals = 2) {
  mu <- mean(vector, na.rm = na.rm)
  s <- sd(vector, na.rm = na.rm)
  n <- length(vector)
  error <- qnorm(1-(1-conf.int)/2)*s/sqrt(n) # 95% confidence interval width
  ci.low <- mu - error
  ci.high <- mu + error
  r <- round(range(vector, na.rm = na.rm), decimals)
  print(paste0('Mean=', round(mu,decimals), ' [', round(conf.int,decimals)*100, '%CI: ',
               round(ci.low,decimals), ', ', round(ci.high,decimals),'] [Range: ',
               r[[1]], ', ', r[[2]], ']'))
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
# return them in a data frame with colnames ending in .suffix.
# These stats are not necessarily complete (e.g. the number of agreement trials
# might be calculated for one contingency but not the number of disagreement trials).
scanTrials <- function(trialSet, suffix = NULL) {
  df <- data.frame(trialCount = dim(trialSet)[1])
  # The medium-confidence trials come in handy, so cache them here
  medSet <- trialSet[which(trialSet$step==confidenceTypes$medium),]
  df$trialCount <- dim(trialSet)[1]
  # Proportion of trials in which the initial decision was correct
  df$proportionCorrect <- length(which(trialSet$cor1==1)) / dim(trialSet)[1]
  # Proportion of trials in which the initial decision was correct and the agree-in-confidence advisor was selected
  df$aicProportionCorrect <- length(which(trialSet$cor1==1 & trialSet$adviceType==adviceTypes$AiC)) /
    length(which(trialSet$adviceType==adviceTypes$AiC))
  # Proportion of trials in which the initial decision was correct and the agree-in-uncertainty advisor was selected
  df$aiuProportionCorrect <- length(which(trialSet$cor1==1 & trialSet$adviceType==adviceTypes$AiU)) /
    length(which(trialSet$adviceType==adviceTypes$AiU))
  # Proportion of trials in which the final decision was correct
  df$proportionCorrectFinal <- length(which(trialSet$cor2==1)) / dim(trialSet)[1]
  # Proportion of trials in which the final decision was correct and the agree-in-confidence advisor was selected
  df$aicProportionCorrectFinal <- length(which(trialSet$cor2==1 & trialSet$adviceType==adviceTypes$AiC)) /
    length(which(trialSet$adviceType==adviceTypes$AiC))
  # Proportion of trials in which the final decision was correct and the agree-in-uncertainty advisor was selected
  df$aiuProportionCorrectFinal <- length(which(trialSet$cor2==1 & trialSet$adviceType==adviceTypes$AiU)) /
    length(which(trialSet$adviceType==adviceTypes$AiU))
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
  # Number of trials on which the agree-in-confidence advisor disagreed
  df$aicDisagreeCount <- length(which(trialSet$agree==0 
                                   & trialSet$adviceType==adviceTypes$AiC))
  # Number of trials on which the agree-in-uncertainty advisor disagreed
  df$aiuDisagreeCount <- length(which(trialSet$agree==0
                                   & trialSet$adviceType==adviceTypes$AiU))
  # Number of (initially) correct trials on which the agree-in-confidence advisor agreed
  df$aicCorrectAgree <- length(which(trialSet$agree==1 
                                     & trialSet$adviceType==adviceTypes$AiC
                                     & trialSet$cor1==1))
  # Number of (initially) correct trials on which the agree-in-uncertianty advisor agreed
  df$aiuCorrectAgree <- length(which(trialSet$agree==1 
                                     & trialSet$adviceType==adviceTypes$AiU
                                     & trialSet$cor1==1))
  # Number of (initially) correct trials on which the agree-in-confidence advisor disagreed
  df$aicCorrectDisagree <- length(which(trialSet$agree==0 
                                        & trialSet$adviceType==adviceTypes$AiC
                                        & trialSet$cor1==1))
  # Number of (initially) correct trials on which the agree-in-uncertainty advisor disagreed
  df$aiuCorrectDisagree <- length(which(trialSet$agree==0 
                                        & trialSet$adviceType==adviceTypes$AiU
                                        & trialSet$cor1==1))
  # Number of (initially) incorrect trials on which the agree-in-confidence advisor agreed
  df$aicIncorrectAgree <- length(which(trialSet$agree==1 
                                     & trialSet$adviceType==adviceTypes$AiC
                                     & trialSet$cor1==0))
  # Number of (initially) incorrect trials on which the agree-in-uncertianty advisor agreed
  df$aiuIncorrectAgree <- length(which(trialSet$agree==1 
                                     & trialSet$adviceType==adviceTypes$AiU
                                     & trialSet$cor1==0))
  # Number of (initially) incorrect trials on which the agree-in-confidence advisor disagreed
  df$aicIncorrectDisagree <- length(which(trialSet$agree==0 
                                        & trialSet$adviceType==adviceTypes$AiC
                                        & trialSet$cor1==0))
  # Number of (initially) incorrect trials on which the agree-in-uncertainty advisor disagreed
  df$aiuIncorrectDisagree <- length(which(trialSet$agree==0 
                                        & trialSet$adviceType==adviceTypes$AiU
                                        & trialSet$cor1==0))
  # Number of choice trials
  df$choiceCount <- length(which(trialSet$hasChoice))
  # Number of forced trials
  df$forcedCount <- length(which(!trialSet$hasChoice))
  
  # Number of choice trials where the advisor agreed
  df$agreeChoice <- length(which(trialSet$agree==1 & trialSet$hasChoice))
  # Number of forced trials where advisor agreed
  df$agreeForced <- length(which(trialSet$agree==1 & !trialSet$hasChoice))
  # Number of choice trials where the advisor disagreed
  df$disagreeChoice <- length(which(trialSet$agree==0 & trialSet$hasChoice))
  # Number of forced trials where advisor disagreed
  df$disagreeForced <- length(which(trialSet$agree==0 & !trialSet$hasChoice))
  # Number of choice trials where the agree-in-confidence advisor agreed
  df$aicAgreeChoice <- length(which(trialSet$agree==1 
                                    & trialSet$adviceType==adviceTypes$AiC
                                    & trialSet$hasChoice))
  # Number of choice trials where the agree-in-uncertainty advisor agreed
  df$aiuAgreeChoice <- length(which(trialSet$agree==1
                                    & trialSet$adviceType==adviceTypes$AiU
                                    & trialSet$hasChoice))
  # Number of choice trials where the agree-in-confidence advisor disagreed
  df$aicDisagreeChoice <- length(which(trialSet$agree==0 
                                    & trialSet$adviceType==adviceTypes$AiC
                                    & trialSet$hasChoice))
  # Number of choice trials where the agree-in-uncertainty advisor disagreed
  df$aiuDisagreeChoice <- length(which(trialSet$agree==0
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
  # Number of low-confidence trials where the agree-in-confidence advisor disagreed
  df$aicDisagree.lowConf <- length(which(trialSet$agree==0
                                      & trialSet$adviceType==adviceTypes$AiC
                                      & trialSet$step == -1))
  # Number of low-confidence trials where the agree-in-uncertainty advisor disgreed
  df$aiuDisagree.lowConf <- length(which(trialSet$agree==0
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
  # Number of med-confidence choice trials where the agree-in-confidence advisor agreed
  df$aicAgreeChoice.medConf <- length(which(trialSet$agree==1
                                            & trialSet$adviceType==adviceTypes$AiC
                                            & trialSet$step == 0
                                            & trialSet$hasChoice == T))
  # Number of med-confidence choice trials where the agree-in-uncertainty advisor agreed
  df$aiuAgreeChoice.medConf <- length(which(trialSet$agree==1
                                            & trialSet$adviceType==adviceTypes$AiU
                                            & trialSet$step == 0
                                            & trialSet$hasChoice == T))
  # Number of med-confidence trials where the agree-in-confidence advisor disagreed
  df$aicDisagree.medConf <- length(which(trialSet$agree==0
                                         & trialSet$adviceType==adviceTypes$AiC
                                         & trialSet$step == 0))
  # Number of med-confidence trials where the agree-in-uncertainty advisor disagreed
  df$aiuDisagree.medConf <- length(which(trialSet$agree==0
                                         & trialSet$adviceType==adviceTypes$AiU
                                         & trialSet$step == 0))
  # Number of med-confidence choice trials where the agree-in-confidence advisor disagreed
  df$aicDisagreeChoice.medConf <- length(which(trialSet$agree==0
                                               & trialSet$adviceType==adviceTypes$AiC
                                               & trialSet$step == 0
                                               & trialSet$hasChoice == T))
  # Number of med-confidence choice trials where the agree-in-uncertainty advisor disagreed
  df$aiuDisagreeChoice.medConf <- length(which(trialSet$agree==0
                                               & trialSet$adviceType==adviceTypes$AiU
                                               & trialSet$step == 0
                                               & trialSet$hasChoice == T))
  # Number of high-confidence trials where the agree-in-confidence advisor agreed
  df$aicAgree.highConf <- length(which(trialSet$agree==1
                                       & trialSet$adviceType==adviceTypes$AiC
                                       & trialSet$step == 1))
  # Number of high-confidence trials where the agree-in-uncertainty advisor agreed
  df$aiuAgree.highConf <- length(which(trialSet$agree==1
                                       & trialSet$adviceType==adviceTypes$AiU
                                       & trialSet$step == 1))
  # Number of high-confidence trials where the agree-in-confidence advisor disagreed
  df$aicDisagree.highConf <- length(which(trialSet$agree==0
                                       & trialSet$adviceType==adviceTypes$AiC
                                       & trialSet$step == 1))
  # Number of high-confidence trials where the agree-in-uncertainty advisor disagreed
  df$aiuDisagree.highConf <- length(which(trialSet$agree==0
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
  
  # Agreement rates calculated from values
  df$aicAgreeRate <- df$aicAgreeCount / (df$aicAgreeCount + df$aicDisagreeCount)
  df$aiuAgreeRate <- df$aiuAgreeCount / (df$aiuAgreeCount + df$aiuDisagreeCount)
  df$aicCorrectAgreeRate <- df$aicCorrectAgree / (df$aicCorrectAgree + df$aicCorrectDisagree)
  df$aiuCorrectAgreeRate <- df$aiuCorrectAgree / (df$aiuCorrectAgree + df$aiuCorrectDisagree)
  df$aicIncorrectAgreeRate <- df$aicIncorrectAgree / (df$aicIncorrectAgree + df$aicIncorrectDisagree)
  df$aiuIncorrectAgreeRate <- df$aiuIncorrectAgree / (df$aiuIncorrectAgree + df$aiuIncorrectDisagree)
  df$aicAgreeRate.highConf <- df$aicAgree.highConf / (df$aicAgree.highConf + df$aicDisagree.highConf)
  df$aiuAgreeRate.highConf <- df$aiuAgree.highConf / (df$aiuAgree.highConf + df$aiuDisagree.highConf)
  df$aicAgreeRate.medConf <- df$aicAgree.medConf / (df$aicAgree.medConf + df$aicDisagree.medConf)
  df$aiuAgreeRate.medConf <- df$aiuAgree.medConf / (df$aiuAgree.medConf + df$aiuDisagree.medConf)
  df$aicAgreeRate.lowConf <- df$aicAgree.lowConf / (df$aicAgree.lowConf + df$aicDisagree.lowConf)
  df$aiuAgreeRate.lowConf <- df$aiuAgree.lowConf / (df$aiuAgree.lowConf + df$aiuDisagree.lowConf)
  
  if(is.null(suffix))
    return(df)
  suffix <- paste0('.', suffix)
  names(df) <- paste0(names(df), suffix)
  return(df)
}

getFavouriteAdviceType <- function(participantId, participant.data.frame = participants, adviceTypeList = adviceTypes) {
  if(participant.data.frame$aicPickRate[which(participant.data.frame$participantId==participantId)] > 0.5)
    return(adviceTypeList$AiC)
  else
    return(adviceTypeList$AiU)
}

lmToStr <- function(eq, coefNames = NULL, roundTo = NULL) {
  coefs <- coef(eq)
  if(is.null(coefNames))
    coefNames <- names(coefs)
  else
    coefNames <- c('NA',coefNames)    # pad supplied coefNames with a value to cover the intercept
  if(!is.null(roundTo))
    coefs <- round(coefs, roundTo)
  coefStr <- ''
  for(i in 2:length(coef(eq))) {
    coefStr = paste0(coefStr, ' + ', coefs[i], coefNames[i])
  }
  out <- paste0('y = ', coefs[1], coefStr)
}
