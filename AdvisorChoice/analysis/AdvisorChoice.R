## Analysis script for AdvisorChoice experiment data ##############################################
# Matt Jaquiery, Jan 2018 (matt.jaquiery@psy.ox.ac.uk)
#
# i) Get Data
# ii) Function definitions - could move to another file
# 1) Demographics
# 2) Is the agree-in-confidence advisor selected more often?
# 3) ANOVA investigating influence
# 4) Trust questionnaire answers
#   i. Trust for each advisor
# 5) Do participants simply prefer agreement?

## Citations 

if(!require('BayesFactor')) {
  #Richard D. Morey and Jeffrey N. Rouder (2015). BayesFactor: Computation of
  #Bayes Factors for Common Designs. R package version 0.9.12-2. 
  install.packages('BayesFactor')
}
library(BayesFactor)

## i) Get Data ####################################################################################
print('Loading data')

# Use the mat2R functions to get hold of some data and do some manipulations
if(!exists("getMatlabData", mode="function")) {
  oldwd <- getwd()
  setwd(dirname(sys.frame(1)$ofile))
  source("mat2R.R")
  setwd(oldwd)
} 


acPth <- "C:/Users/mj221/Filr/My Files/Results/AdvisorChoice"
#acPth <- "D:/Users/MJ/Filr/My Files/Results/AdvisorChoice"

raw_study <- getMatlabData(acPth)  # get some data from the path defined for convenience in mat2R

## ii) Some functions #############################################################################
print('Defining functions')

# Return the subset of trials which are forced trials (unforced if extractForced=FALSE)
getTrialsByForcedState <- function(trials, extractForced = TRUE) {
  if(extractForced == 'ALL')
    return(trials)
  out <- list()
  for(i in seq(dim(trials)[[1]])) {
    lst <- trials[[i,"choice"]] # choice offered to participant
    if( (length(which(lst==0))>0) == extractForced)
      out <- c(as.numeric(out),i)
  }
  #print(paste(dim(trials)[[1]], ' <- ', length(out)))
  if(length(out)<1)
    return(list())
  else
    return(trials[out,])
}

# Print the results of a t-test as we would like to see them reported in a paper
prettyPrint <- function(results) {
  print(paste0('t(',results$parameter,')=',round(results$statistic,2),
               ' [',round(attr(results$conf.int, "conf.level")*100),'%CI: ',
               round(results$conf.int[[1]],2), ', ', round(results$conf.int[[2]],2),'],',
               ' p=',round(aic_test$p.value,3)))
}

## 0) Exclusions ##################################################################################
print('## 0) Running exclusions #########################################################')
# Exclusion rules:
# Proportion of correct initial judgements must be (.60 < cor1/n < .90) 
#NB:practice trials are INCLUDED in this since they are used in part for
#determining confidence calibration
exclusions <- list()
for(p in seq(length(raw_study))) {
  trials <- raw_study[[p]]$trials
  n <- dim(trials)[1]
  cor1 <- length(which(trials[,"cor1"]==1))
  proportion <- cor1/n
  if(proportion < .6 || proportion > .9) {
    print(paste("Excluding participant",p,"with proportion correct of", round(proportion,2)))
    exclusions <- c(as.numeric(exclusions),p)
  } 
}
# copy valid data to a new variable
study <- vector('list',length(raw_study)-length(exclusions))
i <- 1
for(p in seq(length(raw_study))) {
  if(!p %in% exclusions) {
    study[[i]] <- raw_study[[p]]
    i <- i+1
  }
}
print(paste('>>Excluded participant count:', length(exclusions)))

## 1) Demographics ################################################################################
print('## 1) Demographics ###############################################################')

ages <- list()
genders <- list()
for(p in seq(length(study))) {
  s <- study[[p]]$subject
  ages <- c(as.numeric(ages), s$age)
  genders <- c(as.character(genders), s$gender)
}
demographics <- data.frame('N'=length(study),
                           'age_mean'=mean(ages),
                           'age_sd'=sd(ages),
                           'males'=length(which(genders=='m')),
                           'females'=length(which(genders=='f')),
                           'other_gender'=length(which(genders!='m' && genders!='f')))
print(str(demographics))

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

aic_advice_type <- 1 # AdviceType code for the agree-in-confidence advisor
proportions <- list()
for(p in seq(length(study))) {
  p_data <- study[[p]]
  trials <- p_data$trials[which(p_data$trials[,"practice"]==FALSE),] # exclude practice trials
  trials <- getTrialsByForcedState(trials, FALSE) # only want unforced (i.e. choice) trials
  # the process for extracting IDs for advisors is slightly tortured: the
  # compression function seems to drop the identifiers so we have to go by entry
  # order. For reference, the columns are: id, adviceType, pic, voice, name,
  # questionnaireDone
  advisorIds <- as.numeric(p_data$cfg$advisor[seq(1,length(p_data$cfg$advisor),6)])
  advisorAdviceTypes <- as.numeric(p_data$cfg$advisor[seq(1,length(p_data$cfg$advisor),6)])
  aic_advisor <- advisorIds[which(advisorAdviceTypes==aic_advice_type)] # ID of the agree-in-confidence advisor
  
  # Extract only the trials with this advisor
  aic_trials <- which(trials[,"advisorId"]==aic_advisor)
  aic_proportion <- length(aic_trials)/dim(trials)[1]
  proportions <- c(as.numeric(proportions), aic_proportion)
}
aic_selection <- data.frame('mean' = mean(proportions), 'sd' = sd(proportions), row.names = 'proportions')

aic_test <- t.test(proportions, mu=0.5) # testing the proportions versus the null hypothesis of 0.5 (chance selection)
print('>>(aic_test) choice proportion Agree-in-confidence vs. chance level (.5)')
prettyPrint(aic_test)
print('>>(aic_test_b) bayesian examination of above (prior = mean of 0.5, sd as empirically observed)')
aic_test_b <- ttestBF(proportions, mu = 0.5)
print(aic_test_b)
print(paste0('Evidence strength for preferential AiC picking: BF=', round(exp(aic_test_b@bayesFactor$bf),3)))

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
trial_influence <- data.frame(pId=integer(),
                              tId=integer(),
                              block=integer(), 
                              advisorId=integer(), 
                              adviceType=integer(), #  1=Agree-in-confidence; 2=Agree-in-uncertainty
                              choiceAllowed=integer(), #  F=forced; T=choice
                              agree=integer(), #  F=disagree; T=agree
                              initialAnswerRight=integer(), #  F=left; T=right
                              initialConfidenceCategory=integer(), # whether initial answer was low/med/high confidence
                              shift=integer(),  #  amount the confidence changes
                              influence=integer()) #  amount the confidence changes in the direction of the advice
for(p in seq(length(study))) {
  # skip practice trials
  trials <- study[[p]]$trials[which(study[[p]]$trials[,"practice"]==FALSE),]
  advisorIds <- as.numeric(study[[p]]$cfg$advisor[seq(1,length(study[[p]]$cfg$advisor),6)])
  advisorAdviceTypes <- as.numeric(study[[p]]$cfg$advisor[seq(2,length(study[[p]]$cfg$advisor),6)])
  for(t in seq(dim(trials)[1])) {
    # store basic trial information 
    tid <- trials[t, "id"]
    aT <- advisorAdviceTypes[as.numeric(trials[t,"advisorId"])]
    cA <- lapply(trials[t,"choice"],mean)>1
    t_data <- data.frame(pId=p,
                         tId=tid,
                         block=trials[t,"block"],
                         advisorId=trials[t,"advisorId"],
                         adviceType=aT,
                         choiceAllowed=cA,
                         agree=trials[t,"agree"],
                         initialAnswerRight=trials[t,"cj1"]>0,
                         initialConfidenceCategory=NA)
    if(is.nan(trials[[t,"advisorId"]])) {
      # trials without an advisor are entered as NA
      t_data$shift <- NA
      t_data$influence <- NA
    } else {
      # calculate the confidence change on this trial ('shift')
      # shift = Cpost - Cpre
      # C-values are *-1 if answer was 'left', so
      # Cpre [1,55]
      # Cpost [-55,-1]U[1,55]
      cj1 <- trials[[t,"cj1"]]
      cj2 <- trials[[t,"cj2"]]
      if(trials[[t,"cj1"]]<0) {
        # initial response is 'left'
        cj1 <- cj1 * -1
        cj2 <- cj2 * -1
      } 
      shift <- cj2 - cj1 # +ve values indicate shift towards more confidence in initial response
      if((trials[[t,"agree"]])) {
        # on agreement trials shift towards initial response inidicates following advice
        influence <- shift
      } else {
        # on disagreement trials shift AWAY from initial response inidicates following advice
        influence <- shift * -1
      }
      t_data$shift <- shift
      t_data$influence <- influence
      # for correct trials record the confidence 'step'
      if(trials[[t,"cor1"]]) {
        t_data$initialConfidenceCategory <- trials[t,"step"]
      }
    }
    trial_influence <- rbind(trial_influence, t_data)
  }
}

# 2x2x2 ANOVA investigating effects of advisor type
# (agree-in-confidence/uncertainty), choice (un/forced), and agreement
# (dis/agree) on influence. These are all within-subjects manipulations.
print('Running ANOVAs')
anova_output <- aov(formula = influence ~ adviceType * choiceAllowed * agree + Error(pId), data=trial_influence)
print('>>(anova_output)')
print(summary(anova_output))

# The bias-sharing advisor and anti-bias advisors differ in their frequency with
# which they agree with the participant as a  function of participant confidence
# (by design). To control for background effects where people are influenced
# different amounts depending on their own initial confidence, we also look at
# only those trials where participant confidence was in the mid-range (i.e.
# where both advisors agree 70% of the time, and thus where agreement and
# confidence balance out). This subset only includes trials on which the
# participant was CORRECT since the step information is not recorded for
# incorrect trials (where all advisors agree 30% of the time).
trial_influence_70 <- trial_influence[which(trial_influence$initialConfidenceCategory==0),]
anova_output_70 <- aov(formula = influence ~ adviceType * choiceAllowed * agree + Error(pId), data=trial_influence_70)
print('>>(anova_output_70) Looking at only trials where intial decision was correct and made with middle confidence:')
print(summary(anova_output_70))

## 4) Trust questionnaire answers #################################################################
#   i. Trust for each advisor

# We want to know if trust for each advisor changes over time. First we build a
# table for each participant and each of the questionnaire answers.
trust_table <- data.frame(pId=integer(),
                          timepoint=integer(),
                          trialNum=integer(),
                          advisorId=integer(),
                          advisorNth=integer(),
                          answer=integer(),
                          responseTime=double(),
                          qNum=integer(),
                          qNth=integer(),
                          qText=character())
for(p in seq(length(study))) {
  trials <- study[[p]]$trials
  qtrials <- trials[which(trials[,"questionnaire"]==1),]
  if(is.null(dim(qtrials))) {
    # 1 or 0 entries in qtrials (questionnaire only taken once)
    if(length(qtrials)>0) {
      q_data <- as.data.frame(t(as.data.frame(qtrials$qanswers)))
      tId <- qtrials$id
      q_text <- study[[p]]$cfg$instr$Q$q$pro$text[as.numeric(q_data[,"quest"])]
      for(i in seq(dim(q_data)[1])) {
        q_data_in <- data.frame(pId=p, timepoint=1, trialNum=qtrials$id, 
                                advisorId=q_data$obs[[i]], advisorNth=1, 
                                answer=q_data$ans[[i]], responseTime=q_data$response.t[[i]],
                                qNum=q_data$quest[[i]], qNth=q_data$presentation.order[[i]], 
                                qText=q_text[[i]])
        trust_table <- rbind(trust_table, q_data_in)
      }
    }
  } else {
    for(t in seq(dim(qtrials)[1])) {
      q_data <- as.data.frame(t(as.data.frame(qtrials[t,"qanswers"]$qanswers)))
      tId <- as.numeric(qtrials[,"id"])
      q_text <- study[[p]]$cfg$instr$Q$q$pro$text[as.numeric(q_data$quest)]
      for(i in seq(dim(q_data)[1])) {
        if(dim(trust_table)[1]==0)
          atp <- 1
        else
          atp <- 1+floor(length(which(trust_table$advisorId==as.numeric(q_data$obs[[i]])))/4)
        if(atp>1)
          q_text <- study[[p]]$cfg$instr$Q$q$retro$text[as.numeric(q_data[,"quest"])]
        else
          q_text <- study[[p]]$cfg$instr$Q$q$pro$text[as.numeric(q_data[,"quest"])]
        q_data_in <- data.frame(pId=p, timepoint=t, trialNum=tId[[t]], 
                                advisorId=q_data$obs[[i]], advisorNth=atp, 
                                answer=q_data$ans[[i]], responseTime=q_data$response.t[[i]],
                                qNum=q_data$quest[[i]], qNth=q_data$presentation.order[[i]], 
                                qText=q_text[[i]])
        trust_table <- rbind(trust_table, q_data_in)
      }
    }
  }
}

## 5) Do participants simply prefer agreement? ####################################################

# If so, we should see that participants preferentially pick agree-in-confidence
# advisor when their initial confidence is high, and agreee-in-uncertainty when
# their initial confidence is low. We can t-test aic pick proportion in
# high-confidence vs aic pick proportion in low-confidence.

