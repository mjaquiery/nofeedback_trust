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


#acPth <- "C:/Users/mj221/Filr/My Files/Results/AdvisorChoice"
acPth <- "D:/Users/MJ/Filr/My Files/Results/AdvisorChoice"

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

# TODO: change this to match Niccolo's definiton of influence/sway

print('Calculating influence on each trial')
# Calculate the influence of the advisor on each trial
trial_influence <- vector('list',length(study))
for(p in seq(length(study))) {
  p_data <- data.frame('id'=integer(), 'direction'=integer(), 'influence'=integer())
  trials <- study[[p]]$trials
  for(t in seq(dim(trials)[1])) {
    # store the index of this trial in the master trial list
    tid <- trials[t, "id"]
    if(is.nan(trials[[t,"advisorId"]])) {
      # trials without an advisor are entered as NA
      p_data <- rbind(p_data, data.frame(id=tid,direction=NA,influence=NA))
      next 
    }
    # calculate the influence of the advice
    cj1 <- trials[[t,"cj1"]]+55 # rescale from [-55 55] to [0 110]
    cj2 <- trials[[t,"cj2"]]+55
    sway <- cj2 - cj1 # +ve values indicate increasingly right-oriented response
    if((trials[[t,"agree"]] && trials[[t,"cj1"]]<0) ||
       (!trials[[t,"agree"]] && trials[[t,"cj1"]]>=0)) {
      # Advice was 'target is on the left' so we need to reverse the sway
      sway = sway * -1
      direction <- 1
    } else {
      # record the advice direction for posterity
      direction <- 2
    }
    # save this value
    t_data <- data.frame('id'=tid, 'direction'=direction, 'influence'=sway)
    p_data <- rbind(p_data, t_data)
  }
  trial_influence[[p]] <- p_data
}

# 2x2x2 ANOVA investigating effects of advisor type
# (agree-in-confidence/uncertainty), choice (un/forced), and agreement
# (dis/agree) on influence. These are all within-subjects manipulations.

# First prepare the data by setting a data frame with the relevant information
# (the participant ID, the factors, and the mean influence for each of those
# factor combination).

# The bias-sharing advisor and anti-bias advisors differ in their frequency with
# which they agree with the participant as a  function of participant confidence
# (by design). To control for background effects where people are influenced
# different amounts depending on their own initial confidence, we also look at
# only those trials where participant confidence was in the mid-range (i.e.
# where both advisors agree 70% of the time, and thus where agreement and
# confidence balance out). This subset only includes trials on which the
# participant was CORRECT since the step information is not recorded for
# incorrect trials (where all advisors agree 30% of the time).
anova_data <- data.frame(pId=integer(),
                         advice=integer(),
                         choice=integer(),
                         agreement=integer(),
                         influence=double(),
                         n=integer())
anova_data_70 <- anova_data
advisorTypes <- c(1,2) # 1=aic, 2=aiu
choiceTypes <- c(0,1) # 0=forced, 1=unforced
agreementTypes <- c(0,1) # 0=disagree, 1=agree
for(p in seq(length(study))) {
  p_data <- study[[p]]
  trials <- p_data$trials
  trials <- trials[which(trials[,"practice"]==FALSE),]
  # the _70 variables concern only trials where participant was correct with
  # middle confidence
  trials_70 <- trials[which(trials[,"step"]==0),] 
  advisorIds <- as.numeric(p_data$cfg$advisor[seq(1,length(p_data$cfg$advisor),6)])
  advisorAdviceTypes <- as.numeric(p_data$cfg$advisor[seq(2,length(p_data$cfg$advisor),6)])
  # for each advisor type
  for(aT in advisorTypes) {
    trials_A <- trials[which(trials[,"advisorId"]==advisorIds[which(advisorAdviceTypes==aT)]),]
    trials_70_A <- trials_70[which(trials_70[,"advisorId"]==advisorIds[which(advisorAdviceTypes==aT)]),]
    # for each choice type
    for(cT in choiceTypes) {
      # using ...mean>1 here is a huge HACK. It works because the no choice is
      # represented by 0. So the expected combinations (ignoring NaNs) are 1,0;
      # 2,0; and 2,1 which average to .5, 1, and 1.5 respectively. Since we only
      # care about the 2,1 case (a genuine choice between advisors), the hack
      # works.
      trials_C <- trials_A[which((as.numeric(lapply(trials_A[,"choice"],mean))>1)==cT),]
      trials_70_C <- trials_70_A[which((as.numeric(lapply(trials_70_A[,"choice"],mean))>1)==cT),]
      # for each agreement type
      for(agT in agreementTypes) {
        trials_G <- trials_C[which(trials_C[,"agree"]==agT),]
        trials_70_G <- trials_70_C[which(trials_70_C[,"agree"]==agT),]
        # calculate the influence and record the value
        mean_influence <- 
          mean(trial_influence[[p]][which(trial_influence[[p]][,"id"]%in%trials_G[,"id"]),"influence"])
        if(length(trials_70_G)==dim(trials)[2]) {
          # only one entry was found for this case, so mean is just the value
          mean_influence_70 <- trial_influence[[p]][which(trial_influence[[p]][,"id"]==trials_70_G["id"]),"influence"]
          dim(trials_70_G) <- c(1, dim(trials)[2])
        } else {
          mean_influence_70 <- 
            mean(trial_influence[[p]][which(trial_influence[[p]][,"id"]%in%trials_70_G[,"id"]),"influence"])
        }
        r_data <- data.frame(pId=p, 
                             adviceType=aT, 
                             choiceAllowed=cT, 
                             agreement=agT, 
                             influence=mean_influence, 
                             n=dim(trials_G)[1])
        r_data_70 <- data.frame(pId=p,
                                adviceType=aT,
                                choiceAllowed=cT,
                                agreement=agT,
                                influence=mean_influence_70,
                                n=dim(trials_70_G)[1])
        anova_data <- rbind(anova_data, r_data)
        anova_data_70 <- rbind(anova_data_70, r_data_70)
      }
    }
  }
}
# now we can run the anova. Let's build the model:
anova_output <- aov(formula = influence ~ adviceType * choiceAllowed * agreement + Error(pId), data=anova_data)
print('>>(anova_output)')
print(summary(anova_output))
anova_output_70 <- aov(formula = influence ~ adviceType * choiceAllowed * agreement + Error(pId), data=anova_data_70)
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
