## Analysis script for AdvisorChoice experiment data ##############################################
# Matt Jaquiery, Jan 2018 (matt.jaquiery@psy.ox.ac.uk)
#
# i) Get Data
# ii) Function definitions - could move to another file
# 1) Demographics
# 2) Is the agree-in-confidence advisor selected more often?
# 3) Was the agree-in-confidence advisor more influential?
# 4) ANOVA investigating influence


## i) Get Data ####################################################################################
print('Loading data')

# Use the mat2R functions to get hold of some data and do some manipulations
if(!exists("getMatlabData", mode="function")) source("mat2R.R")

acPth <- "C:/Users/mj221/Filr/My Files/Results/AdvisorChoice"
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

## 3) Was the agree-in-confidence advisor more influential? #######################################
print('## 3) TEST Advisor influence #####################################################')

#Influence is defined as the extent to which the judge's (participant's) final
#decision has moved from their initial decision in the direction of the advice
#received.

# We begin by calculating influence for all trials and saving that information
# since it will come in handy for looking at influence on subsets of trials
# later. Next we calculate the influence of the agree-in-confidence and
# agree-in-uncertainty advisors separately. We store the mean influence of each
# advisor for each participant. For interest, we can calculate for each
# individual participant whether the advisors are significantly different in
# their influence. For the key analysis, of course, we are comparing the mean
# influence of each advisor over all participants.

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

print('Influence by advisor')

# Calculate the influence of each advisor
advisor_influence <- data.frame("pId"=integer(),
                                "aic_influence_mean"=double(),
                                "aic_influence_sd"=double(),
                                "aiu_influence_mean"=double(),
                                "aiu_influence_sd"=double())
advisor_influence_details <- vector('list', length(study))
advisor_influence_test <- vector('list', length(study))
for(p in seq(length(study))) {
  p_data <- study[[p]]
  trials <- p_data$trials[which(p_data$trials[,"practice"]==FALSE),] # exclude practice trials
  adviceTypes <- c(1,2)
  # advisors don't have $id tags so we have to use sequences to extract the key vars
  advisorIds <- as.numeric(p_data$cfg$advisor[seq(1,length(p_data$cfg$advisor),6)])
  advisorAdviceTypes <- as.numeric(p_data$cfg$advisor[seq(2,length(p_data$cfg$advisor),6)])
  a_influence <- vector('list',2)
  for(adviceType in adviceTypes) {
    # find the advisorId for this adviceType
    advisorId <- advisorIds[which(advisorAdviceTypes==adviceType)] # ID of the advisor
    # find the influence on this advisor's trials
    a_trials <- which(as.numeric(trials[,"advisorId"])==as.numeric(advisorId))
    a_trial_ids <- as.numeric(trials[a_trials,"id"])
    a_influence[[adviceType]] <- trial_influence[[p]][which(trial_influence[[p]][,"id"]%in%a_trial_ids),"influence"]
  }
  advisor_influence_details[[p]] <- a_influence
  advisor_influence_test[[p]] <- t.test(a_influence[[1]],a_influence[[2]])
  t_data <- data.frame('pId'=p,
                       'aic_influence_mean'=mean(a_influence[[1]]),
                       'aic_influence_sd'=sd(a_influence[[1]]),
                       'aiu_influence_mean'=mean(a_influence[[2]]),
                       'aiu_influence_sd'=sd(a_influence[[2]]))
  advisor_influence <- rbind(advisor_influence, t_data)
}

influence_by_advisor <- t.test(advisor_influence$aic_influence_mean,
                               advisor_influence$aiu_influence_mean, 
                               paired = T)

print('>>(influence_by_advisor) t-test of mean influence: agree-in-confidence versus agree-in-uncertainty')
prettyPrint(influence_by_advisor)

## 4) ANOVA investigating influence ###############################################################
print('## 4) ANOVA investigating influence ##############################################')
# 2x2x2 ANOVA investigating effects of advisor type
# (agree-in-confidence/uncertainty), choice (un/forced), and agreement
# (dis/agree) on influence. These are all within-subjects manipulations.

# First prepare the data by setting a data frame with the relevant information
# (the participant ID, the factors, and the mean influence for each of those
# factor combination).
anova_data <- data.frame(pId=integer(),
                         advice=integer(),
                         choice=integer(),
                         agreement=integer(),
                         influence=double(),
                         n=integer())
advisorTypes <- c(1,2) # 1=aic, 2=aiu
choiceTypes <- c(0,1) # 0=forced, 1=unforced
agreementTypes <- c(0,1) # 0=disagree, 1=agree
for(p in seq(length(study))) {
  p_data <- study[[p]]
  trials <- p_data$trials
  trials <- trials[which(trials[,"practice"]==FALSE),]
  advisorIds <- as.numeric(p_data$cfg$advisor[seq(1,length(p_data$cfg$advisor),6)])
  advisorAdviceTypes <- as.numeric(p_data$cfg$advisor[seq(2,length(p_data$cfg$advisor),6)])
  # for each advisor type
  for(aT in advisorTypes) {
    trials_A <- trials[which(trials[,"advisorId"]==advisorIds[which(advisorAdviceTypes==aT)]),]
    # for each choice type
    for(cT in choiceTypes) {
      # using ...mean>1 here is a huge HACK. It works because the no choice is
      # represented by 0. So the expected combinations (ignoring NaNs) are 1,0;
      # 2,0; and 2,1 which average to .5, 1, and 1.5 respectively. Since we only
      # care about the 2,1 case (a genuine choice between advisors), the hack
      # works.
      trials_C <- trials_A[which((as.numeric(lapply(trials_A[,"choice"],mean))>1)==cT),]
      # for each agreement type
      for(agT in agreementTypes) {
        trials_G <- trials_C[which(trials_C[,"agree"]==agT),]
        # calculate the influence and record the value
        mean_influence <- 
          mean(trial_influence[[p]][which(trial_influence[[p]][,"id"]%in%trials_G[,"id"]),"influence"])
        r_data <- data.frame(pId=p, 
                             advice=aT, 
                             choice=cT, 
                             agreement=agT, 
                             influence=mean_influence, 
                             n=dim(trials_G)[1])
        anova_data <- rbind(anova_data, r_data)
      }
    }
  }
}
# now we can run the anova. Let's build the model:
anova_output <- aov(formula = influence ~ advice * choice * agreement + Error(pId), data=anova_data)
print('>>(anova_output)')
print(summary(anova_output))
