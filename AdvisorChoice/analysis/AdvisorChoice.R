#####
# Analysis script for AdvisorChoice experiment data
# Matt Jaquiery, Jan 2018 (matt.jaquiery@psy.ox.ac.uk)
#
# i) Get Data
# ii) Function definitions - could move to another file
# 1) Demographics
# 2) Is the agree-in-confidence advisor selected more often?
# 3) Was the agree-in-confidence advisor more influential?
# 


## i) Get Data ####################################################################################

# Use the mat2R functions to get hold of some data and do some manipulations
if(!exists("getMatlabData", mode="function")) source("mat2R.R")

acPth <- "C:/Users/mj221/Filr/My Files/Results/AdvisorChoice"
acPth <- "D:/Users/MJ/Filr/My Files/Results/AdvisorChoice"

study <- getMatlabData(acPth)  # get some data from the path defined for convenience in mat2R

## ii) Some functions #############################################################################

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

## 1) Demographics ################################################################################
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

## 2) Is the agree-in-confidence advisor selected more often? ###################################### 

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

## 3) Was the agree-in-confidence advisor more influential? #######################################

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

# Calculate the influence of the advisor on each trial
trial_influence <- vector('list',length(study))
for(p in seq(length(study))) {
  p_data <- data.frame('id'=integer(), 'direction'=integer(), 'influence'=integer())
  trials <- study[[p]]$trials
  for(t in seq(dim(trials)[1])) {
    # store the index of this trial in the master trial list so we can save to the master list later
    tid <- which(as.numeric(study[[p]]$trials[,"id"])==as.numeric(trials[t,"id"]))
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

## TODO: check this part!!!
# Calculate the influence of each advisor
advisor_influence <- data.frame("pId"=integer(),
                                "aic_influence_mean"=double(),
                                "aic_influence_sd"=double(),
                                "aiu_influence_mean"=double(),
                                "aiu_influence_sd"=double())
advisor_influence_details <- vector('list', length(study))
advisor_influence_tests <- vector('list', length(study))
for(p in seq(length(study))) {
  p_data <- study[[p]]
  trials <- p_data$trials[which(p_data$trials[,"practice"]==FALSE),] # exclude practice trials
  adviceTypes <- c(1,2)
  a_influence <- vector('list',2)
  for(adviceType in adviceTypes) {
    # find the advisorId for this adviceType
    advisorIds <- as.numeric(p_data$cfg$advisor[seq(1,length(p_data$cfg$advisor),6)])
    advisorAdviceTypes <- as.numeric(p_data$cfg$advisor[seq(1,length(p_data$cfg$advisor),6)])
    advisorId <- advisorIds[which(advisorAdviceTypes==adviceType)] # ID of the advisor
    # find the influence on this advisor's trials
    a_trials <- which(as.numeric(trials[,"advisorId"])==as.numeric(advisorId))
    a_influence[[adviceType]] <- trial_influence[[p]][as.numeric(trials[a_trials,"id"]),"influence"]
  }
  advisor_influence_details[[p]] <- a_influence
  advisor_influence_test[[p]] <- t.test(a_influence[1],a_influence[2])
  t_data <- data.frame('pId'=p,
                       'aic_influence_mean'=mean(a_influence[1]),
                       'aic_influence_sd'=sd(a_influence[1]),
                       'aiu_influence_mean'=mean(a_influence[2]),
                       'aiu_influence_sd'=sd(a_influence[2]))
  advisor_influence <- rbind(advisor_influence, t_data)
}
