# Load MATLAB data and reformat it to be easily used
# - matt.jaquiery@psy.ox.ac.uk
#
# Requires R.matlab
#
# Addressing default MATLAB data objects looks like:
# a$cfg[,,1]$advisors[,,1]$count[,,1]$real We fix that by dimension reduction
# and add in a transpose for the trials
#
# Returns a list out with three parts: out$cfg - configuration data accessed
# with out$cfg$property out$subject - details concerning the subject's specific
# profile (age, handedness, file location, etc.) out$trials - trial details
# accessed with out$trials[trialNum,"property"] or
# out$trials[trialNum,]$property
#
# Overall this fits into a schema whereby a master list of experimental sessions
# (i.e. subjects) holds details of the configuration, subject, and trials
# e.g. to get the value for 'fieldName' for a particular trial from subject 'subjectNum':
# experiments[subjectNum]$trials[trialNum,]$fieldName
# e.g. to get a vector of 'fieldName' values for all trials:
# experiments[subjectNum]$trials[,"fieldName"]
# 

if(!require("R.matlab"))
  install.packages("R.matlab")
library(R.matlab)

# pth <- "C:/Users/mj221/Filr/My Files/Results/AdvisorChoicetest/behaviour/2017-11-22T153220_999_final.mat"
# pth <- "C:/Users/mj221/Filr/My Files/Results/PoliticalDifferences"
pth <- "G:/Documents/University/Google Drive/Results/AdvisorChoice"

# crawl a directory and return a list of participants' MATLAB data
getMatlabData <- function (folder) {
  print(paste('getMatlabData:', folder))
  files <- list.files(folder, pattern = '*_final_R.mat$', full.names = T, recursive = T)
  out <- list()
  for (i in seq(length(files))) {
    filename <- files[[i]]
    print(paste('mat2R:', filename))
    out[[i]] <- mat2R(filename)
  } 
  
  out <- frameMess(out)
  return(out)
}

mat2R <- function (matpath) {
  matdata <- readMat(matpath)
  dataout <- restructure(matdata)
  return(dataout)
}

# Turns the MATLAB object and returns a more appropriately formatted set of data structures
restructure <- function (listIn) {
  outList <- list()
  outList$cfg <- clean_list(listIn$cfg)
  outList$subject <- clean_list(listIn$subject)
  outList$trials <- clean_trials(listIn$trials)
  
  # Need to rearrange trial data so variables are columns and trials are rows
  #outList$trials <- t(outList$trials)
  
  return(outList)
}

# Recursively clean a list of unnecessary dimensions
clean_list <- function (listIn) {
  
  if(!is.list(listIn) || length(listIn)==0) {
    # it's not a list, just hand it back
    return(drop(listIn))
  }
  
  d <- dim(listIn)
  if(!is.null(d) && 1 %in% d) {
    # drop if we've extraneous dimensions
    return(clean_list(drop(listIn)))
  }
  
  if(length(listIn)==1 && (is.null(names(listIn)) || as.character(names(listIn)[[1]])=="")) {
    # 1-item lists should be reduced
    return(clean_list(listIn[[1]]))
  }
  
  for(i in seq(length(listIn))) {
    if(i>length(listIn)) {
      # catch cases where the list has been truncated under our feet
      break
    }
    # a little bit of recursion never hurt anyone...
    listIn[[i]] <- clean_list(listIn[[i]])
  }
  
  return(listIn)
}

# Produces a neatly formatted list of trials such that we get data out by:
# ..$trials[trialNum]$fieldName
clean_trials <- function(trialList) {
  outList <- drop(trialList)
  outList <- t(outList)
  return(outList)
}

numerify <- function(df, nanBecomes = NaN) {
  nf <- sapply(df, as.numeric)
  nf[which(is.nan(nf))] = nanBecomes
  dim(nf) <- dim(df)
  colnames(nf) <- colnames(df)
  rownames(nf) <- rownames(df)
  return(nf)
}

# Current study format is matlab data sorted into multiple lists.
# A neater approach is to exchange lists for data frames, and specifically
# having a data frame for trials, advisors, and participants. 
# IDs tie things together, and participantId is of particular relevance for 
# fetching values across tables.
frameMess <- function(raw_study) {
  participants.all <- NULL
  trials.all <- NULL
  advisors.all <- NULL
  questionnaires.all <- NULL
  for(p in 1:length(raw_study)) {
    # participant data
    pId <- ifelse(length(raw_study[[p]]$subject$id)!=1, p, raw_study[[p]]$subject$id)
    name <- ifelse(length(raw_study[[p]]$subject$name)!=1, NA, raw_study[[p]]$subject$name)
    gender <- ifelse(length(raw_study[[p]]$subject$gender)!=1, NA, raw_study[[p]]$subject$gender)
    age <- ifelse(length(raw_study[[p]]$subject$age)!=1, NA, raw_study[[p]]$subject$age)
    restarted <- ifelse(length(raw_study[[p]]$subject$restarted)!=1, NA, raw_study[[p]]$subject$restarted)
    date <- ifelse(length(raw_study[[p]]$subject$date)!=1, NA, raw_study[[p]]$subject$date)
    start.time.year <- ifelse(length(raw_study[[p]]$subject$start.time)==0, NA, raw_study[[p]]$subject$start.time[[1]])
    start.time.month <- ifelse(length(raw_study[[p]]$subject$start.time)==0, NA, raw_study[[p]]$subject$start.time[[2]])
    start.time.day <- ifelse(length(raw_study[[p]]$subject$start.time)==0, NA, raw_study[[p]]$subject$start.time[[3]])
    start.time.hour <- ifelse(length(raw_study[[p]]$subject$start.time)==0, NA, raw_study[[p]]$subject$start.time[[4]])
    start.time.minute <- ifelse(length(raw_study[[p]]$subject$start.time)==0, NA, raw_study[[p]]$subject$start.time[[5]])
    start.time.second <- ifelse(length(raw_study[[p]]$subject$start.time)==0, NA, raw_study[[p]]$subject$start.time[[6]])
    screen <- ifelse(length(raw_study[[p]]$subject$screen)!=1, NA, raw_study[[p]]$subject$screen)
    computer <- ifelse(length(raw_study[[p]]$cfg$computer)!=1, NA, raw_study[[p]]$cfg$computer)
    os <- ifelse(length(raw_study[[p]]$cfg$os)!=1, NA, raw_study[[p]]$cfg$os)
    debug <- ifelse(is.null(raw_study[[p]]$cfg$debug), NA, raw_study[[p]]$cfg$debug)
    short.mode <- ifelse(is.null(raw_study[[p]]$cfg$shortMode), NA, raw_study[[p]]$cfg$shortMode)
    stimulus.response.interval <- ifelse(is.null(raw_study[[p]]$cfg$stim$SRI1), NA, raw_study[[p]]$cfg$stim$SRI1)
    response.stimulus.interval.1 <- ifelse(is.null(raw_study[[p]]$cfg$stim$RSI1), NA, raw_study[[p]]$cfg$stim$RSI1)
    response.stimulus.interval.2 <- ifelse(is.null(raw_study[[p]]$cfg$stim$RSI2), NA, raw_study[[p]]$cfg$stim$RSI2)
    initial.dot.difference <- ifelse(is.null(raw_study[[p]]$cfg$stim$initialDotDifference), 
                                     NA, 
                                     raw_study[[p]]$cfg$stim$initialDotDifference)
    fixation.pre.flicker.time <- ifelse(is.null(raw_study[[p]]$cfg$stim$fixationFlicker$time$pre),
                                        NA,
                                        raw_study[[p]]$cfg$stim$fixationFlicker$time$pre)
    fixation.flicker.time <- ifelse(is.null(raw_study[[p]]$cfg$stim$fixationFlicker$time$duration),
                                    NA,
                                    raw_study[[p]]$cfg$stim$fixationFlicker$time$duration)
    fixation.post.flicker.time <- ifelse(is.null(raw_study[[p]]$cfg$stim$fixationFlicker$time$post),
                                         NA,
                                         raw_study[[p]]$cfg$stim$fixationFlicker$time$post)
    rng.code <- ifelse(is.null(raw_study[[p]]$cfg$resetrn),
                       NA,
                       raw_study[[p]]$cfg$resetrn)
    participant <- data.frame(participantId = p, nominalParticipantId = pId, name, gender, age, restarted, date, 
                              start.time.year, start.time.month, start.time.day, 
                              start.time.hour, start.time.minute, start.time.second, 
                              screen, computer, os, 
                              debug,  # should never be true
                              short.mode,  # should never be true
                              stimulus.response.interval, # time between stimulus and response enabling
                              response.stimulus.interval.1, # time between response and advice choice
                              response.stimulus.interval.2, # time between advice and second response
                              initial.dot.difference,
                              fixation.pre.flicker.time, # fixation initial offset
                              fixation.flicker.time, # fixation flicker onset
                              fixation.post.flicker.time, # fixation temporary offset
                              rng.code)
    participants.all <- rbind(participants.all, participant)
    
    # advisor data
    advisor <- raw_study[[p]]$cfg$advisor
    for(a in seq(1,length(advisor),6)) {
      advisor.df <- data.frame(participantId = p,
                               id = advisor[[a]],
                               advice.type = advisor[[a+1]],
                               portrait = advisor[[a+2]],
                               voice = advisor[[a+3]],
                               name = advisor[[a+4]])
      advisors.all <- rbind(advisors.all, advisor.df)
    }
    
    # trials
    trials <- as.data.frame(raw_study[[p]]$trials)
    # subset for testing!
    #trials <- trials[,1:10]
    trials <- cbind(data.frame(participantId=p), trials)
    trials.all <- rbind(trials.all, trials)
    
    # questionnaires
    qtrials <- trials[trials$questionnaire==1,]
    for(q in 1:dim(qtrials)[1]) {
      qs <- qtrials[q,"qanswers"][[1]]
      # split the questionnaire list into 8 questions with 9 fields apiece
      dim(qs) <- c(9,8)
      qs <- as.data.frame(t(qs))
      names(qs) <- c('questionId', 'advisorId', 'questionNumber', 'answer', 'initialPosition',
                     'presentationOrder', 'hasChanged', 'responseTime', 'onsetTime')
      qs$participantId <- p
      qs$timePoint <- q
      qs <- qs[,order(c(10,11,2,1,3:9))]
      questionnaires.all <- rbind(questionnaires.all, qs)
    }
  }
  out <- list(participants=participants.all, advisors=advisors.all, trials=trials.all, questionnaires=questionnaires.all)
}


# # Make stuff into numeric if possible
# typecastData <- function(datum) {
#   if(length(datum)==0)
#     return(datum)
#   if(length(datum)==1) {
#     x <- as.numeric(datum)
#     if(is.na(x))
#       return(datum) 
#     return(x)
#   }
#   out <- list()
#   for (i in seq(length(datum))) {
#     out[[length(out)+1]]<- typecastData(datum[[i]])
#   }
#   return(out)
# }