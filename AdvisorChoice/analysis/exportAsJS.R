# Convert data to align with JavaScript version of the experiment ####
exName <- 'MAT'
export <- list()

# Participants target:
# [1] "id"                     "blockCount"             "catchPerBlock"     "forcePerBlock" "practiceCatchPerBlock" 
# [6] "practiceForcePerBlock"  "practiceChoicePerBlock" "difficultyStep"    "dotCount"      "preTrialInterval"      
# [11] "preStimulusInterval"    "stimulusDuration"       "feedbackDuration" "timeStart"     "timeEnd"               
# [16] "experimentDuration"     "manipulationQuestion"   "debriefComments"  "pid"           "excluded"              
# [21] "experiment"  
tmp <- all.participants
export[["all.participants"]] <- data.frame(id = tmp$participantId,
                                     blockCount = NA,
                                     catchPerBlock = NA,
                                     forcePerBlock = NA,
                                     practiceCatchPerBlock = NA,
                                     practiceForcePerBlock = NA,
                                     practiceChoicePerBlock = NA,
                                     difficultyStep = NA,
                                     dotCount = NA,
                                     preTrialInterval = tmp$fixation.pre.flicker.time,
                                     preStimulusInterval = tmp$fixation.flicker.time + 
                                       tmp$fixation.post.flicker.time,
                                     stimulusDuration = tmp$stimulus.response.interval,
                                     feedbackDuration = NA,
                                     timeStart = NA,
                                     timeEnd = NA,
                                     experimentDuration = NA,
                                     manipulationQuestion = NA,
                                     debriefComments = NA, 
                                     pid = paste0(exName, which(!is.na(tmp$participantId))),
                                     excluded = tmp$excluded,
                                     experiment = exName) 
export[["participants"]] <- export$all.participants[!export$all.participants$excluded, ]

# Advisors target:
# [1] "participantId" "id"  "adviceType"  "name"  "portraitSrc" "voiceId" "pid" "experiment" 
tmp <- all.advisors
export[["all.advisors"]] <- data.frame(id = tmp$id,
                                       participantId = tmp$participantId,
                                       adviceType = tmp$advice.type+2,
                                       name = tmp$name,
                                       portraitSrc = tmp$portrait,
                                       voiceId = tmp$voice,
                                       pid = sapply(tmp$participantId,  
                                                    function(x){export$all.participants$pid[
                                                      export$all.participants$id == x]}),
                                       experiment = exName)
export[["advisors"]] <- export$all.advisors[export$all.advisors$pid %in% export$participants$pid, ]

# Trials target:
# [1] "participantId"         "id"                 "block"                  "practice"               "type"                  
# [6] "typeName"              "dotDifference"      "correctAnswer"          "initialAnswer"          "finalAnswer"           
# [11] "initialConfidence"    "finalConfidence"   "confidenceCategory"     "hasChoice"              "choice0"               
# [16] "choice1"              "advisorId"         "advisorAgrees"          "adviceSide"             "feedback"              
# [21] "warnings"         "timeInitialStart"  "timeInitialFixation"    "timeInitialStimOn"      "timeInitialStimOff"    
# [26] "timeInitialResponse" "adviceString" "durationAdvisorChoice"  "durationAdviceDuration" "timeFinalStart"        
# [31] "timeFinalFixation"    "timeFinalStimOn"   "timeFinalStimOff"       "timeFinalResponse"      "adviceRight"           
# [36] "adviceSideOld"    "advisorAgreesOld"  "adviceType"             "confidenceShift"        "confidenceShiftRaw"    
# [41] "influence"            "rawInfluence"      "switch"                 "initialCorrect"         "finalCorrect"          
# [46] "initialConfSpan"      "finalConfSpan"     "irrational"             "pid"                    "experiment"   
tmp <- all.trials
export[["all.trials"]] <- data.frame(participantId = tmp$participantId,
                                     id = rep(1:nrow(tmp[tmp$participantId==tmp$participantId[1], ]),
                                              length(unique(tmp$participantId))), # MATLAB id is not trial order
                                     block = as.numeric(tmp$block),
                                     practice = as.numeric(tmp$practice),
                                     type = as.numeric(tmp$taskType),
                                     typeName = NA,
                                     dotDifference = as.numeric(tmp$dotdifference),
                                     correctAnswer = as.numeric(tmp$wherelarger)-1,
                                     initialAnswer = as.numeric(tmp$int1)-1,
                                     finalAnswer = as.numeric(tmp$int2)-1,
                                     initialConfidence = abs(as.numeric(tmp$cj1)),
                                     finalConfidence = abs(as.numeric(tmp$cj2)),
                                     confidenceCategory = as.numeric(tmp$step)+1,
                                     hasChoice = sapply(tmp$choice, function(x) ifelse(all(x > 0), T, F)),
                                     choice0 = sapply(tmp$choice, function(x) x[1]),
                                     choice1 = sapply(tmp$choice, function(x) x[2]),
                                     advisorId = as.numeric(tmp$advisorId),
                                     advisorAgrees = as.numeric(tmp$agree),
                                     adviceSide = ifelse(as.numeric(tmp$obsacc), 
                                                         as.numeric(tmp$wherelarger)-1, 
                                                         !(as.numeric(tmp$wherelarger)-1)),
                                     feedback = as.numeric(tmp$feedback),
                                     warnings = NA,
                                     timeInitialStart = as.numeric(tmp$time.startTrial),
                                     timeInitialFixation = NA,
                                     timeInitialStimOn = NA,
                                     timeInitialStimOff = NA,
                                     timeInitialResponse = as.numeric(tmp$time.response1),
                                     adviceString = NA,
                                     durationAdvisorChoice = as.numeric(tmp$choiceTime),
                                     durationAdviceDuration = as.numeric(tmp$time.responseStart2)-
                                       as.numeric(tmp$time.response1),
                                     timeFinalStart = as.numeric(tmp$time.responseStart2),
                                     timeFinalFixation = NA,
                                     timeFinalStimOn = NA,
                                     timeFinalStimOff = NA,
                                     timeFinalResponse = as.numeric(tmp$time.response2),
                                     adviceRight = as.numeric(tmp$obsacc),
                                     adviceSideOld = NA,
                                     advisorAgreesOld = NA,
                                     adviceType = as.numeric(
                                       sapply(1:nrow(tmp), function(i) export$all.advisors$adviceType[
                                       which(export$all.advisors$id == as.numeric(tmp$advisorId[i]) &
                                         export$all.advisors$participantId == tmp$participantId[i])
                                     ])),
                                     confidenceShift = NA,
                                     confidenceShiftRaw = NA,
                                     influence = NA,
                                     rawInfluence = NA,
                                     switch = as.numeric(tmp$int1) == as.numeric(tmp$int2),
                                     initialCorrect = as.numeric(tmp$cor1),
                                     finalCorrect = as.numeric(tmp$cor2),
                                     initialConfSpan = as.numeric(tmp$cj1),
                                     finalConfSpan = as.numeric(tmp$cj2),
                                     irrational = NA,
                                     pid = sapply(tmp$participantId,  
                                                  function(x){export$all.participants$pid[
                                                    export$all.participants$id == x]}),
                                     experiment = exName)
export[["trials"]] <- export$all.trials[export$all.trials$pid %in% export$participants$pid & 
                                          !export$all.trials$practice, ]

# Questionnaires target:
# [1] "participantId"     "advisorId"         "afterTrial"        "timeStart"   "timeResponseStart" "timeEnd"          
# [7] "duration"          "benevolence"       "likeability"       "ability"     "pid"               "adviceType"       
# [13] "timepoint"         "advisorName"       "advisorPortrait"   "advisorAge" "advisorCategory"   "experiment" 

save(export, file = 'jsExport.RData')
