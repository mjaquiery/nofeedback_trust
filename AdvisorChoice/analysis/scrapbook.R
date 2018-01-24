## Example analysis which produces summary info for each participant

processed_data <- list()

# Process each participant
for (p_num in seq(length(study))) {
  p_all <- study[[p_num]]$trials  # a single participant
  # extract real trials
  if ('practice' %in% names(p_all)) {
    p <- p_all[which(p_all[,"practice"]!=1),]
  } else {
    p <- p_all[which(p_all[,"id"] > study[[1]]$cfg$practice$block.count * study[[1]]$cfg$practice$trial.count),]
  } 
  names(p[1,]) # fields in each trial
  s <- p[,1:6]  # a subset of their data
  s <- numerify(s)  # numerified for convenience
  m <- colMeans(s)  # summary stats
  
  #block3 <- which(p[,"block"]==3)
  #b3 <- numerify(p[block3,1:6])
  
  ## Analyse data by advisor type and by whether choice of advisor was forced
  # Get rid of NaNs which mess up which calls (replace with -1)
  p[,"advisorId"] <- lapply(p[,"advisorId"],function(x) {ifelse(is.nan(x),-1,x)})
  advs <- as.numeric(unique(p[,"advisorId"]))
  advs <- advs[order(advs)]
  clabels <- c("advisorId",
               "forced",
               "total trials", 
               "proportion initially correct", 
               "proportion finally correct", 
               "mean initial confidence", 
               "sdev initial confidence",
               "mean final confidence", 
               "sdev final confidence",
               "mean shift on agreement",
               "sdev shift on agreement",
               "mean shift on disagreement",
               "sdev shift on disagreement")
  if(exists('res'))
    rm(res)
  for (a in advs) {
    tlist <- p[which(as.numeric(p[,"advisorId"])==a),]
    # split into forced and choice trials
    for (forced in c(TRUE,FALSE,'ALL')) {
      trials <- getTrialsByForcedState(tlist, forced)
      if(length(trials)==0)
        next
      n <- length(trials[,1])
      pcor_i <- length(which(trials[,"cor1"]==1))/n
      pcor_f <- length(which(trials[,"cor2"]==1))/n
      conf_i <- as.numeric(trials[,"cj1"])
      conf_i_m <- mean(conf_i)
      conf_i_s <- sd(conf_i)
      conf_f <- as.numeric(trials[,"cj2"])
      conf_f_m <- mean(conf_f)
      conf_f_s <- sd(conf_f)
      # the trial-outcome-specific stuff might need case-adjusting by trial outcome
      trials_agree <- trials[which(trials[,"agree"]==1),]
      shift_a <- as.numeric(trials_agree[,"cj1"]) - as.numeric(trials_agree[,"cj2"])
      shift_a_m <- mean(shift_a)
      shift_a_s <- sd(shift_a)
      trials_disagree <- trials[which(trials[,"agree"]==0),]
      shift_d <- as.numeric(trials_disagree[,"cj1"]) - as.numeric(trials_disagree[,"cj2"])
      shift_d_m <- mean(shift_d)
      shift_d_s <- sd(shift_d)
      drow <- data.frame(a, forced, n, pcor_i, pcor_f, conf_i_m, conf_i_s, conf_f_m, conf_f_s, shift_a_m, shift_a_s, shift_d_m, shift_d_s)
      names(drow) <- clabels
      if(exists('res')) {
        res <- rbind(res, drow)
      } else {
        res <- drow
      }
    }
  }
  processed_data[[p_num]] <- res
}

## 3) Was the agree-in-confidence advisor more influential? #######################################
print('## 3) TEST Advisor influence #####################################################')
#we calculate the influence of the agree-in-confidence and agree-in-uncertainty
#advisors separately. We store the mean influence of each advisor for each
#participant. For interest, we can calculate for each individual participant
#whether the advisors are significantly different in their influence. For the
#key analysis, of course, we are comparing the mean influence of each advisor
#over all participants. Calculate the influence of each advisor
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
print('>>(influence_by_advisor_b) bayesian test of mean influence: aic versus aiu')
influence_by_advisor_b <- ttestBF(advisor_influence$aic_influence_mean - advisor_influence$aiu_influence_mean)
print(influence_by_advisor_b)
print(paste0('Evidence strength in favour of AiC != AiU: BF=', round(exp(influence_by_advisor_b@bayesFactor$bf),3)))
