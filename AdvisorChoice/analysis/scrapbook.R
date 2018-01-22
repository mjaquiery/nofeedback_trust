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