# Modelling trials required to understand an advisor's worth ####

## Data and libraries ####
library(tidyverse)
library(reshape2)

# Check the workspace has data
if(!exists('participants') | !exists('advisors') | !exists('trials'))
  stop('Experiment data missing from workspace, run AdvisorChoice.R before continuing.')

## Average difficulty ####
for(i in 1:dim(participants)[1]) {
  t <- as.numeric(trials$dotdifference[which(trials$participantId==i
                                             & trials$practice==F)])
  # Checking the distribution with hist(t) will show if participant is behaving normally
  participants$meanDotDifference[i] <- mean(t)
  participants$sdDotDifference[i] <- sd(t)
  # Calculate mode
  ut <- unique(t)
  participants$modeDotDifference[i] <- ut[which.max(tabulate(match(t,ut)))]
}

ggplot(participants, aes(meanDotDifference)) +
  geom_histogram(binwidth = 1) + 
  annotate(geom='text', label=paste('mean =', round(mean(participants$meanDotDifference),2)), x=10, y=5.2) +
  annotate(geom='text', label=paste('sd =', round(sd(participants$meanDotDifference),2)), x=10, y=4.8) +
  theme_light() +
  labs(title = 'Mean dot difference for each participant')

## Evolution of preference over time ####
# plot the favouritism trajectory for each participant
tmp <- melt(participants.byBlock, id.vars = c('participantId'), 
            measure.vars = names(participants.byBlock)[grep('aicPickRate.block', 
                                                            names(participants.byBlock), 
                                                            fixed=T)],
            value.name = 'aicPickRate', variable.name = 'block')
tmp$block <- as.numeric(sub('aicPickRate.block', '', tmp$block, fixed = T))

ggplot(tmp, aes(x=block, y=aicPickRate, color=as.factor(participantId))) + 
  geom_smooth(se=F)

## Plotting experienced agreement by trial ####

# For each advisor and confidence level, put together a track of observed mean
# agreement across participants
suppressWarnings(rm('cumulativeAgreement'))
for(a in c(adviceTypes$AiC, adviceTypes$AiU)) {
  for(c in confidenceTypes) {
    v <- data.frame(trialId=integer(), agree=double(), n=integer(), cumAgree=double())
    for(t in unique(as.numeric(trials$id))) {
      tmp <- trials$agree[which(trials$id==t & trials$advisorId==a & trials$step==c)]
      s <- sum(tmp, na.rm = T)
      n <- length(tmp)
      if(is.finite(s/n)) {
        # add to the cumulative total by weighting
        v <- rbind(v, data.frame(trialId = t,
                                 agree=s,
                                 n,
                                 cumAgree = 0))
        v$cumAgree[length(v$cumAgree)] <- (sum(v$agree)+s) / (n+sum(v$n))
      }
    }
    v$conf <- getConfidenceTypeName(c)
    v$advisor <- adviceTypeShortNames[[a]]
    if(exists('cumulativeAgreement'))
      cumulativeAgreement <- rbind(cumulativeAgreement,v)
    else
      cumulativeAgreement <- v
  }
}

# Now we can plot that data
tmp <- cumulativeAgreement
tmp$trialId <- tmp$trialId - min(tmp$trialId)
tmp <- tmp[which(tmp$trialId < 100),]
ggplot(tmp, aes(x=trialId, y=cumAgree, color=conf)) + 
  geom_line() + 
  facet_wrap(~ advisor) +
  theme_light() + 
  theme(legend.position = 'bottom',
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_y_continuous(expand = c(0,0)) +
  ylab('cumulative agreement')

## Exploring stability of confidence scale ####
suppressWarnings(rm('confidenceBins'))
for(t in unique(as.numeric(trials$id))) {
  df <- data.frame(trialId = integer(), conf = integer(), participantId = integer(), stat=factor(), value = double())
  for(c in confidenceTypes) {
    v <- data.frame(participantId = integer(), n = integer())
    for(participantId in participants$participantId) {
      v <- rbind(v, data.frame(participantId, n = length(which(trials$id<=t & trials$step==c
                                                               & trials$participantId==participantId))))
    }
    df <- rbind(df, data.frame(trialId=t, conf=c, participantId = NA,
                               stat=as.factor('mean'), value=mean(v$n, na.rm = T)))
    v <- v[which(v$n==min(v$n, na.rm = T)),]
    df <- rbind(df, data.frame(trialId = t, 
                               conf = c, 
                               participantId = v$participantId[1],
                               stat = as.factor('min'),
                               value = v$n[1]))
  }
  if(exists('confidenceBins')) {
    confidenceBins <- rbind(confidenceBins, df)
  }
  else {
    confidenceBins <- df
  }
}
tmp <- confidenceBins
tmp$trialId <- tmp$trialId - min(tmp$trialId)
tmp$conf <- as.factor(tmp$conf)
ggplot(tmp, aes(x=trialId, y=value, color=conf, linetype=stat)) +
  geom_abline(slope = 0.4*0.7, intercept = 0, color = 'black', linetype = 'dashed', size = 2) +
  geom_abline(slope = 0.3*0.7, intercept = 0, color = 'black', linetype = 'dashed', size = 2) +
  geom_line(size = 1) +
  ylab('trials') +
  xlab('trial number') +
  theme_light() +
  scale_x_continuous(breaks = seq(0,325,25), expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0))
