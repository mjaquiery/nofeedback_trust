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
pth <- "C:/Users/mj221/Filr/My Files/Results/AdvisorChoice"

# crawl a directory and return a list of participants' MATLAB data
getMatlabData <- function (folder) {
  print(paste('getMatlabData:', folder))
  out <- list()
  files <- list.files(folder)
  mask <- regexpr('_final_R.mat$', files)>=0
  if(length(mask)>0) {
    for (i in seq(length(files))) {
      if (mask[i]) {
        filename <- paste(folder, files[[i]], sep='/')
        print(paste('mat2R:', filename))
        out[[length(out)+1]] <- c("filename" = files[[i]], mat2R(filename))
        # break  # only accept one _final.mat file
      }
    } 
  }
  dirs = list.dirs(folder,recursive = FALSE)
  if (length(dirs) > 0) {
    for (i in seq(length(dirs))) {
      out <- c(out, getMatlabData(dirs[[i]]))
    }
  }
  
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