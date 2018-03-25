# inspired by https://stackoverflow.com/questions/6967664/ggplot2-histogram-with-normal-curve (JWilliman)
# Matt Jaquiery, March 2018

# Return a data frame with the observations 'vector' and normal distribution values 'normal'
getNormal <- function(vector, bin.width) {
  out <- data.frame(predicted = vector,
                    normal = dnorm(vector, 
                                   mean(vector, na.rm = T), 
                                   sd(vector, na.rm = T)) * length(which(!is.na(vector)))*bin.width)
  return(out)
}

# #Example usage:
# library(ggplot2)
# column <- rnorm(720, mean = 4, sd = 2)
# column <- c(column, vector(length = 1500))
# column[column==0] <- NA
# binwidth <- 0.1
# norm <- getNormal(column, binwidth)
# 
# ggplot(norm, aes(predicted))  +
#   geom_histogram(breaks = seq(-3,10, binwidth), colour = "black", fill = "white") +
#   geom_line(aes(y = norm$normal), colour = "red")
