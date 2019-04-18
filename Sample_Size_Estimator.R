#  Function to estimate confidence interval width (in the units of measurement) as a function of sample size (from knonw standard deviation)
#     inputs:
#        SD: this is the only manditory value, it can be a vector or a single value. If it is a vector, the standard deviation of the vector is used. If it is a single number, this is used as the standard deviation   
#        LowerBound: the MINIMUM sample size you wish to consider (default = 2)... !!! NOTE should be higher than 2
#        UpperBound: the MAXIMUM sample size you wish to consider (default = 100)
#        alpha: the desired level of confidence (default = 0.05, i.e., 95% confidence interval)
#        DesiredCI: the desired confidence interval width (in units of measurement)      
#
#     outputs:
#         OP: a data frame containing the the sample size and resulting confidence interval width, and the SS sample size needed if DesiredCI is provided
#
#     figure: a plot of confidence interval width as a function of sample size


SS_Est <- function(SD, LowerBound = 2, UpperBound = 100, alpha = 0.05, DesiredCI = NULL){
   OP <- list()
   if(length(SD) > 1){
      SD <- sd(SD, na.rm = T)
   }
   CI <- rep(NA, length(LowerBound:UpperBound))
   SD <- 5
   for(i in LowerBound:UpperBound){
      DF <- i - 1
      SE <- SD/sqrt(i)
      ME <- qt(1-(alpha/2), DF)*SE
      CI[i - LowerBound + 1] <- 2*ME
   }
   if(min(CI, na.rm = T) > DesiredCI){
      print("WARNING!!! You need a larger sample size than currently provided by 'UpperBound'. Increase 'UpperBOund'")
   }
   if(!DesiredCI){
      plot(LowerBound:UpperBound, CI, bty = "l", pch = 16, cex = 2, ylab = paste((1-alpha)*100, "% Confidence Interval Width (in units of measurement)", sep = ""), type = "o", xlab = "Sample Size (n)")
   }
   if(DesiredCI){
      if(length(which(CI > DesiredCI))){
         SS <- max(which(CI > DesiredCI), na.rm = T) + LowerBound
         plot(LowerBound:UpperBound, CI, bty = "l", pch = 16, cex = 2, ylab = paste((1-alpha)*100, "% Confidence Interval Width", sep = ""), type = "o", xlab = "Sample Size (n)")
         abline(h = DesiredCI, lwd = 2, lty = 2, col = "red")
         points(LowerBound:(SS-1), CI[1:(SS-LowerBound)], pch = 16, cex = 2, col = "red")
         legend("topright", "Desired Confidence Interval Width", lty = 2, lwd = 2, col = "red", bty = "n")
         legend("center", paste("You need n =", SS), cex = 2, bty = "n")
      }
      if(!length(which(CI > DesiredCI))){
         print("WARNING!!! You achieve you're desired CI width at a smaller sample size than 'LowerBound'. Decrease 'LowerBound'")
      }
   }
   OP[[1]] <- as.data.frame(cbind(LowerBound:UpperBound, CI))
   colnames(OP[[1]]) <- c("n","CI_Width")
   OP[[2]] <- SS
   names(OP) <- c("DF", "SS")
   return(OP)
}


# example using the a standard deviation = 5 and a desired confidence interval under 10 (in the units of measurement)
Ex1<-SS_Est(5, DesiredCI = 10)

# full data frame
Ex1$DF

# the sample size you need to reach a confidence interval width < 'DesiredCI'
Ex1$SS



# example using a vector and a desired CI width of 3
tmpV <- rnorm(100, 74, 12.5)
Ex2 <- SS_Est(tmpV, DesiredCI = 3)

# full data frame
Ex2$DF

# the sample size you need to reach a confidence interval width < 'DesiredCI'
Ex2$SS



