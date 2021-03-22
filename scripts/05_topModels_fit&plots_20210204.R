# Collared pika abundance surveys in Alaska.
# Data collected July - Sep 2018 and July - Aug 2019
# Author: J. Wagner, P. Schuette
# Last updated: 16 Nov 2020

###############################################################-#

###          Step 5: Fit Models and Make Predictions          ###

###############################################################-#

# This script fits the top models and tests goodness of fit, makes predictions based on those models,
# and predicted density against model parameters.

# Read in the model building script. ** Note: This may take a while to load as R has to rebuild all of the models. **
source("scripts/04_distSamp_models.r")


# Part 1: Goodness of fit of the best performing models  --------------------------------------------------------
## Goodness of fit test: the model is a good fit if results of some or all of these tests show p > 0.05

# Fit a model: use your best-supported model (Lowest AIC)
summary(m3)  # Top model

(fm.m3 <- m3)

# Function returning three fit-statistics.
fitstats <- function(fm.m3) {
  observed <- getY(fm.m3@data)
  expected <- fitted(fm.m3)
  resids <- residuals(fm.m3)
  sse <- sum(resids^2)
  chisq <- sum((observed - expected)^2 / expected)
  freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
  out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
  return(out)
}
(pb.m3 <- parboot(fm.m3, fitstats, nsim=500, report=1))  #Chisq shows a pretty good fit.

## Check fit for next best model, m4
# summary(m4) 
# 
# (fm.m4 <- m4)
# 
# # Function returning three fit-statistics.
# fitstats <- function(fm.m4) {
#   observed <- getY(fm.m4@data)
#   expected <- fitted(fm.m4)
#   resids <- residuals(fm.m4)
#   sse <- sum(resids^2)
#   chisq <- sum((observed - expected)^2 / expected)
#   freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
#   out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
#   return(out)
# }
# (pb.m4 <- parboot(fm.m4, fitstats, nsim=500, report=1))  #Chisq shows a pretty good fit.
# 
# plot(fm.m4 residuals(fm.m4))
# 
# # Compare fit of both models
# pb.m3
# pb.m4

# Models have near-identical fit statistics. We will proceed with both models.

# Part 2: Predicted transect-level density estimates  --------------------------------------------------------------------
# Create new data frame with the covariates of interest for each model
head(transect.covs)

m3.covs <- data.frame(summerTemp=transect.covs$summerTemp,
                      transect=transect.covs$transect,
                      Location=transect.covs$Location)

# m4.covs <- data.frame(latitude=transect.covs$latitude, 
#                       summerTemp=transect.covs$summerTemp,
#                       transect=transect.covs$transect,
#                       Location=transect.covs$Location)

# Use the predict function to get estimates of density (type='state' indicates you want density) using coefficients
# from the best-supported models, combined with the covariate values for each transect.
m3.pred <- predict(m3, type='state', newdata=m3.covs, appendData=TRUE)
summary(m3.pred)

m3.pred.arrange <- m3.pred %>% 
  arrange(Predicted)

hist(m3.pred$Predicted)


# m4.pred <- predict(m4, type='state', newdata=m4.covs, appendData=TRUE)
# summary(m4.pred)
# 
# m4.pred.arrange <- m4.pred %>% 
#   arrange(Predicted)
# 
# hist(m4.pred$Predicted)

# Export an excel file that you can load into ArcGIS for making a pretty map
# write.csv(m3.pred, file="m3_density_20201118.csv")
# write.csv(m4.pred, file="m4_density_20201118.csv")

# Part 3: Predictions for explanatory variables  ---------------------------------------------------------------------
# M3 ------------------
# Define the dataframe as you have done already. But create a sequence of values within the range you had at your sites.
meanlat <- mean(transect.covs$latitude)

# Create a sequence of mean summer temperatures
summerTemp <- seq(min(transect.covs$summerTemp), max(transect.covs$summerTemp), length = 20)

m3.latConstant <- data.frame(summerTemp = summerTemp, latitude = meanlat)
m3.latConstant.predict <- predict(m3, type="state", newdata=m3.latConstant, appendData=TRUE)


# Part 4: Plot results: how do the model predictors influence density?  ----------------------------------------------
# Model 3, summer temperature -----------
summerTemp.m3 <- ggplot(m3.pred, aes(x=summerTemp, y=Predicted)) +
  geom_point()+
  theme_classic()+
  xlab("Mean Summer Temp (ºC)")+
  theme(axis.title.y = element_blank())

summerTemp.m3


# Plots for m3
summerTemp.m3 <- ggplot(m3.pred.arrange, aes(x=summerTemp, y=Predicted)) +
  geom_point()+
  geom_smooth(method = "loess", size = 1, color = "black")+
  theme_classic()+
  ylab(expression(paste("Pika / km"^"2")))+
  xlab("Mean Summer Temp (ºC)")+
  theme(axis.title.y = element_blank())

summerTemp.m3

with(m3.pred.arrange, {
  plot(summerTemp, Predicted, ylim=c(0,50), type="n",  #xaxt="n", yaxt="n",
       xlab="Mean Summer Temp (ºC)",
       ylab=(expression(paste("Pika /  ", km^"2"))),
       cex.lab=1.75, cex.axis=1.5) 
  lines(lowess(summerTemp, Predicted), lty=1, lwd=2) 
  lines(lowess(summerTemp, upper), lty=1, lwd=0.5)
  lines(lowess(summerTemp, lower), lty=1, lwd=0.5)   #aspect for exported image 600x540
})
