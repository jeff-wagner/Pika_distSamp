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
source("scripts/04_distSamp_models_20211006.r")


# Part 1: Goodness of fit of the best performing models  --------------------------------------------------------
## Goodness of fit test: the model is a good fit if results of some or all of these tests show p > 0.05

# Fit a model: use your best-supported model (Lowest AICc)
fm1 <- models[[1]] # Top model
summary(fm1)  
confint(fm1, type = "state") # Coefficients do not overlap 0

# Function returning three fit-statistics.
fitstats <- function(fm1) {
  observed <- getY(fm1@data)
  expected <- fitted(fm1)
  resids <- residuals(fm1)
  sse <- sum(resids^2)
  chisq <- sum((observed - expected)^2 / expected)
  freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
  out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
  return(out)
}
(pb.fm1 <- parboot(fm1, fitstats, nsim=500, report=1))  #Chisq shows a pretty good fit.

# Part 2: Predicted transect-level density estimates  --------------------------------------------------------------------
# Create new data frame with the covariates of interest for each model
head(transect.covs)

fm1.covs <- data.frame(summer.pcpn.mm=transect.covs$summer.pcpn.mm,
                      aspect=transect.covs$aspect,
                      summer.tmax=transect.covs$summer.tmax,
                      percent.tmax.days=transect.covs$percent.tmax.days,
                      transect=transect.covs$transect,
                      Location=transect.covs$Location)


# Use the predict function to get estimates of density (type='state' indicates you want density) using coefficients
# from the best-supported models, combined with the covariate values for each transect.
fm1.pred <- predict(fm1, type='state', newdata=fm1.covs, appendData=TRUE)
summary(fm1.pred)

fm1.pred.arrange <- fm1.pred %>% 
  arrange(Predicted)

hist(fm1.pred$Predicted)

# Export an excel file that you can load into ArcGIS for making a pretty map
# write.csv(m3.pred, file="m3_density_20201118.csv")
# write.csv(m4.pred, file="m4_density_20201118.csv")

# Part 3: Predictions for explanatory variables  ---------------------------------------------------------------------
# fm1 ------------------
# Define the dataframe as you have done already. But create a sequence of values within the range you had at your sites.
meansummerpcpn <- mean(fm1.covs$summer.pcpn.mm)
meanaspect <- mean(fm1.covs$aspect)
meansummertmax <- mean(fm1.covs$summer.tmax)
meantmaxdays <- mean(fm1.covs$percent.tmax.days)

# Create a sequence for each variable
summerpcpn <- seq(min(fm1.covs$summer.pcpn.mm), max(fm1.covs$summer.pcpn.mm), length = 20)
aspect <- seq(min(fm1.covs$aspect), max(fm1.covs$aspect), length = 20)
summertmax <- seq(min(fm1.covs$summer.tmax), max(fm1.covs$summer.tmax), length = 20)
tmaxdays <- seq(min(fm1.covs$percent.tmax.days), max(fm1.covs$percent.tmax.days), length = 20)

fm1.summerpcpn <- data.frame(summer.pcpn.mm = summerpcpn, aspect = meanaspect,
                       summer.tmax = meansummertmax, percent.tmax.days = meantmaxdays)
fm1.aspect <- data.frame(summer.pcpn.mm = meansummerpcpn, aspect = aspect,
                         summer.tmax = meansummertmax, percent.tmax.days = meantmaxdays)
fm1.summertmax <- data.frame(summer.pcpn.mm = meansummerpcpn, aspect = meanaspect,
                             summer.tmax = summertmax, percent.tmax.days = meantmaxdays)
fm1.tmaxdays <- data.frame(summer.pcpn.mm = meansummerpcpn, aspect = meanaspect,
                           summer.tmax = meansummertmax, percent.tmax.days = tmaxdays)

fm1.summerpcpn.predict <- predict(fm1, type="state", newdata=fm1.summerpcpn, appendData=TRUE)
fm1.aspect.predict <- predict(fm1, type="state", newdata=fm1.aspect, appendData=TRUE)
fm1.summertmax.predict <- predict(fm1, type="state", newdata=fm1.summertmax, appendData=TRUE)
fm1.tmaxdays.predict <- predict(fm1, type="state", newdata=fm1.tmaxdays, appendData=TRUE)


# Part 4: Plot results: how do the model predictors influence density?  ----------------------------------------------
# Top model, summer precipitation -----------
summerpcpn.fm1 <- ggplot(fm1.summerpcpn.predict, aes(x=summer.pcpn.mm, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("Cumulative Summer Precipitation (mm)")+
  theme(axis.title.y = element_blank())

summerpcpn.fm1


# Top model, aspect -----------
aspect.fm1 <- ggplot(fm1.aspect.predict, aes(x=aspect, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("Aspect (º)")+
  theme(axis.title.y = element_blank())

aspect.fm1

# Top model, mean summer max temperature -----------
summertmax.fm1 <- ggplot(fm1.summertmax.predict, aes(x=summer.tmax, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("Average Summer Maximum Temperature (ºC)")+
  theme(axis.title.y = element_blank())

summertmax.fm1

# Top model, percent summer max temp days -----------
tmaxdays.fm1 <- ggplot(fm1.tmaxdays.predict, aes(x=percent.tmax.days, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("Percent of Summer Days with\nMaximum Temperature > 26ºC")+
  theme(axis.title.y = element_blank())

tmaxdays.fm1

library(gridExtra)
library(grid)
png("./figures/fm1.covs.plot.png", units = "in", width = 8, height = 8, res = 300)
fm1.covs.plot <- grid.arrange(arrangeGrob(aspect.fm1, summerpcpn.fm1,
                                          summertmax.fm1, tmaxdays.fm1,
                                          nrow = 2, ncol = 2, 
                                          left = textGrob(expression(
                                            paste("Denisty (Pika / km" ^ "2"*")")),
                                                         rot = 90,  gp = gpar(fontsize = 10))))
dev.off()

library(gplots)
pdf("./figures/top.models.table.pdf", width = 22, height = 6)
grid.table(modelList.sub)
dev.off()

with(m3.pred.arrange, {
  plot(summerTemp, Predicted, ylim=c(0,50), type="n",  #xaxt="n", yaxt="n",
       xlab="Mean Summer Temp (ºC)",
       ylab=(expression(paste("Pika /  ", km^"2"))),
       cex.lab=1.75, cex.axis=1.5) 
  lines(lowess(summerTemp, Predicted), lty=1, lwd=2) 
  lines(lowess(summerTemp, upper), lty=1, lwd=0.5)
  lines(lowess(summerTemp, lower), lty=1, lwd=0.5)   #aspect for exported image 600x540
})
