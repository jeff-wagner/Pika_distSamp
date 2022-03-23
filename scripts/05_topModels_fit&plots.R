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

# Fit a model: use your best-supported model (Lowest AICc)
fm1 <- models[[1]] # Top model
summary(fm1)  
confint(fm1, type = "state") # Coefficient for summer.tmax overlaps 0

# Assess multicolinearity
# vif(fm1, type = "state")
fm1.covs <- data.frame(meanNDVI=transect.covs$meanNDVI,
                       shrubCover=transect.covs$shrubCover,
                       summer.tmax=transect.covs$summer.tmax,
                       transect=transect.covs$transect,
                       Location=transect.covs$Location)
# fm1.covs$bogus <- runif(119)
# library(car)
# m.vif <- lm(bogus ~ vegclass + scale(aspect) + scale(latitude), data = fm1.covs)
# vif(m.vif)
# detach("package:car")

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
# Use the predict function to get estimates of density (type='state' indicates you want density) using coefficients
# from the best-supported models, combined with the covariate values for each transect.
fm1.pred <- predict(fm1, type='state', newdata=fm1.covs, appendData=TRUE)
summary(fm1.pred)

fm1.pred.arrange <- fm1.pred %>% 
  arrange(Predicted)

hist(fm1.pred$Predicted)

# Export an excel file that you can load into ArcGIS for making a pretty map
# write.csv(fm1.pred, file="output/fm1_density.csv")

# Part 3: Predictions for explanatory variables  ---------------------------------------------------------------------
# fm1 ------------------
# Define the dataframe as you have done already. But create a sequence of values within the range you had at your sites.
mean.meanNDVI <- mean(fm1.covs$meanNDVI)
mean.shrubCover <- mean(fm1.covs$shrubCover)
mean.summer.tmax <- mean(fm1.covs$summer.tmax)

# Create a sequence for each variable
meanNDVI <- seq(min(fm1.covs$meanNDVI), max(fm1.covs$meanNDVI), length = 119)
shrubCover <- seq(min(fm1.covs$shrubCover), max(fm1.covs$shrubCover), length = 119)
summer.tmax <- seq(min(fm1.covs$summer.tmax), max(fm1.covs$summer.tmax), length = 119)

# Create dataframes for each variable, filling other variables with the mean values
fm1.NDVI <- data.frame(meanNDVI = meanNDVI, shrubCover = mean.shrubCover, 
                       summer.tmax = mean.summer.tmax, transect = fm1.covs$transect, Location = fm1.covs$Location)

fm1.shrubCover <- data.frame(meanNDVI = mean.meanNDVI, shrubCover = shrubCover, 
                             summer.tmax = mean.summer.tmax, transect = fm1.covs$transect, Location = fm1.covs$Location)

fm1.summer.tmax <- data.frame(meanNDVI = mean.meanNDVI, shrubCover = mean.shrubCover, 
                              summer.tmax = summer.tmax, transect = fm1.covs$transect, Location = fm1.covs$Location)

# Make predictions
fm1.meanNDVI.predict <- predict(fm1, type = "state", newdata = fm1.NDVI, appendData=TRUE)
fm1.shrubCover.predict <- predict(fm1, type = "state", newdata = fm1.shrubCover, appendData=TRUE)
fm1.summer.tmax.predict <- predict(fm1, type = "state", newdata = fm1.summer.tmax, appendData=TRUE)

# Create dataframes for each prediction
fm1.meanNDVI.predict.df <- data.frame(Predicted = fm1.meanNDVI.predict$Predicted,
                                       lower = fm1.meanNDVI.predict$lower,
                                       upper = fm1.meanNDVI.predict$upper,
                                       fm1.NDVI)
fm1.shrubCover.predict.df <- data.frame(Predicted = fm1.shrubCover.predict$Predicted,
                                      lower = fm1.shrubCover.predict$lower,
                                      upper = fm1.shrubCover.predict$upper,
                                      fm1.shrubCover)
fm1.summer.tmax.predict.df <- data.frame(Predicted = fm1.summer.tmax.predict$Predicted,
                                        lower = fm1.summer.tmax.predict$lower,
                                        upper = fm1.summer.tmax.predict$upper,
                                        fm1.summer.tmax)


# Part 4: Plot results: how do the model predictors influence density?  ----------------------------------------------
# Top model, meanNDVI -----------
meanNDVI.fm1 <- ggplot(fm1.meanNDVI.predict.df, aes(x=meanNDVI, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("Mean NDVI")+
  theme(axis.title.y = element_blank())

meanNDVI.fm1

# Top model, shrubCover -----------
shrubCover.fm1 <- ggplot(fm1.shrubCover.predict.df, aes(x=shrubCover, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("% Shrub Cover")+
  theme(axis.title.y = element_blank())

shrubCover.fm1

# Top model, summer.tmax -----------
summer.tmax.fm1 <- ggplot(fm1.summer.tmax.predict.df, aes(x=summer.tmax, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("Average Daily Summer Maximum Temperature (ºC)")+
  theme(axis.title.y = element_blank())

summer.tmax.fm1

library(gridExtra)
library(grid)
png("./figures/fm1.covs.plot.png", units = "in", width = 8, height = 8, res = 300)
fm1.covs.plot <- grid.arrange(arrangeGrob(meanNDVI.fm1, shrubCover.fm1,
                                          grid::nullGrob(), summer.tmax.fm1, grid::nullGrob(),
                                          layout_matrix = matrix(c(1,1,2,2,3,4,4,5), byrow = TRUE, ncol = 4),
                                          left = textGrob(expression(
                                            paste("Denisty (Pika / km" ^ "2"*")")),
                                                         rot = 90,  gp = gpar(fontsize = 14))))
dev.off()

library(gplots)
pdf("./figures/top.models.table.pdf", width = 26, height = 3)
grid.table(modelList.sub)
dev.off()

# MODEL AVERAGING ---------------------------------------------------------
library(AICcmodavg)
avg.covs <- data.frame(latitude=transect.covs$latitude,
                       summer.pcpn.mm=transect.covs$summer.pcpn.mm,
                       summer.tmax=transect.covs$summer.tmax,
                       meanNDVI=transect.covs$meanNDVI,
                       shrubCover=transect.covs$shrubCover,
                       transect=transect.covs$transect,
                       Location=transect.covs$Location)
NDVIshrubTemp <- models[[1]]
NDVIshrub <- models[[2]]
latNDVIshrub <- models[[3]]
latNDVIshrubTemp <- models[[4]]
NDVIshrubPrecip <- models[[5]]
NDVIshrubPrecipTemp <- models[[6]]

mods <- list(NDVIshrubTemp=NDVIshrubTemp, NDVIshrub=NDVIshrub, 
             latNDVIshrub=latNDVIshrub, latNDVIshrubTemp=latNDVIshrubTemp, 
             NDVIshrubPrecip=NDVIshrubPrecip, NDVIshrubPrecipTemp=NDVIshrubPrecipTemp)

# Look at model coefficients: which models do we want to average?
(NDVIshrubTempCI <- data.frame(coef = coef(NDVIshrubTemp, type = "state"),  CI = confint(NDVIshrubTemp, type = "state")))
(NDVIshrubCI <- data.frame(coef = coef(NDVIshrub, type = "state"),  CI = confint(NDVIshrub, type = "state")))
(latNDVIshrubCI <- data.frame(coef = coef(latNDVIshrub, type = "state"),  CI = confint(latNDVIshrub, type = "state")))
(latNDVIshrubTempCI <- data.frame(coef = coef(latNDVIshrubTemp, type = "state"),  CI = confint(latNDVIshrubTemp, type = "state")))
(NDVIshrubPrecipCI <- data.frame(coef = coef(NDVIshrubPrecip, type = "state"),  CI = confint(NDVIshrubPrecip, type = "state")))
(NDVIshrubPrecipTempCI <- data.frame(coef = coef(NDVIshrubPrecipTemp, type = "state"),  CI = confint(NDVIshrubPrecipTemp, type = "state")))


Modnames <- names(mods)
modavg.pred <- modavgPred(cand.set = mods, modnames = Modnames, parm.type = "lambda",
                          newdata=avg.covs)


modavg.pred_df <- data.frame(Predicted = modavg.pred$mod.avg.pred,
                                         lower = modavg.pred$lower.CL,
                                         upper = modavg.pred$upper.CL,
                                         avg.covs)

# Predictions for explanatory variables  ---------------------------------------------------------------------
# fm1 ------------------
# Define the dataframe as you have done already. But create a sequence of values within the range you had at your sites.
meanlatitude <- mean(avg.covs$latitude)
meansummerpcpn <- mean(avg.covs$summer.pcpn.mm)
meansummertmax <- mean(avg.covs$summer.tmax)


# Create a sequence for each variable
latitude <- seq(min(avg.covs$latitude), max(avg.covs$latitude), length = 119)
summerpcpn <- seq(min(avg.covs$summer.pcpn.mm), max(avg.covs$summer.pcpn.mm), length = 119)
summertmax <- seq(min(avg.covs$summer.tmax), max(avg.covs$summer.tmax), length = 119)


avg.latitude <- data.frame(latitude = latitude, summer.pcpn.mm = meansummerpcpn, 
                           summer.tmax = meansummertmax, transect = avg.covs$transect, Location = avg.covs$Location)
avg.summerpcpn <- data.frame(latitude = meanlatitude, summer.pcpn.mm = summerpcpn, 
                             summer.tmax = meansummertmax, transect = avg.covs$transect, Location = avg.covs$Location)
avg.summertmax <- data.frame(latitude = meanlatitude, summer.pcpn.mm = meansummerpcpn, 
                             summer.tmax = summertmax, transect = avg.covs$transect, Location = avg.covs$Location)

avg.latitude.predict <- modavgPred(cand.set = mods, modnames = Modnames, parm.type = "lambda",
                                   newdata=avg.latitude)
avg.summerpcpn.predict <- modavgPred(cand.set = mods, modnames = Modnames, parm.type = "lambda",
                                     newdata=avg.summerpcpn)
avg.summertmax.predict <- modavgPred(cand.set = mods, modnames = Modnames, parm.type = "lambda",
                                     newdata=avg.summertmax)

avg.latitude.pred_df <- data.frame(Predicted = avg.latitude.predict$mod.avg.pred,
                                     lower = avg.latitude.predict$lower.CL,
                                     upper = avg.latitude.predict$upper.CL,
                                     avg.latitude)

avg.summerpcpn.pred_df <- data.frame(Predicted = avg.summerpcpn.predict$mod.avg.pred,
                                       lower = avg.summerpcpn.predict$lower.CL,
                                       upper = avg.summerpcpn.predict$upper.CL,
                                       avg.summerpcpn)

avg.summertmax.pred_df <- data.frame(Predicted = avg.summertmax.predict$mod.avg.pred,
                                     lower = avg.summertmax.predict$lower.CL,
                                     upper = avg.summertmax.predict$upper.CL,
                                     avg.summertmax)

# Plot results: how do the model predictors influence density?  ----------------------------------------------
# latitude 
latAvg <- ggplot(avg.latitude.pred_df, aes(x=latitude, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("Latitude")+
  theme(axis.title.y = element_blank())

latAvg

precipAvg <- ggplot(avg.summerpcpn.pred_df, aes(x=summer.pcpn.mm, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("Average cumulative summer precipitation (mm)")+
  theme(axis.title.y = element_blank())

precipAvg

tempAvg <- ggplot(avg.summertmax.pred_df, aes(x=summer.tmax, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("Average daily maximum summer temperatutre (ºC)")+
  theme(axis.title.y = element_blank())

tempAvg

library(gridExtra)
library(grid)
png("./figures/modAvgPlot.png", units = "in", width = 4, height = 8, res = 300)
avgPlots <- grid.arrange(arrangeGrob(latAvg, precipAvg, tempAvg,
                                          layout_matrix = matrix(c(1,1,2,2,3,3), byrow = TRUE, ncol = 1),
                                          left = textGrob(expression(
                                            paste("Denisty (Pika / km" ^ "2"*")")),
                                                         rot = 90,  gp = gpar(fontsize = 14))))
dev.off()
