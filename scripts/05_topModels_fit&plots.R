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
expMod.Selection

## Top model - Climate & Productivity
# Assess multicolinearity
vif(climateProductivity, type = "state")
covs.climateProductivity <- data.frame(precip=transect.covs$precip,
                                       summerWarmth=transect.covs$summerWarmth,
                                       logs=transect.covs$logs,
                                       wetness=transect.covs$wetness)
covs.climateProductivity$bogus <- runif(119)
library(car)
vif.climateProductivity <- lm(bogus ~ scale(precip) + scale(summerWarmth) + scale(logs) + scale(wetness),
                              data = covs.climateProductivity)
vif(vif.climateProductivity)
detach("package:car")

# Function returning three fit-statistics.
fitstats <- function(climateProductivity) {
  observed <- getY(climateProductivity@data)
  expected <- fitted(climateProductivity)
  resids <- residuals(climateProductivity)
  sse <- sum(resids^2)
  chisq <- sum((observed - expected)^2 / expected)
  freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
  out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
  return(out)
}
(pb.climateProductivity <- parboot(climateProductivity, fitstats, nsim=500, report=1))

## Second best model - Climate
# Assess multicolinearity
vif(climate, type = "state")
covs.climate <- data.frame(precip=transect.covs$precip,
                          summerWarmth=transect.covs$summerWarmth,
                          januaryMinTemp=transect.covs$januaryMinTemp)
covs.climate$bogus <- runif(119)
library(car)
vif.climate <- lm(bogus ~ scale(precip) + scale(summerWarmth) + scale(januaryMinTemp), data = covs.climate)
vif(vif.climate)
detach("package:car")

# Function returning three fit-statistics.
fitstats <- function(climate) {
  observed <- getY(climate@data)
  expected <- fitted(climate)
  resids <- residuals(climate)
  sse <- sum(resids^2)
  chisq <- sum((observed - expected)^2 / expected)
  freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
  out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
  return(out)
}
(pb.climate <- parboot(climate, fitstats, nsim=500, report=1))

# Part 2: Predicted transect-level density estimates  --------------------------------------------------------------------
# Use the predict function to get estimates of density (type='state' indicates you want density) using coefficients from the best-supported models, combined with the covariate values for each transect.

## Top model - Climate & Productivity
pred.climateProductivity <- predict(climateProductivity, type='state', newdata=covs.climateProductivity, appendData=TRUE)
summary(pred.climateProductivity)

pred.climateProductivity <- pred.climateProductivity %>% 
  arrange(Predicted)

hist(pred.climateProductivity$Predicted)

# Export an excel file that you can load into ArcGIS for making a pretty map
# write.csv(pred.climateProductivity, file="output/density_climateProductivity.csv")

## Second best model - Climate
pred.climate <- predict(climate, type='state', newdata=covs.climate, appendData=TRUE)
summary(pred.climate)

pred.climate <- pred.climate %>% 
  arrange(Predicted)

hist(pred.climate$Predicted)

# Export an excel file that you can load into ArcGIS for making a pretty map
# write.csv(pred.climate, file="output/density_climate.csv")

# Part 3: Predictions for explanatory variables  ---------------------------------------------------------------------

## Top model - Climate & Productivity
# Define the dataframe as you have done already. But create a sequence of values within the range you had at your sites.
mean.precip <- mean(covs.climateProductivity$precip)
mean.summerWarmth <- mean(covs.climateProductivity$summerWarmth)
mean.logs <- mean(covs.climateProductivity$logs)
mean.wetness <- mean(covs.climateProductivity$wetness)

# Create a sequence for each variable
precip <- seq(min(covs.climateProductivity$precip), max(covs.climateProductivity$precip), length = 100)
summerWarmth <- seq(min(covs.climateProductivity$summerWarmth), max(covs.climateProductivity$summerWarmth), length = 100)
logs <- seq(min(covs.climateProductivity$logs), max(covs.climateProductivity$logs), length = 100)
wetness <- seq(min(covs.climateProductivity$wetness), max(covs.climateProductivity$wetness), length = 100)

df.climateProductivityPrecip <- data.frame(precip = precip, summerWarmth = mean.SummerWarmth, logs = mean.logs, wetness = mean.wetness)
df.climateProductivitySummerWarmth <- data.frame(precip = mean.precip, summerWarmth = summerWarmth, logs = mean.logs, wetness = mean.wetness)
df.climateProductivityLogs <- data.frame(precip = mean.precip, summerWarmth = mean.SummerWarmth, logs = logs, wetness = mean.wetness)
df.climateProductivityWetness <- data.frame(precip = mean.precip, summerWarmth = mean.SummerWarmth, logs = mean.logs, wetness = wetness)

predict.climateProductivityPrecip <- predict(climateProductivity, type="state", newdata=df.climateProductivityPrecip, appendData=TRUE)
predict.climateProductivitySummerWarmth <- predict(climateProductivity, type="state", newdata=df.climateProductivitySummerWarmth, appendData=TRUE)
predict.climateProductivityLogs <- predict(climateProductivity, type="state", newdata=df.climateProductivityLogs, appendData=TRUE)
predict.climateProductivityWetness <- predict(climateProductivity, type="state", newdata=df.climateProductivityWetness, appendData=TRUE)

## Second best model - Climate
# Define the dataframe as you have done already. But create a sequence of values within the range you had at your sites.
mean.precip <- mean(covs.climate$precip)
mean.summerWarmth <- mean(covs.climate$summerWarmth)
mean.januaryMinTemp <- mean(covs.climate$januaryMinTemp)

# Create a sequence for each variable
precip <- seq(min(covs.climate$precip), max(covs.climate$precip), length = 100)
summerWarmth <- seq(min(covs.climate$summerWarmth), max(covs.climate$summerWarmth), length = 100)
januaryMinTemp <- seq(min(covs.climate$januaryMinTemp), max(covs.climate$januaryMinTemp), length = 100)

df.climatePrecip <- data.frame(precip = precip, summerWarmth = mean.SummerWarmth, januaryMinTemp = mean.januaryMinTemp)
df.climateSummerWarmth <- data.frame(precip = mean.precip, summerWarmth = summerWarmth, januaryMinTemp = mean.januaryMinTemp)
df.climateJanuaryMinTemp <- data.frame(precip = mean.precip, summerWarmth = mean.SummerWarmth, januaryMinTemp = januaryMinTemp)


predict.climatePrecip <- predict(climate, type="state", newdata=df.climatePrecip, appendData=TRUE)
predict.climateSummerWarmth <- predict(climate, type="state", newdata=df.climateSummerWarmth, appendData=TRUE)
predict.climateJanuaryMinTemp <- predict(climate, type="state", newdata=df.climateJanuaryMinTemp, appendData=TRUE)


# Part 4: Plot results: how do the model predictors influence density?  ----------------------------------------------
## Top Model - Climate & Productivity -------------------------------
# Precip
plot.climateProductivityPrecip <- ggplot(predict.climateProductivityPrecip, aes(x=precip, y=Predicted)) +
                                          geom_line(size = 1)+
                                          geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
                                          theme_classic()+
                                          xlab("Normalized mean annual precipitation")+
                                          theme(axis.title.y = element_blank())

plot.climateProductivityPrecip

# Summer warmth
plot.climateProductivitySummerWarmth <- ggplot(predict.climateProductivitySummerWarmth, aes(x=summerWarmth, y=Predicted)) +
                                                geom_line(size = 1)+
                                                geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
                                                theme_classic()+
                                                xlab("Normalized summer warmth index")+
                                                theme(axis.title.y = element_blank())

plot.climateProductivitySummerWarmth

# Length of growing season
plot.climateProductivityLogs <- ggplot(predict.climateProductivityLogs, aes(x=logs, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("Length of growing season (days)")+
  theme(axis.title.y = element_blank())

plot.climateProductivityLogs

# Topographic wetness
plot.climateProductivityWetness <- ggplot(predict.climateProductivityWetness, aes(x=wetness, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("Topographic wetness")+
  theme(axis.title.y = element_blank())

plot.climateProductivityWetness

# Stitch plots together
library(gridExtra)
library(grid)
png("./output/figures/climateProductivity.png", units = "in", width = 8, height = 8, res = 300)
plot.climate <- grid.arrange(arrangeGrob(plot.climateProductivityPrecip, plot.climateProductivitySummerWarmth,
                                         plot.climateProductivityLogs, plot.climateProductivityWetness,
                                         layout_matrix = matrix(c(1,2,3,4), byrow = TRUE, ncol = 2),
                                         left = textGrob(expression(
                                           paste("Denisty (Pika / km" ^ "2"*")")),
                                           rot = 90,  gp = gpar(fontsize = 10))))
dev.off()

## Second best model - Climate -----------------------
# Precip
plot.climatePrecip <- ggplot(predict.climatePrecip, aes(x=precip, y=Predicted)) +
                              geom_line(size = 1)+
                              geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
                              theme_classic()+
                              xlab("Normalized mean annual precipitation")+
                              theme(axis.title.y = element_blank())

plot.climatePrecip

# Summer warmth
plot.climateSummerWarmth <- ggplot(predict.climateSummerWarmth, aes(x=summerWarmth, y=Predicted)) +
                                    geom_line(size = 1)+
                                    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
                                    theme_classic()+
                                    xlab("Normalized summer warmth index")+
                                    theme(axis.title.y = element_blank())

plot.climateSummerWarmth

# January minimum temperature
plot.climateJanuaryMinTemp <- ggplot(predict.climateJanuaryMinTemp, aes(x=januaryMinTemp, y=Predicted)) +
                                      geom_line(size = 1)+
                                      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
                                      theme_classic()+
                                      xlab("January minimum temperature (°C)")+
                                      theme(axis.title.y = element_blank())

plot.climateJanuaryMinTemp

# Stitch plots together
png("./output/figures/climate.png", units = "in", width = 8, height = 8, res = 300)
plot.climate <- grid.arrange(arrangeGrob(plot.climatePrecip, plot.climateSummerWarmth,
                                         grid::nullGrob(), plot.climateJanuaryMinTemp, grid::nullGrob(),
                                          layout_matrix = matrix(c(1,1,2,2,3,4,4,5), byrow = TRUE, ncol = 4),
                                          left = textGrob(expression(
                                            paste("Denisty (Pika / km" ^ "2"*")")),
                                                         rot = 90,  gp = gpar(fontsize = 10))))
dev.off()

library(gplots)
pdf("./output/figures/explanatoryModels.pdf", width = 10, height = 4)
grid.table(expMod.Selection)
dev.off()

# # MODEL AVERAGING ---------------------------------------------------------
library(AICcmodavg)
avg.covs <- data.frame(precip=transect.covs$precip,
                       summerWarmth=transect.covs$summerWarmth,
                       januaryMinTemp=transect.covs$januaryMinTemp,
                       logs=transect.covs$logs,
                       wetness=transect.covs$wetness)

mods <- list(climate=climate, climateProductivity=climateProductivity)
modNames <- names(mods)
modavg.pred <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "lambda",
                          newdata=avg.covs)

covs.avg <- data.frame(Predicted = modavg.pred$mod.avg.pred,
                       lower = modavg.pred$lower.CL,
                       upper = modavg.pred$upper.CL,
                       avg.covs)

# Part 3: Predictions for explanatory variables  ---------------------------------------------------------------------
# Define the dataframes -------------------------------------
df.avgPrecip <- data.frame(precip = precip, summerWarmth=mean.summerWarmth, januaryMinTemp=mean.januaryMinTemp,
                           logs=mean.logs, wetness=mean.wetness)
df.avgSummerWarmth <- data.frame(precip = mean.precip, summerWarmth=summerWarmth, januaryMinTemp=mean.januaryMinTemp,
                                 logs=mean.logs, wetness=mean.wetness)
df.avgJanuaryMinTemp <- data.frame(precip = mean.precip, summerWarmth=mean.summerWarmth, januaryMinTemp=januaryMinTemp,
                                    logs=mean.logs, wetness=mean.wetness)
df.avgLogs <- data.frame(precip = mean.precip, summerWarmth=mean.summerWarmth, januaryMinTemp=mean.januaryMinTemp,
                           logs=logs, wetness=mean.wetness)
df.avgWetness <- data.frame(precip = mean.precip, summerWarmth=mean.summerWarmth, januaryMinTemp=mean.januaryMinTemp,
                            logs=mean.logs, wetness=wetness)
# Make predictions -----------------------------------------
predict.avgPrecip <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "lambda",
                                newdata = df.avgPrecip)
predict.avgSummerWarmth <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "lambda",
                                      newdata = df.avgSummerWarmth)
predict.avgJanuaryMinTemp <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "lambda",
                                        newdata = df.avgJanuaryMinTemp)
predict.avgLogs <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "lambda",
                                newdata = df.avgLogs)
predict.avgWetness <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "lambda",
                                newdata = df.avgWetness)
# Convert to dataframes --------------------------------
df.predict.avgPrecip <- data.frame(Predicted = predict.avgPrecip$mod.avg.pred,
                                       lower = predict.avgPrecip$lower.CL,
                                       upper = predict.avgPrecip$upper.CL,
                                       df.avgPrecip)
df.predict.avgSummerWarmth <- data.frame(Predicted = predict.avgSummerWarmth$mod.avg.pred,
                                   lower = predict.avgSummerWarmth$lower.CL,
                                   upper = predict.avgSummerWarmth$upper.CL,
                                   df.avgSummerWarmth)
df.predict.avgJanuaryMinTemp <- data.frame(Predicted = predict.avgJanuaryMinTemp$mod.avg.pred,
                                   lower = predict.avgJanuaryMinTemp$lower.CL,
                                   upper = predict.avgJanuaryMinTemp$upper.CL,
                                   df.avgJanuaryMinTemp)
df.predict.avgLogs <- data.frame(Predicted = predict.avgLogs$mod.avg.pred,
                                   lower = predict.avgLogs$lower.CL,
                                   upper = predict.avgLogs$upper.CL,
                                   df.avgLogs)
df.predict.avgWetness <- data.frame(Predicted = predict.avgWetness$mod.avg.pred,
                                   lower = predict.avgWetness$lower.CL,
                                   upper = predict.avgWetness$upper.CL,
                                   df.avgWetness)

# Plot -----------------------------
# Precip
plot.avgPrecip <- ggplot(df.predict.avgPrecip, aes(x=precip, y=Predicted)) +
                          geom_line(size = 1)+
                          geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
                          theme_classic()+
                          xlab("Normalized mean annual precipitation")+
                          theme(axis.title.y = element_blank())

plot.avgPrecip

# Summer warmth
plot.avgSummerWarmth <- ggplot(df.predict.avgSummerWarmth, aes(x=summerWarmth, y=Predicted)) +
                                geom_line(size = 1)+
                                geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
                                theme_classic()+
                                xlab("Normalized summer warmth index")+
                                theme(axis.title.y = element_blank())

plot.avgSummerWarmth

# January minimum temperature
plot.avgJanuaryMinTemp <- ggplot(df.predict.avgJanuaryMinTemp, aes(x=januaryMinTemp, y=Predicted)) +
                                  geom_line(size = 1)+
                                  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
                                  theme_classic()+
                                  xlab("January minimum temperature (°C)")+
                                  theme(axis.title.y = element_blank())

plot.avgJanuaryMinTemp

# Length of growing season
plot.avgLogs <- ggplot(df.predict.avgLogs, aes(x=logs, y=Predicted)) +
                        geom_line(size = 1)+
                        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
                        theme_classic()+
                        xlab("Length of growing season (days)")+
                        theme(axis.title.y = element_blank())

plot.avgLogs

# Topographic wetness
plot.avgWetness <- ggplot(df.predict.avgWetness, aes(x=wetness, y=Predicted)) +
                          geom_line(size = 1)+
                          geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
                          theme_classic()+
                          xlab("Topographic wetness")+
                          theme(axis.title.y = element_blank())

plot.avgWetness

# Stitch plots together -----------------------------------
png("./output/figures/averaged.png", units = "in", width = 8, height = 10, res = 300)
plot.climate <- grid.arrange(arrangeGrob(plot.avgPrecip, plot.avgSummerWarmth,
                                         plot.avgJanuaryMinTemp, plot.avgLogs,
                                         grid::nullGrob(), plot.avgWetness, grid::nullGrob(),
                                         layout_matrix = matrix(c(1,1,2,2,
                                                                  3,3,4,4,
                                                                  5,6,6,7), byrow = TRUE, ncol = 4),
                                         left = textGrob(expression(
                                           paste("Denisty (Pika / km" ^ "2"*")")),
                                           rot = 90,  gp = gpar(fontsize = 10))))
dev.off()
