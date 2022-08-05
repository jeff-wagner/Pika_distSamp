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

## Top model - Climate & Productivity (with logs)
covs.climateProductivity2 <- data.frame(precip=transect.covs$precip,
                                       summerWarmth=transect.covs$summerWarmth,
                                       logs=transect.covs$logs,
                                       ndvi=transect.covs$ndvi)

## Next best model - Climate & Productivity
# Assess multicolinearity
vif(climateProductivity, type = "state")
covs.climateProductivity <- data.frame(precip=transect.covs$precip,
                                       summerWarmth=transect.covs$summerWarmth,
                                       januaryMinTemp=transect.covs$januaryMinTemp,
                                       ndvi=transect.covs$ndvi)
# covs.climateProductivity$bogus <- runif(119)
# library(car)
# vif.climateProductivity <- lm(bogus ~ scale(precip) + scale(summerWarmth) + scale(januaryMinTemp) + scale(ndvi),
#                               data = covs.climateProductivity)
# vif(vif.climateProductivity)
# detach("package:car")

# Function returning three fit-statistics.
# fitstats <- function(climateProductivity) {
#   observed <- getY(climateProductivity@data)
#   expected <- fitted(climateProductivity)
#   resids <- residuals(climateProductivity)
#   sse <- sum(resids^2)
#   chisq <- sum((observed - expected)^2 / expected)
#   freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
#   out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
#   return(out)
# }
# (pb.climateProductivity <- parboot(climateProductivity, fitstats, nsim=500, report=1))

## Next best model - Climate
# Assess multicolinearity
vif(climate, type = "state")
covs.climate <- data.frame(precip=transect.covs$precip,
                          summerWarmth=transect.covs$summerWarmth,
                          januaryMinTemp=transect.covs$januaryMinTemp)
# covs.climate$bogus <- runif(119)
# library(car)
# vif.climate <- lm(bogus ~ scale(precip) + scale(summerWarmth) + scale(januaryMinTemp), data = covs.climate)
# vif(vif.climate)
# detach("package:car")

# Function returning three fit-statistics.
# fitstats <- function(climate) {
#   observed <- getY(climate@data)
#   expected <- fitted(climate)
#   resids <- residuals(climate)
#   sse <- sum(resids^2)
#   chisq <- sum((observed - expected)^2 / expected)
#   freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
#   out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
#   return(out)
# }
# (pb.climate <- parboot(climate, fitstats, nsim=500, report=1))

# Part 2: Predicted transect-level density estimates  --------------------------------------------------------------------
# Use the predict function to get estimates of density (type='state' indicates you want density) using coefficients from the best-supported models, combined with the covariate values for each transect.

## Top model - Climate & Productivity (with logs)
pred.climateProductivity2 <- predict(climateProductivity2, type='state', newdata=covs.climateProductivity2, appendData=TRUE)
summary(pred.climateProductivity2)

pred.climateProductivity2 <- pred.climateProductivity2 %>% 
  arrange(Predicted)

hist(pred.climateProductivity2$Predicted)

## Next best model - Climate & Productivity
pred.climateProductivity <- predict(climateProductivity, type='state', newdata=covs.climateProductivity, appendData=TRUE)
summary(pred.climateProductivity)

pred.climateProductivity <- pred.climateProductivity %>% 
  arrange(Predicted)

hist(pred.climateProductivity$Predicted)

# Export an excel file that you can load into ArcGIS for making a pretty map
# write.csv(pred.climateProductivity, file="output/density_climateProductivity.csv")

## Next best model - Climate
pred.climate <- predict(climate, type='state', newdata=covs.climate, appendData=TRUE)
summary(pred.climate)

pred.climate <- pred.climate %>% 
  arrange(Predicted)

hist(pred.climate$Predicted)

# Export an excel file that you can load into ArcGIS for making a pretty map
# write.csv(pred.climate, file="output/density_climate.csv")

# Part 3: Predictions for explanatory variables  ---------------------------------------------------------------------

## Top model - Climate & Productivity (with logs)
# Define the dataframe as you have done already. But create a sequence of values within the range you had at your sites.
mean.precip <- mean(covs.climateProductivity2$precip)
mean.summerWarmth <- mean(covs.climateProductivity2$summerWarmth)
mean.logs <- mean(covs.climateProductivity2$logs)
mean.ndvi <- mean(covs.climateProductivity2$ndvi)

# Create a sequence for each variable
precip <- seq(min(covs.climateProductivity2$precip), max(covs.climateProductivity2$precip), length = 100)
summerWarmth <- seq(min(covs.climateProductivity2$summerWarmth), max(covs.climateProductivity2$summerWarmth), length = 100)
logs <- seq(min(covs.climateProductivity2$logs), max(covs.climateProductivity2$logs), length = 100)
ndvi <- seq(min(covs.climateProductivity2$ndvi), max(covs.climateProductivity2$ndvi), length = 100)

df.climateProductivity2Precip <- data.frame(precip = precip, summerWarmth = mean.summerWarmth, logs = mean.logs, ndvi = mean.ndvi)
df.climateProductivity2SummerWarmth <- data.frame(precip = mean.precip, summerWarmth = summerWarmth, logs = mean.logs, ndvi = mean.ndvi)
df.climateProductivity2Logs <- data.frame(precip = mean.precip, summerWarmth = mean.summerWarmth, logs = logs, ndvi = mean.ndvi)
df.climateProductivity2NDVI <- data.frame(precip = mean.precip, summerWarmth = mean.summerWarmth, logs = mean.logs, ndvi = ndvi)

predict.climateProductivity2Precip <- predict(climateProductivity2, type="state", newdata=df.climateProductivity2Precip, appendData=TRUE)
predict.climateProductivity2SummerWarmth <- predict(climateProductivity2, type="state", newdata=df.climateProductivity2SummerWarmth, appendData=TRUE)
predict.climateProductivity2Logs <- predict(climateProductivity2, type="state", newdata=df.climateProductivity2Logs, appendData=TRUE)
predict.climateProductivity2NDVI <- predict(climateProductivity2, type="state", newdata=df.climateProductivity2NDVI, appendData=TRUE)


## Next best model - Climate & Productivity
# Define the dataframe as you have done already. But create a sequence of values within the range you had at your sites.
mean.precip <- mean(covs.climateProductivity$precip)
mean.summerWarmth <- mean(covs.climateProductivity$summerWarmth)
mean.januaryMinTemp <- mean(covs.climateProductivity$januaryMinTemp)
mean.ndvi <- mean(covs.climateProductivity$ndvi)

# Create a sequence for each variable
precip <- seq(min(covs.climateProductivity$precip), max(covs.climateProductivity$precip), length = 100)
summerWarmth <- seq(min(covs.climateProductivity$summerWarmth), max(covs.climateProductivity$summerWarmth), length = 100)
januaryMinTemp <- seq(min(covs.climateProductivity$januaryMinTemp), max(covs.climateProductivity$januaryMinTemp), length = 100)
ndvi <- seq(min(covs.climateProductivity$ndvi), max(covs.climateProductivity$ndvi), length = 100)

df.climateProductivityPrecip <- data.frame(precip = precip, summerWarmth = mean.summerWarmth, januaryMinTemp = mean.januaryMinTemp, ndvi = mean.ndvi)
df.climateProductivitySummerWarmth <- data.frame(precip = mean.precip, summerWarmth = summerWarmth, januaryMinTemp = mean.januaryMinTemp, ndvi = mean.ndvi)
df.climateProductivityJanuaryMinTemp <- data.frame(precip = mean.precip, summerWarmth = mean.summerWarmth, januaryMinTemp = januaryMinTemp, ndvi = mean.ndvi)
df.climateProductivityNDVI <- data.frame(precip = mean.precip, summerWarmth = mean.summerWarmth, januaryMinTemp = mean.januaryMinTemp, ndvi = ndvi)

predict.climateProductivityPrecip <- predict(climateProductivity, type="state", newdata=df.climateProductivityPrecip, appendData=TRUE)
predict.climateProductivitySummerWarmth <- predict(climateProductivity, type="state", newdata=df.climateProductivitySummerWarmth, appendData=TRUE)
predict.climateProductivityJanuaryMinTemp <- predict(climateProductivity, type="state", newdata=df.climateProductivityJanuaryMinTemp, appendData=TRUE)
predict.climateProductivityNDVI <- predict(climateProductivity, type="state", newdata=df.climateProductivityNDVI, appendData=TRUE)

## Next best model - Climate
# Define the dataframe as you have done already. But create a sequence of values within the range you had at your sites.
mean.precip <- mean(covs.climate$precip)
mean.summerWarmth <- mean(covs.climate$summerWarmth)
mean.januaryMinTemp <- mean(covs.climate$januaryMinTemp)

# Create a sequence for each variable
precip <- seq(min(covs.climate$precip), max(covs.climate$precip), length = 100)
summerWarmth <- seq(min(covs.climate$summerWarmth), max(covs.climate$summerWarmth), length = 100)
januaryMinTemp <- seq(min(covs.climate$januaryMinTemp), max(covs.climate$januaryMinTemp), length = 100)

df.climatePrecip <- data.frame(precip = precip, summerWarmth = mean.summerWarmth, januaryMinTemp = mean.januaryMinTemp)
df.climateSummerWarmth <- data.frame(precip = mean.precip, summerWarmth = summerWarmth, januaryMinTemp = mean.januaryMinTemp)
df.climateJanuaryMinTemp <- data.frame(precip = mean.precip, summerWarmth = mean.summerWarmth, januaryMinTemp = januaryMinTemp)


predict.climatePrecip <- predict(climate, type="state", newdata=df.climatePrecip, appendData=TRUE)
predict.climateSummerWarmth <- predict(climate, type="state", newdata=df.climateSummerWarmth, appendData=TRUE)
predict.climateJanuaryMinTemp <- predict(climate, type="state", newdata=df.climateJanuaryMinTemp, appendData=TRUE)


# Part 4: Plot results: how do the model predictors influence density?  ----------------------------------------------

## Top Model - Climate & Productivity 2 (with logs) ---
# Precip
plot.climateProductivity2Precip <- ggplot(predict.climateProductivity2Precip, aes(x=precip, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("Normalized mean annual precipitation")+
  theme(axis.title.y = element_blank())

plot.climateProductivity2Precip

# Summer warmth
plot.climateProductivity2SummerWarmth <- ggplot(predict.climateProductivity2SummerWarmth, aes(x=summerWarmth, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("Normalized summer warmth index")+
  theme(axis.title.y = element_blank())

plot.climateProductivity2SummerWarmth

# Length of growing season
plot.climateProductivity2Logs <- ggplot(predict.climateProductivity2Logs, aes(x=logs, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("Length of growing season (days)")+
  theme(axis.title.y = element_blank())

plot.climateProductivity2Logs

# NDVI
plot.climateProductivity2NDVI <- ggplot(predict.climateProductivity2NDVI, aes(x=ndvi, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("NDVI")+
  theme(axis.title.y = element_blank())

plot.climateProductivity2NDVI

# Stitch plots together
library(gridExtra)
library(grid)
png("./output/figures/climateProductivity2.png", units = "in", width = 8, height = 8, res = 300)
plot.climateProductivity2 <- grid.arrange(arrangeGrob(plot.climateProductivity2Precip, plot.climateProductivity2SummerWarmth,
                                                     plot.climateProductivity2Logs, plot.climateProductivity2NDVI,
                                                     layout_matrix = matrix(c(1,2,3,4), byrow = TRUE, ncol = 2),
                                                     left = textGrob(expression(
                                                       paste("Denisty (Pika / km" ^ "2"*")")),
                                                       rot = 90,  gp = gpar(fontsize = 10))))
dev.off()


## Next Best Model - Climate & Productivity ---
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

# January Minimum Temperature
plot.climateProductivityJanuaryMinTemp <- ggplot(predict.climateProductivityJanuaryMinTemp, aes(x=januaryMinTemp, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("January minimum temperature (°C)")+
  theme(axis.title.y = element_blank())

plot.climateProductivityJanuaryMinTemp

# NDVI
plot.climateProductivityNDVI <- ggplot(predict.climateProductivityNDVI, aes(x=ndvi, y=Predicted)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_classic()+
  xlab("NDVI")+
  theme(axis.title.y = element_blank())

plot.climateProductivityNDVI

# Stitch plots together
library(gridExtra)
library(grid)
png("./output/figures/climateProductivity.png", units = "in", width = 8, height = 8, res = 300)
plot.climateProductivity <- grid.arrange(arrangeGrob(plot.climateProductivityPrecip, plot.climateProductivitySummerWarmth,
                                         plot.climateProductivityJanuaryMinTemp, plot.climateProductivityNDVI,
                                         layout_matrix = matrix(c(1,2,3,4), byrow = TRUE, ncol = 2),
                                         left = textGrob(expression(
                                           paste("Denisty (Pika / km" ^ "2"*")")),
                                           rot = 90,  gp = gpar(fontsize = 10))))
dev.off()

## Next best model - Climate ---
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


## Model tables
library(gplots)
pdf("./output/figures/explanatoryModels.pdf", width = 10, height = 4)
grid.table(expMod.Selection)
dev.off()

# # MODEL AVERAGING ---------------------------------------------------------
library(AICcmodavg)
avg.covs <- data.frame(location=transect.covs$Location,
                       precip=transect.covs$precip,
                       summerWarmth=transect.covs$summerWarmth,
                       januaryMinTemp=transect.covs$januaryMinTemp,
                       logs=transect.covs$logs,
                       ndvi=transect.covs$ndvi)

mods <- list(climate=climate, climateProductivity=climateProductivity, climateProductivity2=climateProductivity2)
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
                           logs=mean.logs, ndvi=mean.ndvi)
df.avgSummerWarmth <- data.frame(precip = mean.precip, summerWarmth=summerWarmth, januaryMinTemp=mean.januaryMinTemp,
                                 logs=mean.logs, ndvi=mean.ndvi)
df.avgJanuaryMinTemp <- data.frame(precip = mean.precip, summerWarmth=mean.summerWarmth, januaryMinTemp=januaryMinTemp,
                                    logs=mean.logs, ndvi=mean.ndvi)
df.avgLogs <- data.frame(precip = mean.precip, summerWarmth=mean.summerWarmth, januaryMinTemp=mean.januaryMinTemp,
                           logs=logs, ndvi=mean.ndvi)
df.avgNDVI <- data.frame(precip = mean.precip, summerWarmth=mean.summerWarmth, januaryMinTemp=mean.januaryMinTemp,
                            logs=mean.logs, ndvi=ndvi)
# Make predictions -----------------------------------------
predict.avgPrecip <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "lambda",
                                newdata = df.avgPrecip)
predict.avgSummerWarmth <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "lambda",
                                      newdata = df.avgSummerWarmth)
predict.avgJanuaryMinTemp <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "lambda",
                                        newdata = df.avgJanuaryMinTemp)
predict.avgLogs <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "lambda",
                                newdata = df.avgLogs)
predict.avgNDVI <- modavgPred(cand.set = mods, modnames = modNames, parm.type = "lambda",
                                newdata = df.avgNDVI)
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
df.predict.avgNDVI <- data.frame(Predicted = predict.avgNDVI$mod.avg.pred,
                                   lower = predict.avgNDVI$lower.CL,
                                   upper = predict.avgNDVI$upper.CL,
                                   df.avgNDVI)

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

# NDVI
plot.avgNDVI <- ggplot(df.predict.avgNDVI, aes(x=ndvi, y=Predicted)) +
                          geom_line(size = 1)+
                          geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
                          theme_classic()+
                          xlab("NDVI")+
                          theme(axis.title.y = element_blank())

plot.avgNDVI

# Predictions by location
predByLoc <- covs.avg %>% 
  group_by(location) %>% 
  summarize(predicted = mean(Predicted))
plot.Location <- ggplot(predByLoc, aes(x=location, y = predicted)) +
  geom_bar(stat = "identity")+
  theme_classic()+
  xlab("Site Location")+
  theme(axis.title.y = element_blank())

plot.Location

# Stitch plots together -----------------------------------
png("./output/figures/averaged.png", units = "in", width = 8, height = 10, res = 300)
plot.average <- grid.arrange(arrangeGrob(plot.avgPrecip, plot.avgSummerWarmth,
                                         plot.avgJanuaryMinTemp, plot.avgLogs,
                                         plot.avgNDVI, plot.Location,
                                         layout_matrix = matrix(c(1,1,2,2,
                                                                  3,3,4,4,
                                                                  5,5,6,6), byrow = TRUE, ncol = 4),
                                         left = textGrob(expression(
                                           paste("Denisty (Pika / km" ^ "2"*")")),
                                           rot = 90,  gp = gpar(fontsize = 10))))
dev.off()
