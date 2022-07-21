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

# Assess multicolinearity
vif(climate1, type = "state")
climate1Covs <- data.frame(precip=transect.covs$precip,
                           summerWarmth=transect.covs$summerWarmth)
climate1Covs$bogus <- runif(119)
library(car)
m.vif <- lm(bogus ~ scale(precip) + scale(summerWarmth), data = climate1Covs)
vif(m.vif)
detach("package:car")

# Function returning three fit-statistics.
fitstats <- function(climate1) {
  observed <- getY(climate1@data)
  expected <- fitted(climate1)
  resids <- residuals(climate1)
  sse <- sum(resids^2)
  chisq <- sum((observed - expected)^2 / expected)
  freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
  out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
  return(out)
}
(pb.climate1 <- parboot(climate1, fitstats, nsim=500, report=1))

# Part 2: Predicted transect-level density estimates  --------------------------------------------------------------------
# Use the predict function to get estimates of density (type='state' indicates you want density) using coefficients
# from the best-supported models, combined with the covariate values for each transect.
climate1Pred <- predict(climate1, type='state', newdata=climate1Covs, appendData=TRUE)
summary(climate1Pred)

climate1Pred <- climate1Pred %>% 
  arrange(Predicted)

hist(climate1Pred$Predicted)

# Export an excel file that you can load into ArcGIS for making a pretty map
# write.csv(climate1Pred, file="output/climate1_density.csv")

# Part 3: Predictions for explanatory variables  ---------------------------------------------------------------------
# climate1 ------------------
# Define the dataframe as you have done already. But create a sequence of values within the range you had at your sites.
meanPrecip <- mean(climate1Covs$precip)
meanSummerWarmth <- mean(climate1Covs$summerWarmth)


# Create a sequence for each variable
precip <- seq(min(climate1Covs$precip), max(climate1Covs$precip), length = 119)
summerWarmth <- seq(min(climate1Covs$summerWarmth), max(climate1Covs$summerWarmth), length = 20)

climate1Precip <- data.frame(precip = precip, summerWarmth = meanSummerWarmth)
climate1SummerWarmth <- data.frame(precip = meanPrecip, summerWarmth = summerWarmth)

climate1PrecipPredict <- predict(climate1, type="state", newdata=climate1Precip, appendData=TRUE)
climate1SummerWarmthPredict <- predict(climate1, type="state", newdata=climate1SummerWarmth, appendData=TRUE)

# Part 4: Plot results: how do the model predictors influence density?  ----------------------------------------------
# Climate1 model, precip -----------
plot.climate1Precip <- ggplot(climate1PrecipPredict, aes(x=precip, y=Predicted)) +
                              geom_line(size = 1)+
                              geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
                              theme_classic()+
                              xlab("Normalized mean annual precipitation")+
                              theme(axis.title.y = element_blank())

plot.climate1Precip

# Climate1 model, summer warmth -----------
plot.climate1SummerWarmth <- ggplot(climate1SummerWarmthPredict, aes(x=summerWarmth, y=Predicted)) +
                                    geom_line(size = 1)+
                                    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
                                    theme_classic()+
                                    xlab("Normalized summer warmth index")+
                                    theme(axis.title.y = element_blank())

plot.climate1SummerWarmth

# Stitch plots together
library(gridExtra)
library(grid)
png("./output/figures/climate1.png", units = "in", width = 8, height = 5, res = 300)
plot.climate1 <- grid.arrange(arrangeGrob(plot.climate1Precip, plot.climate1SummerWarmth,
                                          layout_matrix = matrix(c(1,2), byrow = TRUE, ncol = 2),
                                          left = textGrob(expression(
                                            paste("Denisty (Pika / km" ^ "2"*")")),
                                                         rot = 90,  gp = gpar(fontsize = 10))))
dev.off()

library(gplots)
pdf("./output/figures/explanatoryModels.pdf", width = 8, height = 6)
grid.table(expMod.Selection)
dev.off()

# MODEL AVERAGING ---------------------------------------------------------
library(AICcmodavg)
avg.covs <- data.frame(aspect=transect.covs$aspect,
                       latitude=transect.covs$latitude,
                       vegclass=transect.covs$vegclass,
                       summer.pcpn.mm=transect.covs$summer.pcpn.mm,
                       summer.tmax=transect.covs$summer.tmax,
                       transect=transect.covs$transect,
                       Location=transect.covs$Location)
fm2 <- models[[2]]
fm3 <- models[[3]]
fm4 <- models[[4]]
mods <- list(fm1,fm2, fm3, fm4)

Modnames <- paste("mod", 1:length(mods), sep = "")
modavg.pred <- modavgPred(cand.set = mods, modnames = Modnames, parm.type = "lambda",
                          newdata=avg.covs)

modavg.pred_df <- data.frame(Predicted = modavg.pred$mod.avg.pred,
                                         lower = modavg.pred$lower.CL,
                                         upper = modavg.pred$upper.CL,
                                         avg.covs)

# Part 3: Predictions for explanatory variables  ---------------------------------------------------------------------
# fm1 ------------------
# Define the dataframe as you have done already. But create a sequence of values within the range you had at your sites.
meanaspect <- mean(avg.covs$aspect)
meanlatitude <- mean(avg.covs$latitude)
meansummerpcpn <- mean(avg.covs$summer.pcpn.mm)
meansummertmax <- mean(avg.covs$summer.tmax)


# Create a sequence for each variable
aspect <- seq(min(avg.covs$aspect), max(avg.covs$aspect), length = 119)
latitude <- seq(min(avg.covs$latitude), max(avg.covs$latitude), length = 119)
summerpcpn <- seq(min(avg.covs$summer.pcpn.mm), max(avg.covs$summer.pcpn.mm), length = 119)
summertmax <- seq(min(avg.covs$summer.tmax), max(avg.covs$summer.tmax), length = 119)
vegclass <- factor("eds", levels=c("eds","dds","dgh","lic", "ls", "ts"))

avg.aspect <- data.frame(aspect = aspect, latitude = meanlatitude, summer.pcpn.mm = meansummerpcpn, 
                         summer.tmax = meansummertmax, vegclass = vegclass, transect = avg.covs$transect,
                         Location = avg.covs$Location)
avg.latitude <- data.frame(aspect = meanaspect, latitude = latitude, summer.pcpn.mm = meansummerpcpn, 
                           summer.tmax = meansummertmax, vegclass = vegclass, transect = avg.covs$transect,
                           Location = avg.covs$Location)
avg.vegclass <- data.frame(aspect = meanaspect, latitude = meanlatitude, summer.pcpn.mm = meansummerpcpn, 
                           summer.tmax = meansummertmax, vegclass = avg.covs$vegclass, transect = avg.covs$transect,
                           Location = avg.covs$Location)
avg.summerpcpn <- data.frame(aspect = meanaspect, latitude = meanlatitude, summer.pcpn.mm = summerpcpn, 
                             summer.tmax = meansummertmax, vegclass = vegclass, transect = avg.covs$transect,
                             Location = avg.covs$Location)
avg.summertmax <- data.frame(aspect = meanaspect, latitude = meanlatitude, summer.pcpn.mm = meansummerpcpn, 
                             summer.tmax = summertmax, vegclass = vegclass, transect = avg.covs$transect,
                             Location = avg.covs$Location)

avg.aspect.predict <- modavgPred(cand.set = mods, modnames = Modnames, parm.type = "lambda",
                                 newdata=avg.aspect)
avg.latitude.predict <- modavgPred(cand.set = mods, modnames = Modnames, parm.type = "lambda",
                                   newdata=avg.latitude)
avg.vegclass.predict <- modavgPred(cand.set = mods, modnames = Modnames, parm.type = "lambda",
                                   newdata=avg.vegclass)
avg.summerpcpn.predict <- modavgPred(cand.set = mods, modnames = Modnames, parm.type = "lambda",
                                     newdata=avg.summerpcpn)
avg.summertmax.predict <- modavgPred(cand.set = mods, modnames = Modnames, parm.type = "lambda",
                                     newdata=avg.summertmax)

avg.summerpcpn.pred_df <- data.frame(Predicted = avg.summerpcpn.predict$mod.avg.pred,
                                       lower = avg.summerpcpn.predict$lower.CL,
                                       upper = avg.summerpcpn.predict$upper.CL,
                                       avg.summerpcpn)
