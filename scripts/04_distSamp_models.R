# Collared pika abundance surveys in Alaska.
# Data collected July - Sep 2018 and July - Aug 2019
# Author: J. Wagner, P. Schuette
# Last updated: 16 Nov 2020

###############################################################-#

###      Step 4: Distance Sampling: Candidate Models          ###

###############################################################-#

# This script builds distance sampling models in unmarked and performs model selection using AIC.

# We will use a simple set of models with the hazard detection function, constant detection probability, 
# and single covariate and additive models.Each model represents an a priori hypothesis we are testing.
# umf is the unmarked data frame, and we want the output as density in square km.

# Read in the observation and covariate data from our data management scripts.
source("scripts/03_distSamp_formatting.r")

# View the data one more time to verify that each row contains the transect observation data and matching 
# transect-level covariates. ** Make sure the first column matches the Location, Site, and transect columns. **
umf

# Part 1: Detection Function Models  -----------------------------------------------------------------------
# Explore the best detection function option: halfnorm, hazard, or uniform. Select via AIC.
m.halfnorm <- distsamp(~1 ~1, umf, keyfun="halfnorm", output="density", unitsOut="kmsq")
m.hazard <- distsamp(~1 ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
m.uniform <- distsamp(~1 ~1, umf, keyfun="uniform", output="density", unitsOut="kmsq")

fmList <- fitList(m.halfnorm=m.halfnorm, m.hazard=m.hazard, m.uniform=m.uniform)

# Typical model selection with fmList based on AIC scores
modSel(fmList)

# Part 2: Detection Probability Models  --------------------------------------------------------------------
# Explore covariates that hypothesize might influence detection probability.
# Use the 'scale' command for continuous variables to improve model fit and to allow comparisons of coefficients on a standardized scale.
m.det1 <- distsamp(~scale(search.speed) ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
m.det2 <- distsamp(~observer ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
m.det3 <- distsamp(~Observer ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
m.det4 <- distsamp(~scale(tempc) ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
m.det5 <- distsamp(~scale(windms) ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
m.det6 <- distsamp(~scale(start.hr) ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
m.det7 <- distsamp(~scale(day.of.year) ~1, umf, keyfun ="hazard", output="density", unitsOut = "kmsq")

fmList <- fitList(m.det1=m.det1, m.det2=m.det2, m.det3=m.det3, m.det4=m.det4, m.det5=m.det5, m.det6=m.det6, m.det7=m.det7)
modSel(fmList)

# Part 3: Single Covariate Models  ---------------------------------------------------------------------------
# Test single covariates for significance. 

null <- distsamp(~scale(search.speed) ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
m1 <- distsamp(~scale(search.speed) ~scale(latitude), 
               umf, keyfun="hazard", output="density", unitsOut="kmsq")
m2 <- distsamp(~scale(search.speed) ~scale(slope), 
               umf, keyfun="hazard", output="density", unitsOut="kmsq")
m3 <- distsamp(~scale(search.speed) ~scale(northness), 
               umf, keyfun="hazard", output="density", unitsOut="kmsq")
m4 <- distsamp(~scale(search.speed) ~scale(elevation), 
               umf, keyfun="hazard", output="density", unitsOut="kmsq")  
m5 <- distsamp(~scale(search.speed) ~scale(dist.road), 
               umf, keyfun="hazard", output="density", unitsOut="kmsq")
m6 <- distsamp(~scale(search.speed) ~scale(summer.tmax), 
               umf, keyfun="hazard", output="density", unitsOut="kmsq")
m7 <- distsamp(~scale(search.speed) ~scale(winter.tmin), 
               umf, keyfun="hazard", output="density", unitsOut="kmsq")
m8 <- distsamp(~scale(search.speed) ~scale(percent.tmax.days), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
m9 <- distsamp(~scale(search.speed) ~scale(summer.pcpn.mm), 
               umf, keyfun="hazard", output="density", unitsOut="kmsq")
m10 <- distsamp(~scale(search.speed) ~scale(winter.pcpn.mm), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m11 <- distsamp(~scale(search.speed) ~scale(eastness), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m12 <- distsamp(~scale(search.speed) ~scale(lowshrub.cover), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m13 <- distsamp(~scale(search.speed) ~scale(veg.height), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")


fmList <- fitList(null=null, m1=m1, m2=m2, m3=m3, m4=m4, m5=m5, m6=m6, m7=m7, 
                  m8=m8, m9=m9, m10=m10, m11=m11, m12=m12, m13=m13)

modSel(fmList)

# Part 4: Parameter Estimates – look for significance ---------------------
summary(m1)
confint(m1, type = "state")

summary(m9)
confint(m9, type = "state")

summary(m6)
confint(m6, type = "state")

summary(m8)
confint(m8, type = "state") # We will omit this as it's usefulness for AK is questionable

summary(m10)
confint(m10, type = "state") # Overlaps zero

summary(m4)
confint(m4, type = "state") # Overlaps zero

summary(m5)
confint(m5, type = "state") # Overlaps zero

summary(m11)
confint(m11, type = "state") # Overlaps zero

summary(null)
confint(null, type = "state") 

summary(m12)
confint(m12, type = "state") # Overlaps zero

summary(m7)
confint(m7, type = "state") # Overlaps zero

summary(m13)
confint(m13, type = "state") # Overlaps zero

summary(m2)
confint(m2, type = "state") # Overlaps zero

summary(m3)
confint(m3, type = "state") # Overlaps zero

# Part 5: Final Model Set: Single covariate & additive models  -------------------------------------------------

# Use the MumIn package to build final model list using the six covariates identified above.
# The pdredge function will automatically build models for all possible combinations and rank them
# according to AICc

library(MuMIn)
library(parallel)
library(snow)

full <- distsamp(~scale(search.speed) ~scale(latitude) + scale(summer.pcpn.mm) +
                 scale(summer.tmax), umf, keyfun="hazard",
                 output="density", unitsOut="kmsq")

# Set up cluster
no_cores <- detectCores()-1
clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
clust <- try(makeCluster(getOption("cl.cores", no_cores), type = clusterType))
clusterEvalQ(clust, library(unmarked))
clusterExport(clust, "umf")

modelList <- pdredge(full, clust, rank = "AICc", extra = "adjR^2", fixed = "p(shapescale(search.speed))")

par(mfrow = c(1,1), pty = "s", mai = c(0.5,0.2,3,0.2))
plot(modelList)

# Save all models with a delta < 4
modelList.sub <- subset(modelList, delta <= 4, recalc.weights = TRUE)
models <- get.models(modelList, delta < 4, clust)

stopCluster(clust)

rm(list=setdiff(ls(), c("umf", "modelList", "modelList.sub", "models", "transect.covs")))
   