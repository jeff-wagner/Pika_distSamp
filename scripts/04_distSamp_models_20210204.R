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
# Use the 'scale' command for continuous variables to improve model fit and to allow comparisons of coefficients 
# on a standardized scale.
m.det1 <- distsamp(~scale(search.speed) ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
m.det2 <- distsamp(~observer ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
m.det3 <- distsamp(~Observer ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
m.det4 <- distsamp(~scale(tempc) ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
m.det5 <- distsamp(~scale(windms) ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
m.det6 <- distsamp(~scale(start.hr) ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
m.det7 <- distsamp(~scale(roughness) ~1, umf, keyfun ="hazard", output="density", unitsOut = "kmsq")

fmList <- fitList(m.det1=m.det1, m.det2=m.det2, m.det3=m.det3, m.det4=m.det4, m.det5=m.det5, m.det6=m.det6, m.det7=m.det7)
modSel(fmList)

# Part 3: Single Covariate Models  ---------------------------------------------------------------------------
# Develop a set of models varying in complexity that are hypothesized to influence species density, while using 
# the top-performing covariate on det.prob and detection function.

null <- distsamp(~scale(search.speed) ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
phys1 <- distsamp(~scale(search.speed) ~scale(latitude), umf, keyfun="hazard", output="density", unitsOut="kmsq")
phys2 <- distsamp(~scale(search.speed) ~scale(slope), umf, keyfun="hazard", output="density", unitsOut="kmsq")
phys3 <- distsamp(~scale(search.speed) ~scale(aspect), umf, keyfun="hazard", output="density", unitsOut="kmsq")
phys4 <- distsamp(~scale(search.speed) ~scale(elevation), umf, keyfun="hazard", output="density", unitsOut="kmsq")  
phys5 <- distsamp(~scale(search.speed) ~scale(roughness), umf, keyfun="hazard", output="density", unitsOut="kmsq")
phys6 <- distsamp(~scale(search.speed) ~scale(talus), umf, keyfun="hazard", output="density", unitsOut="kmsq")
phys7 <- distsamp(~scale(search.speed) ~scale(dist.road), umf, keyfun="hazard", output="density", unitsOut="kmsq")

env1 <- distsamp(~scale(search.speed) ~scale(day.of.year), umf, keyfun="hazard", output="density", unitsOut="kmsq")
env2 <- distsamp(~scale(search.speed) ~scale(veg.height), umf, keyfun="hazard", output="density", unitsOut="kmsq")
env3 <- distsamp(~scale(search.speed) ~scale(lowshrub.cover), umf, keyfun="hazard", output="density", unitsOut="kmsq")
env4 <- distsamp(~scale(search.speed) ~vegclass, umf, keyfun="hazard", output="density", unitsOut="kmsq")

temp1 <- distsamp(~scale(search.speed) ~scale(summerTemp), umf, keyfun="hazard", output="density", unitsOut="kmsq")
temp2 <- distsamp(~scale(search.speed) ~scale(winterTemp), umf, keyfun="hazard", output="density", unitsOut="kmsq")

fmList <- fitList(null=null, phys1=phys1, phys2=phys2, phys3=phys3, phys4=phys4, phys5=phys5, 
                  phys6=phys6, phys7=phys7,
                  env1=env1, env2=env2, env3=env3, env4=env4,
                  temp1=temp1, temp2=temp2)

modSel(fmList)


# Part 4: Parameter Estimates – look for significance ---------------------
summary(temp1)
confint(temp1, type = "state")

summary(env4)
confint(env4, type = "state")

summary(phys1)
confint(phys1, type = "state")

summary(env1)
confint(env1, type = "state")

summary(phys3)
confint(phys3, type = "state")

summary(phys4)
confint(phys4, type = "state") # Overlaps zero

summary(phys7)
confint(phys7, type = "state") # Overlaps zero

summary(null)
confint(null, type = "state") # Overlaps zero

summary(env3)
confint(env3, type = "state") # Overlaps zero

summary(env2)
confint(env2, type = "state") # Overlaps zero

summary(temp2)
confint(temp2, type = "state") # Overlaps zero

summary(phys2)
confint(phys2, type = "state") # Overlaps zero

summary(phys5)
confint(phys5, type = "state") # Overlaps zero

summary(phys6)
confint(phys6, type = "state") # Overlaps zero

# Part 5: Final Model Set: Single covariate & additive models  -------------------------------------------------

# Final Model List:
# m1 <- distsamp(~scale(search.speed) ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
# m2 <- distsamp(~scale(search.speed) ~scale(latitude), umf, keyfun="hazard", output="density", unitsOut="kmsq")
# m3 <- distsamp(~scale(search.speed) ~scale(day.of.year)+scale(windms)+scale(eds), umf, keyfun="hazard", output="density", unitsOut="kmsq")
# m4 <- distsamp(~scale(search.speed) ~scale(latitude)+scale(day.of.year)+scale(windms)+scale(eds), umf, keyfun="hazard", output="density", unitsOut="kmsq") # global model

# m1 <- distsamp(~scale(search.speed) ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
# m2 <- distsamp(~scale(search.speed) ~scale(latitude), umf, keyfun="hazard", output="density", unitsOut="kmsq")
m3 <- distsamp(~scale(search.speed) ~scale(summerTemp), umf, keyfun="hazard", output="density", unitsOut="kmsq")
# m4 <- distsamp(~scale(search.speed) ~scale(latitude) + scale(summerTemp), umf, keyfun="hazard", output="density", unitsOut="kmsq")
# m5 <- distsamp(~scale(search.speed) ~scale(latitude^2), umf, keyfun="hazard", output="density", unitsOut="kmsq")
# m6 <- distsamp(~scale(search.speed) ~scale(latitude^2) + scale(summerTemp), umf, keyfun="hazard", output="density", unitsOut="kmsq")
# 
# fmList <- fitList(m1=m1, m2=m2, m3=m3, m4=m4, m5=m5, m6=m6)
# 
# modSel(fmList)

# Mean summer temperature (m3) is the best performing model, with the global model (m4) a close second.
# Since the AIC values are so close, both models are worth interpreting. Next, we will examine model fit.
