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
# Develop a set of models varying in complexity that are hypothesized to influence species density, while using the top-performing covariate on det.prob and detection function.

null <- distsamp(~scale(search.speed) ~1, umf, keyfun="hazard", output="density", unitsOut="kmsq")
m1 <- distsamp(~scale(search.speed) ~scale(latitude), 
               umf, keyfun="hazard", output="density", unitsOut="kmsq")
m2 <- distsamp(~scale(search.speed) ~scale(slope), 
               umf, keyfun="hazard", output="density", unitsOut="kmsq")
m3 <- distsamp(~scale(search.speed) ~scale(aspect), 
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
m11 <- distsamp(~scale(search.speed) ~vegclass, 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m12 <- distsamp(~scale(search.speed) ~scale(lowshrub.cover), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m13 <- distsamp(~scale(search.speed) ~scale(veg.height), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")


fmList <- fitList(null=null, m1=m1, m2=m2, m3=m3, m4=m4, m5=m5, m6=m6, m7=m7, 
                  m8=m8, m9=m9, m10=m10, m11=m11, m12=m12, m13=m13)

modSel(fmList)


# Part 4: Multiple covariate models ---------------------------------------
m14 <- distsamp(~scale(search.speed) ~scale(latitude) + vegclass, 
               umf, keyfun="hazard", output="density", unitsOut="kmsq")
m15 <- distsamp(~scale(search.speed) ~vegclass + scale(aspect), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m16 <- distsamp(~scale(search.speed) ~vegclass + scale(summer.pcpn.mm), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m17 <- distsamp(~scale(search.speed) ~vegclass + scale(elevation), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m18 <- distsamp(~scale(search.speed) ~vegclass + scale(dist.road), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m19 <- distsamp(~scale(search.speed) ~vegclass + scale(percent.max.temp.days), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m20 <- distsamp(~scale(search.speed) ~scale(latitude) + scale(aspect), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m21 <- distsamp(~scale(search.speed) ~scale(latitude) + scale(summer.pcpn.mm), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m22 <- distsamp(~scale(search.speed) ~scale(latitude) + scale(elevation), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m23 <- distsamp(~scale(search.speed) ~scale(latitude) + scale(dist.road), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m24 <- distsamp(~scale(search.speed) ~scale(latitude) + scale(percent.max.temp.days), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m25 <- distsamp(~scale(search.speed) ~scale(aspect) + scale(summer.pcpn.mm), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m26 <- distsamp(~scale(search.speed) ~scale(aspect) + scale(elevation), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m27 <- distsamp(~scale(search.speed) ~scale(aspect) + scale(dist.road), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m28 <- distsamp(~scale(search.speed) ~scale(aspect) + scale(percent.max.temp.days), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m29 <- distsamp(~scale(search.speed) ~scale(summer.pcpn.mm) + scale(elevation), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m30 <- distsamp(~scale(search.speed) ~scale(summer.pcpn.mm) + scale(dist.road), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m31 <- distsamp(~scale(search.speed) ~scale(summer.pcpn.mm) + scale(percent.max.temp.days), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m32 <- distsamp(~scale(search.speed) ~scale(elevation) + scale(dist.road), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m33 <- distsamp(~scale(search.speed) ~scale(elevation) + scale(percent.max.temp.days), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
m34 <- distsamp(~scale(search.speed) ~scale(dist.road) + scale(percent.max.temp.days), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")






fmList <- fitList(null=null, m1=m1, m2=m2, m3=m3, m4=m4, m5=m5, m6=m6, m7=m7, 
                  m8=m8, m9=m9, m10=m10, m11=m11, m12=m12, m13=m13, m14=m14, 
                  m15=m15, m16=m16, m17=m17, m18=m18, m19=m19, m20=m20, m21=m21,
                  m22=m22, m23=m23, m24=m24, m25=m25, m26=m26, m27=m27, m28=m28,
                  m29=m29, m30=m30, m31=m31, m32=m32, m33=m33, m34=m34)

modSel(fmList)

# Part 4: Parameter Estimates – look for significance ---------------------
summary(m11)
confint(m11, type = "state")

summary(m1)
confint(m1, type = "state")

summary(m9)
confint(m9, type = "state")

summary(m3)
confint(m3, type = "state")

summary(m6)
confint(m6, type = "state")

summary(m8)
confint(m8, type = "state") # Positive effect of tmax days on density... strange

summary(m10)
confint(m10, type = "state") # Overlaps zero

summary(m4)
confint(m4, type = "state") # Overlaps zero

summary(m5)
confint(m5, type = "state") # Overlaps zero

summary(null)
confint(null, type = "state") # Overlaps zero

summary(m12)
confint(m12, type = "state") # Overlaps zero

summary(m7)
confint(m7, type = "state") # Overlaps zero

summary(m13)
confint(m13, type = "state") # Overlaps zero

summary(m2)
confint(m2, type = "state") # Overlaps zero

# Part 5: Final Model Set: Single covariate & additive models  -------------------------------------------------

# Final Model List:

# Top single covariate models with non-overlapping estimates:
fm1 <- distsamp(~scale(search.speed) ~vegclass, 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm2 <- distsamp(~scale(search.speed) ~scale(latitude), 
               umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm3 <- distsamp(~scale(search.speed) ~scale(summer.pcpn.mm), 
               umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm4 <- distsamp(~scale(search.speed) ~scale(aspect), 
               umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm5 <- distsamp(~scale(search.speed) ~scale(summer.tmax), 
               umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm6 <- distsamp(~scale(search.speed) ~scale(percent.tmax.days), 
               umf, keyfun="hazard", output="density", unitsOut="kmsq")

# 2 Covariate models

fm7 <- distsamp(~scale(search.speed) ~vegclass + scale(latitude), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm8 <- distsamp(~scale(search.speed) ~vegclass + scale(summer.pcpn.mm), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm9 <- distsamp(~scale(search.speed) ~vegclass + scale(aspect), 
                umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm10 <- distsamp(~scale(search.speed) ~vegclass + scale(summer.tmax), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm11 <- distsamp(~scale(search.speed) ~vegclass + scale(percent.tmax.days), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm12 <- distsamp(~scale(search.speed) ~scale(latitude) + scale(summer.pcpn.mm), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm13 <- distsamp(~scale(search.speed) ~scale(latitude) + scale(aspect), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm14 <- distsamp(~scale(search.speed) ~scale(latitude) + scale(summer.tmax), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm15 <- distsamp(~scale(search.speed) ~scale(latitude) + scale(percent.tmax.days), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm16 <- distsamp(~scale(search.speed) ~(summer.pcpn.mm) + scale(aspect), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm17 <- distsamp(~scale(search.speed) ~(summer.pcpn.mm) + scale(summer.tmax), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm18 <- distsamp(~scale(search.speed) ~(summer.pcpn.mm) + scale(percent.tmax.days), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm19 <- distsamp(~scale(search.speed) ~scale(aspect) + scale(summer.tmax), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm20 <- distsamp(~scale(search.speed) ~scale(aspect) + scale(percent.tmax.days), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm21 <- distsamp(~scale(search.speed) ~scale(summer.tmax) + scale(percent.tmax.days), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")

# 3 Covariate models

fm22 <- distsamp(~scale(search.speed) ~vegclass + scale(latitude) + scale(summer.pcpn.mm), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm23 <- distsamp(~scale(search.speed) ~vegclass + scale(latitude) + scale(aspect), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm24 <- distsamp(~scale(search.speed) ~vegclass + scale(latitude) + scale(summer.tmax), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm25 <- distsamp(~scale(search.speed) ~vegclass + scale(latitude) + scale(percent.tmax.days), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm26 <- distsamp(~scale(search.speed) ~vegclass + scale(summer.pcpn.mm) + scale(aspect), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm27 <- distsamp(~scale(search.speed) ~vegclass + scale(summer.pcpn.mm) + scale(summer.tmax), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm28 <- distsamp(~scale(search.speed) ~vegclass + scale(summer.pcpn.mm) + scale(percent.tmax.days), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm29 <- distsamp(~scale(search.speed) ~vegclass + scale(aspect) + scale(summer.tmax), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm30 <- distsamp(~scale(search.speed) ~vegclass + scale(aspect) + scale(percent.tmax.days), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm31 <- distsamp(~scale(search.speed) ~vegclass + scale(summer.tmax) + scale(percent.tmax.days), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm32 <- distsamp(~scale(search.speed) ~scale(latitude) + scale(summer.pcpn.mm) + scale(aspect), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm33 <- distsamp(~scale(search.speed) ~scale(latitude) + scale(summer.pcpn.mm) + scale(summer.tmax), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm34 <- distsamp(~scale(search.speed) ~scale(latitude) + scale(summer.pcpn.mm) + scale(percent.tmax.days), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm35 <- distsamp(~scale(search.speed) ~scale(latitude) + scale(aspect) + scale(summer.tmax), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm36 <- distsamp(~scale(search.speed) ~scale(latitude) + scale(aspect) + scale(percent.tmax.days), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm37 <- distsamp(~scale(search.speed) ~scale(latitude) + scale(summer.tmax) + scale(percent.tmax.days), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm38 <- distsamp(~scale(search.speed) ~scale(summer.pcpn.mm) + scale(aspect) + scale(summer.tmax), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm39 <- distsamp(~scale(search.speed) ~scale(summer.pcpn.mm) + scale(aspect) + scale(percent.tmax.days), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm40 <- distsamp(~scale(search.speed) ~scale(summer.pcpn.mm) + scale(summer.tmax) + scale(percent.tmax.days), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")
fm41 <- distsamp(~scale(search.speed) ~scale(aspect) + scale(summer.tmax) + scale(percent.tmax.days), 
                 umf, keyfun="hazard", output="density", unitsOut="kmsq")


fmList <- fitList(fm1=fm1, fm2=fm2, fm3=fm3, fm4=fm4, fm5=fm5, fm6=fm6, fm7=fm7,
                  fm8=fm8, fm9=fm9, fm10=fm10, fm11=fm11, fm12=fm12, fm13=fm13,
                  fm14=fm14, fm15=fm15, fm16=fm16, fm17=fm17, fm18=fm18, fm19=fm19,
                  fm20=fm20, fm21=fm21, fm22=fm22, fm23=fm23, fm24=fm24, fm25=fm25,
                  fm26=fm26, fm27=fm27, fm28=fm28, fm29=fm29, fm30=fm30, fm31=fm31,
                  fm32=fm32, fm33=fm33, fm34=fm34, fm35=fm35, fm36=fm36, fm37=fm37,
                  fm38=fm38, fm39=fm39, fm40=fm40, fm41=fm41)

modSel(fmList)

# Mean summer temperature (m3) is the best performing model, with the global model (m4) a close second.
# Since the AIC values are so close, both models are worth interpreting. Next, we will examine model fit.
