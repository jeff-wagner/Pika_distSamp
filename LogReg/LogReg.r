# Collared pika occupancy & abundance surveys in Alaska.
# Data collected July - Sep 2018 and July - Aug 2019
# Author: J. Wagner
# Last updated: 16 Nov 2021

###############################################################-#

###         Logistic Regression of Historical Sites           ###

###############################################################-#

# This script conducts a simple logistic regression of occupied vs. unoccupied historical sites.

# Read in the initialization script (reads in required packages, etc)
source("LogReg/initscript.r")

# Read in the data
siteOcc <- read.csv(path)

# We are only interested in historically occupied sites for the purposes of this analysis:
siteOcc <- siteOcc %>% 
  filter(random_site == 0)

# Read in site (transect) covariates from the distance sampling analysis
source("scripts/02.3_covariates.R")

# Filter to keep only one row per site & join to siteOcc
transect.covs <- transect.covs %>% 
  select(-(obs1.4:search.speed), -latitude, -longitude) %>% # get rid of transect-specific columns
  distinct() %>% # filter out duplicate rows
  filter(Site %in% unique(siteOcc$Site)) # keep only historical sites

siteOcc <- siteOcc %>% 
  left_join(., transect.covs, by = c("Location", "Site", "Year"))

# Create indicator variable for occupied status
siteOcc$occIndicator <- ifelse(siteOcc$occ.status=="occupied", 1, 0)

# Write out site-level data with covariates
# save(siteOcc, file = "./LogReg/siteOcc.RDS")
# write.csv(siteOcc, file = "./LogReg/siteOcc.csv")


## Bayesian model, this is JAGS code
modSummerTemp <- "
model{
    for(i in 1:length(y)){
        y[i] ~dbern(p[i])
        logit(p[i]) = beta0 + beta1*x[i]        
    }
    beta0~dnorm(0, 0.001)
    beta1~dnorm(0, 0.001)

}
"
sT.dataList <- list(y = siteOcc$occIndicator,
                  x = siteOcc$summer.tmax)

params <- c('beta0','beta1')

## initial values for the MCMC
inits <- NULL

## Run the Bayesian model using MCMC
output <- jags(data = sT.dataList,
               inits = inits,
               parameters.to.save = params,
               model.file = textConnection(modSummerTemp),
               n.chains = 3,
               n.adapt = 5000,
               n.iter = 11000,
               n.burnin = 1000,
               n.thin = 1,
               parallel = FALSE,
               DIC = FALSE)

output

