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

# Read in site covariates from the distance sampling analysis
source("scripts/02.3_covariates.R")
