# Pika logistic regression (historical occupancy) initialization script

# To be sourced in at the beginning of every script

# Set working directory ---------------------------------------------------------
# No need to set wd in Rproject
getwd()

# Load packages -----------------------------------------------------------------

library(dplyr)
library(data.table)
library(tidyverse)
library(jagsUI)
library(rjags)
library(ggmcmc)


# Set path to Excel workbook that contains the data -----------------------------
path <- "./data/pika_site_occupation.csv"