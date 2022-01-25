# Pika logistic regression (historical occupancy) initialization script

# To be sourced in at the beginning of every script

## GITHUB PAC: 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

# Set working directory ---------------------------------------------------------
# No need to set wd in Rproject
getwd()

# Load packages -----------------------------------------------------------------

library(dplyr)
library(data.table)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readxl)
library(psych)
library(sf)
library(sp)
library(mapview)
library(lwgeom)


# Set path to Excel workbook that contains the data -----------------------------
path <- "./data/pika_site_occupation.csv"