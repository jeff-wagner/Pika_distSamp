# Pika distance sampling initialization script

# To be sourced in at the beginning of every script

## GITHUB PAC: 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

# Set working directory ---------------------------------------------------------
# Set the working directory to the 'pika_distsamp' folder so we can access both the 
# 'data' and 'scripts' subfolders
getwd()
# setwd("C:/Users/jeffw/GitHub/Pika_distSamp")

# Load packages -----------------------------------------------------------------

library(dplyr)
library(data.table)
library(tidyverse)
library(unmarked)
library(lubridate)
library(ggplot2)
library(readxl)
library(psych)
library(snotelr)
library(sf)
library(sp)
library(mapview)
library(lwgeom)
library(AICcmodavg)


# Set path to Excel workbook that contains the data -----------------------------
path <- "./data/pika.distance.sampling.11.8.2019.xlsx"