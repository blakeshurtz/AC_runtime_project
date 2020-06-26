#this primary script runs all of the scripts used in the analysis 
#including: data wrangling, modeling and charts
#recommended that you open the R Project File "AC_runtime_project"
#root directory is "/AC_runtime_project
getwd()
#scripts may change directories but will revert to the root directory

#set seed and load libraries
set.seed(38462346) #as always, smushing all keys simultaneously
library(tidyverse)
library(lubridate)
library(readxl)
library(broom)
library(extraDistr)
library(rstan)
library(shinystan)
library(gridExtra)

#load raw data, wrangle and create dataset "ccl.csv"
source('scripts/01-create-data-set.r')

#fit individual effects model in stan
source('scripts/02-individual-effects-model.R')
#alternatively, you can import the model
#ind_m <- readRDS(file="models/individual_effects_model.rds")

#visualize individual effects model
source('scripts/03-plot-individual-effects.R')

#fit multi-level model in stan
source('scripts/04-multi-level-model.R')

#alternatively, you can import the model
#ml_m <- readRDS(file="models/multi_level_model.rds")

#diagnose multi-level model
#launch_shinystan(ml_m)

#visualize individual effects model
source('scripts/05-plot-multi-level-model.R')
