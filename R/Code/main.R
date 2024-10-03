### Reproducible Research Fundamentals -  Main R Script

# Load libraries ---- 

# Load necessary libraries
library(haven)  # for reading .dta files
library(dplyr)  # for data manipulation
library(tidyr)  # for reshaping data
library(stringr) # work with strings
library(labelled) # use labels
library(gtsummary) # tables
library(gt) # tables
library(ggplot2) #graphs
library(tidyverse) # working with tidy data
library(modelsummary) # creating summary tables
library(stargazer) # writing nice tables
library(RColorBrewer)# color palettes
library(estimatr)

# Recover environment ----
# new users need to restore the env. by doing 
# you only need to do this the first time you interact with the project 
# renv::restore()
data_path <- "C:\\Users\\sagrawal9\\Downloads\\DataWork\\Data"

data <- read_dta(file.path(data_path, "Raw/TZA_CCT_baseline.dta"))

# this is the second root of the project, the first root is the code whose directory 
# is already being handled by the rstudio project.



# Run the R scripts ----

source("Code/01-processing-data.R")
source("Code/02-constructing-data.R")
source("Code/03-analyzing-data.R")

