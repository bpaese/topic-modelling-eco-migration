########################################################################
# Master file for the data repository of the EGEI master's dissertation
# Bruno Paese
# Last updated: August 21, 2025
########################################################################

## Install packages (if necessary) and load libraries ------------------------
# source(here("code","00_PackageManagement.R"))
library(here)

## Setup - Define global variables -------------------------------------------
source(here("code","02_Setup.R"))

## Toolbox with user-written functions ---------------------------------------
source(here("code","03_Toolbox.R"))

## Metadata ------------------------------------------------------------------
source(here("code","04_Metadata.R"))

## Topic modelling -----------------------------------------------------------
source(here("code","05_TopicModelling.R"))

## Diversity indices ---------------------------------------------------------
source(here("code","06_DiversityIndices.R"))