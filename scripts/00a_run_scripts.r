
setwd("[omitted]")

local({
  r <- list("cran" = "[omitted]")
  options(repos = r)
})

library(data.table)
library(RMySQL)

rm(list = ls())

cohort_years <- 1997:2022

sql_channel <-
  dbConnect(
    RMySQL::MySQL(),
    username = "[omitted]",
    password = .rs.askForPassword("log in CPRU SQL server"),
    host = "[omitted]",
    port = omitted,
    dbname = "[omitted]"
  )

source("scripts/00b_functions.r")
source("scripts/01_extract_delivery_episodes.r")
source("scripts/02_initial_cleaning.r")
source("scripts/03_initial_processing.r")
source("scripts/04_identify_delivery_episode.r")
source("scripts/05_unassigned_episodes.r")
source("scripts/06_get_ethnicity.r")
source("scripts/07_clean_maternity_tail.r")
source("scripts/08_birth_status.r")
source("scripts/09_numpreg_birthorder.r")
source("scripts/10_get_apc.r")
source("scripts/11_implement_phenotypes.r")
source("scripts/12_get_mortality.r")
source("scripts/13_get_region.r")
source("scripts/14_exclusions.r")
source("scripts/15_tidy_and_save.r")
