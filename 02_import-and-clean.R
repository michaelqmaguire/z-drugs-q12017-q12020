#-----------------------------------------------------------------------------------------------------------------#
#                                                                                                                 #
# PROJECT: SDUD Z-DRUGS 2017-2020	                                                                                #
# AUTHOR: MICHAEL MAGUIRE, MS, DMA II                                                                             #
# INSTITUTION: UNIVERSITY OF FLORIDA, COLLEGE OF PHARMACY                                                         #
# DEPARTMENT: PHARMACEUTICAL OUTCOMES AND POLICY                                                                  #
# SUPERVISORS: AMIE GOODIN, PHD, MPP | JUAN HINCAPIE-CASTILLO, PHARMD, PHD, MS                                    #
# SCRIPT: 02_import-and-clean.R                                                                			              #
#                                                                                                                 #
#-----------------------------------------------------------------------------------------------------------------#

library(data.table)
library(tidyverse)

## Create 'states' file containing all 50 states and their abbreviations.

states <-
  bind_cols(
    state.name,
    state.abb
  ) %>%
  rename(
    "state" = `...1`,
    "abbreviation" = `...2`
  )

## Read in base file.

base <-
  fread(
    file_location,
    colClasses = c("proper_ndc" = "character")
  )

## Read in SD file.

rxSD <-
  readxl::read_xlsx(
    path = "./data/raw/Z drugs raw data 2020.xlsx",
    col_names = TRUE
  ) %>%
  select(seqidndc)

rxSD %>%
  janitor::get_dupes(seqidndc)

## Store seqIDNDC into a vector.

rxSeq <-
  rxSD %>%
  select(seqidndc) %>%
  as_vector()

## Extract years of interest, drugs of interest, and states of interest.

yrSt <-
  base[
    i = year %in% c(2017:2020) & state != "GA" & seqidndc %in% rxSeq,
    j = .(year, proper_ndc, quarter, state, suppression, numberrx, prodnme, gennme)
  ]

## Checking years and quarters.

yrSt %>%
  distinct(year, quarter) %>%
  arrange(year, desc(quarter))

## Aggregate by year, state, quarter, and whether values were suppressed.

drugsAggState <-
  yrSt[
    i  = ,
    j  = .(totalRX = sum(numberrx)),
    by = c("year", "state", "quarter", "suppression")
  ]

setorder(drugsAggState, year, state, quarter, suppression)

## Aggregate by year, state, quarter, generic name, and whether values were suppressed.

drugsAggStateGeneric <-
  yrSt[
    i  = ,
    j  = .(totalRX = sum(numberrx)),
    by = c("year", "gennme", "state", "quarter", "suppression")
  ]

setorder(drugsAggStateGeneric, year, state, quarter, gennme, suppression)

## Aggregate by year, state, quarter, generic name, brand name, and whether values were suppressed.

drugsAggStateProdnme <-
  yrSt[
    i  = ,
    j  = .(totalRX = sum(numberrx)),
    by = c("year", "gennme", "prodnme", "state", "quarter", "suppression")
  ]

setorder(drugsAggStateProdnme, year, state, quarter, gennme, prodnme, suppression)

## Get unique generic and brand names.

rxIncluded <-
  unique(yrSt[, c("gennme", "prodnme")], by = c("gennme", "prodnme"))

fwrite(drugsAggState, file = "./data/clean/01_z-drugs-2017-2020-aggregate-by-state.csv", na = "")
fwrite(drugsAggStateGeneric, file = "./data/clean/02_z-drugs-2015-2020-aggregate-by-state-and-generic.csv", na = "")
fwrite(drugsAggStateProdnme, file = "./data/clean/03_z-drugs-2015-2020-aggregate-by-state-generic-and-brand.csv", na = "")
fwrite(rxIncluded, file = "./data/clean/04_generic-brand-names-in-dataset.csv", na = "")


