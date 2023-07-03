#' =============================================================================
#' Author: Lucas Johnson
#'
#' Date: 2020-08-10
#' Description: compute AGB for ESF CFI data using Woodall et al. CRM method
#' Inputs: Takes in CFI inventory, and Woodall et al model coefficient
#'         spreadsheets
#' Outputs: Data tables of AGB values by CFI plot
#' =============================================================================

library(dplyr)
library(ggplot2)
source(here::here("R/crm_functions.R"))
source(here::here("R/sample_residuals.R"))
# x <- "~/Code/lib/cafri/data/allometrics/REF_SPECIES.csv"
# download.file("https://apps.fs.usda.gov/fia/datamart/CSV/REF_SPECIES.csv", x)

config <- read.csv("~/Code/lib/cafri/data/allometrics/NE_config_volcfgrs.csv")
species_refs <- read.csv("~/Code/lib/cafri/data/allometrics/REF_SPECIES.csv")
volume_coefs <- read.csv(path.expand("~/Code/lib/cafri/data/allometrics/NE_coefs_volcfgrs.csv"))
fia_data <- read.csv("~/Documents/CAFRI/data/Inventory/FIA/NY/NYS_CSV_2019/NY_TREE.csv")

set.seed(123)
fia_sample <- fia_data[sample(1:nrow(fia_data), 1000),]

fia_sample <- fia_sample |>
    rowwise() |>
    dplyr::mutate(is_dead = STATUSCD == 2) |>
    dplyr::mutate(my_agb = get_ag_biomass(DIA, BOLEHT, SPCD, config, volume_coefs,
                                          species_refs, cull = CULL,
                                          dc = DECAYCD, is_dead = is_dead,
                                          residual = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(err = my_agb - DRYBIO_AG) |>
    dplyr::mutate(abs_err = abs(err))
