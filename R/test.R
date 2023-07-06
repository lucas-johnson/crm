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
source(here::here("R/sample_residuals.R"))
# x <- "~/Code/lib/cafri/data/allometrics/REF_SPECIES.csv"
# download.file("https://apps.fs.usda.gov/fia/datamart/CSV/REF_SPECIES.csv", x)

fia_data <- read.csv("~/Documents/CAFRI/data/Inventory/FIA/NY/NYS_CSV_2019/NY_TREE.csv")


# LIVE example
set.seed(123)
live_sample <- fia_data |>
    dplyr::filter(STATUSCD == 1) |>
    dplyr::sample_n(1) |>
    dplyr::mutate(my_agb = get_ag_biomass(dbh = DIA, boleht = BOLEHT, species = SPCD, cull = CULL,
                                          is_dead = STATUSCD == 2, dc = DECAYCD,
                                          residual = FALSE)) |>
    dplyr::select(my_agb, DRYBIO_AG)

# DEAD example
dead_sample <- fia_data |>
    dplyr::filter(STATUSCD == 2) |>
    dplyr::sample_n(1) |>
    dplyr::mutate(my_agb = get_ag_biomass(dbh = DIA, boleht = BOLEHT, species = SPCD,
                                          cull = CULL,
                                          dc = DECAYCD,
                                          residual = FALSE)) |>
    dplyr::mutate(err = my_agb - DRYBIO_AG) |>
    dplyr::mutate(abs_err = abs(err))
