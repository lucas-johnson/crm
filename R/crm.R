get_loss_ratio <- function(dc, type) {
    if (is.na(dc)) {
        return(1)
    }
    loss_ratios <- data.frame(
        class = c(1, 2, 3, 4, 5),
        bark = c(0.92, 0.66, 0.39, 0.21, 0.0),
        top = c(1.0, 0.5, 0.2, 0.1, 0.0),
        stump  = c(1.0, 1.0, 1.0, 1.0, 1.0),
        bole = c(1.0, 1.0, 1.0, 1.0, 1.0)
    )  |>
        tidyr::pivot_longer(-class, names_to = 'loss_type', values_to = 'ratio')
    loss_ratios |> dplyr::filter(loss_type == type & class == dc) |> dplyr::pull(ratio)
}

get_spec_grav_wood <- function(species, reference) {
    reference |>
        dplyr::filter(SPCD == species) |>
        dplyr::pull(WOOD_SPGR_GREENVOL_DRYWT)
}
get_spec_grav_bark <- function(species, reference) {
    reference |>
        dplyr::filter(SPCD == species) |>
        dplyr::pull(BARK_SPGR_GREENVOL_DRYWT)
}

get_density_reduction_factor <- function(dc, species, reference,
                                         residual = FALSE) {
    if (is.na(dc)) {
        drf <- 1
    } else {
        drf <- reference |> dplyr::filter(SPCD == species) |>
            dplyr::pull(paste0("STANDING_DEAD_DECAY_RATIO", dc))
        if (residual) {
            drf_residual <- get_drf_residual(drf,
                                             dc,
                                             reference |>
                                                 dplyr::filter(SPCD == species) |>
                                                 dplyr::pull(MAJOR_SPGRPCD))
            drf <- drf + drf_residual
        }
    }
    return(drf)
}

get_jenkins_component <- function(dbh, b0, b1, bio) {
    ratio <- exp(b0 + (b1 / measurements::conv_unit(dbh, "inch", "cm")))
    bio * ratio
}

remove_cull <- function(input, percent_cull_rotten) {
    
    if (is.na(percent_cull_rotten)) {
        percent_cull_rotten <- 0
    } else if (percent_cull_rotten > 98) {
        percent_cull_rotten <- 100
    }
    input * (1 - (percent_cull_rotten / 100))
}

get_bole_wood_biomass <- function(snd_vol, spec_grav_wood, species, reference,
                                  w = 62.40, dc = NULL,
                                  drf = NULL) {
    bw_biomass <- snd_vol * spec_grav_wood * w
    if (is.null(drf)) {
        if (!is.null(dc) & !is.na(dc)) {
            drf <- get_density_reduction_factor(dc, species, reference)
        } else {
            drf <- 1
        }
    }
    bw_biomass <- bw_biomass * get_loss_ratio(dc, 'bole') * drf
    
    return(bw_biomass)
}

get_bole_bark_biomass <- function(snd_vol, bark_percent, spec_grav_bark,
                                  species, reference,
                                  w = 62.40, dc = NULL, drf = NULL) {
    bb_biomass <- snd_vol * (bark_percent / 100.0) * spec_grav_bark * w
    if (is.null(drf)) {
        if (!is.null(dc) & !is.na(dc)) {
            drf <- get_density_reduction_factor(dc, species, reference)
        } else {
            drf <- 1
        }
    }
    bb_biomass <- bb_biomass * get_loss_ratio(dc, 'bark') * drf
    
    return(bb_biomass)
}

get_total_jenkins_biomass <- function(dbh, species, species_reference,
                                      residual = FALSE) {
    total_jenks_coefs <- species_reference |>
        dplyr::filter(SPCD == species) |>
        dplyr::select(JENKINS_TOTAL_B1, JENKINS_TOTAL_B2)
    
    b0 <- total_jenks_coefs$JENKINS_TOTAL_B1
    b1 <- total_jenks_coefs$JENKINS_TOTAL_B2
    total_jenks_biomass <- exp(b0 + b1 * log(measurements::conv_unit(dbh, "inch", "cm"))) |>
        measurements::conv_unit("kg", "lbs")
    if (residual) {
        total_jenks_biomass <- total_jenks_biomass + get_jenkins_03_residual(
            total_jenks_biomass,
            species_reference |>
                dplyr::filter(SPCD == species) |>
                dplyr::pull(JENKINS_SPGRPCD))
    }
    return(total_jenks_biomass)
}

get_foliage_jenkins_biomass <- function(dbh, species, species_reference,
                                        jenkins_total = NULL, residual = FALSE) {
    foliage_jenks_coefs <- species_reference |> dplyr::filter(SPCD == species) |>
        dplyr::select(JENKINS_FOLIAGE_RATIO_B1, JENKINS_FOLIAGE_RATIO_B2)
    
    foliage_biomass <- get_jenkins_component(
        dbh,
        foliage_jenks_coefs$JENKINS_FOLIAGE_RATIO_B1,
        foliage_jenks_coefs$JENKINS_FOLIAGE_RATIO_B2,
        ifelse(
            is.null(jenkins_total),
            get_total_jenkins_biomass(dbh, species, species_reference, residual = residual),
            jenkins_total
        )
    )
    return(foliage_biomass)
}


get_jenkins_bole_wood_biomass <- function(dbh, species, species_reference,
                                          jenkins_total = NULL, residual = FALSE) {
    bole_wood_jenks_coefs <- species_reference |>
        dplyr::filter(SPCD == species) |>
        dplyr::select(JENKINS_STEM_WOOD_RATIO_B1, JENKINS_STEM_WOOD_RATIO_B2)
    jenks_bole_wood_biomass <- get_jenkins_component(
        dbh,
        bole_wood_jenks_coefs$JENKINS_STEM_WOOD_RATIO_B1,
        bole_wood_jenks_coefs$JENKINS_STEM_WOOD_RATIO_B2,
        ifelse(
            is.null(jenkins_total),
            get_total_jenkins_biomass(dbh, species, species_reference, residual = residual),
            jenkins_total
        )
    )
    return(jenks_bole_wood_biomass)
}

get_jenkins_bole_bark_biomass <- function(dbh, species, species_reference,
                                          jenkins_total = NULL, residual = FALSE) {
    bole_bark_jenks_coefs <- species_reference |>
        dplyr::filter(SPCD == species) |>
        dplyr::select(JENKINS_STEM_BARK_RATIO_B1, JENKINS_STEM_BARK_RATIO_B2)
    jenks_bole_bark_biomass <- get_jenkins_component(
        dbh,
        bole_bark_jenks_coefs$JENKINS_STEM_BARK_RATIO_B1,
        bole_bark_jenks_coefs$JENKINS_STEM_BARK_RATIO_B2,
        ifelse(
            is.null(jenkins_total),
            get_total_jenkins_biomass(dbh, species, species_reference, residual = residual),
            jenkins_total
        )
    )
    return(jenks_bole_bark_biomass)
}

get_jenkins_bole_biomass <- function(dbh, species, species_reference,
                                     jenkins_total = NULL) {
    bole_wood_bio <- get_jenkins_bole_wood_biomass(dbh, species, species_reference,
                                                   jenkins_total = jenkins_total)
    bark_wood_bio <- get_jenkins_bole_bark_biomass(dbh, species, species_reference,
                                                   jenkins_total = jenkins_total)
    total_bio <- bole_wood_bio + bark_wood_bio
    return(total_bio)
}

get_grs_vol <- function(dbh, species, bole_ht, config, volume_coefficients) {
    coef_sp_cd <- config |>
        dplyr::filter(SPECIES_NUM == species) |>
        dplyr::pull(COEF_TBL_SP)
    vol_coefs <- volume_coefficients |> dplyr::filter(Species == coef_sp_cd)
    
    grs_vol <- vol_coefs$B0 + vol_coefs$B1 * (dbh ^ vol_coefs$B2) + vol_coefs$B3 *
        ((dbh ^ vol_coefs$B4) * (bole_ht ^ vol_coefs$B5))
    
    return(grs_vol)
}

get_bole_biomass <- function(dbh, bole_ht, species, config, volume_coefficients,
                             species_reference, cull = 0, dc = NA,
                             spec_grav_wood = NULL,
                             spec_grav_bark = NULL,
                             drf = NULL) {
    grs_vol <- get_grs_vol(dbh, species, bole_ht, config, volume_coefficients)
    snd_vol <- remove_cull(grs_vol, cull)
    
    if (is.null(spec_grav_wood)) {
        spec_grav_wood <- get_spec_grav_wood(species, species_reference)
    }
    if (is.null(spec_grav_bark)) {
        spec_grav_bark <- get_spec_grav_bark(species, species_reference)
    }
    
    bark_percent <- species_reference |>
        dplyr::filter(SPCD == species) |>
        dplyr::pull(BARK_VOL_PCT)
    
    
    bole_wood_biomass <- get_bole_wood_biomass(snd_vol, spec_grav_wood, species,
                                               species_reference, dc = dc,
                                               drf = drf)
    bole_bark_biomass <- get_bole_bark_biomass(snd_vol, bark_percent,
                                               spec_grav_bark, species,
                                               species_reference, dc = dc,
                                               drf = drf)
    
    total_bole_biomass <- bole_wood_biomass + bole_bark_biomass
    return(total_bole_biomass)
}

get_crm_adjustment <- function(bole_biomass, jenkins_bole_biomass) {
    adj_fac <- bole_biomass / jenkins_bole_biomass
    return(adj_fac)
}

get_stump_volume <- function(A, B, dbh) {
    
    stump_vol <- ((pi * (dbh ^ 2)) / (4.0 * 144.0)) *
        (
            (
                ((A - B) ^ 2) + 11.0 * B * (A - B) * log(2.0) - (30.25 / 2.0) * (B ^ 2.0)
            ) -
                (
                    11.0 * B * (A - B) * log(1.0) - 30.25 * (B ^ 2)
                )
        )
    
    return(stump_vol)
}

get_raile_stump_biomass <- function(dbh, species, species_reference,
                                    w = 62.40, spec_grav_wood = NULL,
                                    spec_grav_bark = NULL) {
    if (is.null(spec_grav_wood)) {
        spec_grav_wood <- species_reference |>
            dplyr::filter(SPCD == species) |>
            dplyr::pull(WOOD_SPGR_GREENVOL_DRYWT)
    }
    if (is.null(spec_grav_bark)) {
        spec_grav_bark <- species_reference  |>
            dplyr::filter(SPCD == species) |>
            dplyr::pull(BARK_SPGR_GREENVOL_DRYWT)
    }
    stump_coefs <- species_reference |>
        dplyr::filter(SPCD == species) |>
        dplyr::select(RAILE_STUMP_DOB_B1, RAILE_STUMP_DIB_B1, RAILE_STUMP_DIB_B2)
    
    inside_A <- stump_coefs$RAILE_STUMP_DIB_B1
    inside_B <- stump_coefs$RAILE_STUMP_DIB_B2
    outside_B <-  stump_coefs$RAILE_STUMP_DOB_B1
    
    stump_vol_i <- get_stump_volume(inside_A, inside_B, dbh)
    stump_vol_o <- get_stump_volume(1, outside_B, dbh)
    
    stump_bio_i <- stump_vol_i * spec_grav_wood * w
    stump_bio_o <- (stump_vol_o - stump_vol_i) * spec_grav_bark * w
    total_stump_bio <- (stump_bio_i + stump_bio_o)
    return(total_stump_bio)
    
}

get_crm_stump_biomass <- function(dbh, bole_ht, species, config, volume_coefficients,
                                  species_reference, cull = 0, dc = NA,
                                  crm_adj_fac = NULL, bole_biomass = NULL,
                                  jenkins_bole_biomass = NULL,
                                  raile_stump_biomass = NULL,
                                  spec_grav_wood = NULL,
                                  spec_grav_bark = NULL) {
    if (is.null(raile_stump_biomass)) {
        raile_stump_biomass <- get_raile_stump_biomass(dbh, species, species_reference,
                                                       spec_grav_wood = spec_grav_wood,
                                                       spec_grav_bark = spec_grav_bark)
    }
    
    if (is.null(crm_adj_fac)) {
        crm_adj_fac <- get_crm_adjustment(
            ifelse(
                is.null(bole_biomass),
                get_bole_biomass(dbh,
                                 bole_ht,
                                 species,
                                 config,
                                 volume_coefficients,
                                 species_reference,
                                 cull = cull,
                                 spec_grav_wood = spec_grav_wood,
                                 spec_grav_bark = spec_grav_bark,
                                 dc = NA),
                bole_biomass),
            ifelse(
                is.null(jenkins_bole_biomass),
                get_jenkins_bole_biomass(dbh, species, species_reference),
                jenkins_bole_biomass)
        )
    }
    
    stump_bio <- raile_stump_biomass * crm_adj_fac
    if (!is.null(dc) & !is.na(dc)) {
        stump_bio <- stump_bio * get_loss_ratio(dc, 'stump')
    }
    return(stump_bio)
}

get_tab_biomass <- function(dbh, bole_ht, species, config, volume_coefficients,
                            species_reference, cull = 0,
                            dc = dc, jenkins_total = NULL, crm_adj_fac = NULL,
                            bole_biomass = NULL, jenkins_bole_biomass = NULL,
                            raile_stump_biomass = NULL,
                            spec_grav_wood = NULL, spec_grav_bark = NULL,
                            drf = NULL, residual = FALSE) {
    if (is.null(jenkins_total)) {
        jenkins_total <- get_total_jenkins_biomass(dbh, species, species_reference,
                                                   residual = residual)
    }
    if (is.null(jenkins_bole_biomass)) {
        jenkins_bole_biomass <- get_jenkins_bole_biomass(dbh, species, species_reference,
                                                         jenkins_total = jenkins_total)
    }
    if (is.null(crm_adj_fac)) {
        if (is.null(bole_biomass)) {
            bole_biomass <- get_bole_biomass(dbh, bole_ht, species, config,
                                             volume_coefficients,
                                             species_reference, cull = cull, dc = dc,
                                             spec_grav_wood = spec_grav_wood,
                                             spec_grav_bark = spec_grav_bark,
                                             drf = drf)
        }
        crm_adj_fac <- get_crm_adjustment(bole_biomass, jenkins_bole_biomass)
    }
    
    if (is.null(raile_stump_biomass)) {
        raile_stump_biomass <- get_raile_stump_biomass(dbh, bole_ht, species,
                                                       species_reference,
                                                       spec_grav_wood = spec_grav_wood,
                                                       spec_grav_bark = spec_grav_bark)
    }
    
    foliage_biomass <- get_foliage_jenkins_biomass(dbh, species, species_reference,
                                                   jenkins_total = jenkins_total)
    tab_biomass <- (jenkins_total -
                        jenkins_bole_biomass -
                        raile_stump_biomass -
                        foliage_biomass) * crm_adj_fac
    if (!is.null(dc) & !is.na(dc)) {
        tab_biomass <- tab_biomass * get_loss_ratio(dc, 'top')
    }
    return(tab_biomass)
}

get_ag_biomass <- function(dbh, bole_ht, species, config, volume_coefficients,
                           species_reference, cull = 0, dc = NA, is_dead = FALSE,
                           residual = FALSE, drf = NULL) {
    
    if (is.na(dbh)) {
        ag_biomass <- 0
    } else {
        
        spec_grav_wood <- get_spec_grav_wood(species, species_reference)
        spec_grav_bark <- get_spec_grav_bark(species, species_reference)
        
        if (residual) {
            # 1 is smallest DBH on subplots
            dbh <- max(c(dbh + get_dia_residual(), 1))
            # 4 is shortest tree in DB
            bole_ht <- max(c(bole_ht + get_ht_residual(), 4))
            cull <- max(c(cull + get_cull_residual(), 0))
            cull <- min(c(cull, 100))
            spec_grav_wood <- spec_grav_wood + get_specific_grav_residual(spec_grav_wood)
            spec_grav_bark <- spec_grav_bark + get_specific_grav_residual(spec_grav_bark)
            if (is_dead) {
                dc <- dc + get_decaycd_residual()
                dc <- ifelse(dc < 1, 1, ifelse(dc > 5, 5, dc))
                drf <- get_density_reduction_factor(dc,
                                                    species,
                                                    species_reference,
                                                    residual = TRUE)
            }
        }
        
        jenkins_total <- get_total_jenkins_biomass(dbh, species, species_reference,
                                                   residual = residual)
        
        if (dbh < 5) {
            sapling_adj_fac <- species_reference |>
                dplyr::filter(SPCD == species) |>
                dplyr::pull(JENKINS_SAPLING_ADJUSTMENT)
            
            
            foliage_jenkins <- get_foliage_jenkins_biomass(dbh, species,
                                                           species_reference,
                                                           jenkins_total = jenkins_total)
            sapling_bio <- jenkins_total - foliage_jenkins
            ag_biomass <- sapling_bio * sapling_adj_fac
            
        } else if (is.na(bole_ht)) {
            ag_biomass <- 0
            
        } else {
            bole_bio <- get_bole_biomass(dbh,
                                         bole_ht,
                                         species,
                                         config,
                                         volume_coefficients,
                                         species_reference,
                                         cull = cull,
                                         dc = dc,
                                         spec_grav_wood = spec_grav_wood,
                                         spec_grav_bark = spec_grav_bark,
                                         drf = drf)
            
            jenkins_bole_bio <- get_jenkins_bole_biomass(dbh, species, species_reference,
                                                         jenkins_total = jenkins_total)
            crm_adj_fac <- get_crm_adjustment(bole_biomass = bole_bio,
                                              jenkins_bole_biomass = jenkins_bole_bio)
            
            raile_stump_bio <- get_raile_stump_biomass(dbh,
                                                       species,
                                                       species_reference,
                                                       spec_grav_wood = spec_grav_wood,
                                                       spec_grav_bark = spec_grav_bark)
            stump_bio <- get_crm_stump_biomass(dbh,
                                               bole_ht,
                                               species,
                                               config,
                                               volume_coefficients,
                                               species_reference,
                                               cull = cull,
                                               dc = dc,
                                               raile_stump_biomass = raile_stump_bio)
            tab_bio <- get_tab_biomass(dbh,
                                       bole_ht,
                                       species,
                                       config,
                                       volume_coefficients,
                                       species_reference,
                                       cull = cull,
                                       dc = dc,
                                       jenkins_total = jenkins_total,
                                       bole_biomass = bole_bio,
                                       jenkins_bole_biomass = jenkins_bole_bio,
                                       crm_adj_fac = crm_adj_fac,
                                       raile_stump_biomass = raile_stump_bio)
            ag_biomass <- bole_bio + stump_bio + tab_bio
        }
    }
    
    return(ag_biomass)
}
