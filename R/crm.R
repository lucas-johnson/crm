get_bark_percent <- function(data) {
    ifelse(is.null(data$bark_percent),
           data$species_reference |>
               dplyr::pull(BARK_VOL_PCT),
           data$bark_percent)
}

get_spec_grav_wood <- function(data) {
    ifelse(is.null(data$spec_grav_wood),
           data$species_reference |>
               dplyr::pull(WOOD_SPGR_GREENVOL_DRYWT),
           data$spec_grav_wood)

}

get_spec_grav_bark <- function(data) {
    ifelse(is.null(data$spec_grav_bark),
           data$species_reference |>
               dplyr::pull(BARK_SPGR_GREENVOL_DRYWT),
           data$spec_grav_bark)
}

get_density_reduction_factor <- function(data) {
    if (is.null(data$drf)) {
        drf <- data$drf
    } else {
        if (is.na(data$dc) | is.null(dc)) {
            drf <- 1
        } else {
            drf <- data$species_reference |>
                dplyr::pull(paste0("STANDING_DEAD_DECAY_RATIO", data$dc))
        }
    }
    return(drf)
}

get_structural_loss <- function(data, type) {
    if (is.null(data[[paste0('sl_', type)]])) {
        slr <- data[[paste0('sl_', type)]]
    } else {
        if (is.na(data$dc)) {
            slr <- 1
        }
        loss_ratios <- data.frame(
            class = c(1, 2, 3, 4, 5),
            bark = c(0.92, 0.66, 0.39, 0.21, 0.0),
            top = c(1.0, 0.5, 0.2, 0.1, 0.0),
            stump  = c(1.0, 1.0, 1.0, 1.0, 1.0),
            bole = c(1.0, 1.0, 1.0, 1.0, 1.0)
        )  |>
            tidyr::pivot_longer(-class, names_to = 'loss_type', values_to = 'ratio')
        slr <- loss_ratios |> dplyr::filter(loss_type == type & class == data$dc) |>
            dplyr::pull(ratio)
    }
    return(slr)
}

remove_cull <- function(input, percent_cull_rotten) {

    if (is.na(percent_cull_rotten)) {
        percent_cull_rotten <- 0
    } else if (percent_cull_rotten > 98) {
        percent_cull_rotten <- 100
    }
    input * (1 - (percent_cull_rotten / 100))
}

get_crm_adjustment <- function(data) {
    if (!is.null(data$crm_adj_factor)) {
        crm_adj_factor <- data$crm_adj_factor
    } else {
        crm_adj_factor <- get_bole_biomass(data) /
            get_jenkins_bole_biomass(data)
    }
    return(crm_adj_factor)
}

get_jenkins_component <- function(dbh, b0, b1, bio) {
    ratio <- exp(b0 + (b1 / measurements::conv_unit(dbh, "inch", "cm")))
    bio * ratio
}

get_jenkins_total_biomass <- function(data) {
    if (!class(data) == 'crm_data') {
        stop("Only accepts objects of class 'crm_data'. Use prep_data() to build an object of class 'crm_data'. ")
    }
    if (!is.null(data$jenkins_total_biomass)) {
        jenkins_total_biomass <- data$jenkins_total_biomass
    } else {
        total_jenks_coefs <- data$species_reference |>
            dplyr::select(JENKINS_TOTAL_B1, JENKINS_TOTAL_B2)

        b0 <- total_jenks_coefs$JENKINS_TOTAL_B1
        b1 <- total_jenks_coefs$JENKINS_TOTAL_B2
        jenkins_total_biomass <- exp(b0 + b1 * log(measurements::conv_unit(dbh, "inch", "cm"))) |>
            measurements::conv_unit("kg", "lbs")
        if (data$residual) {
            jenkins_total_biomass <- jenkins_total_biomass +
                get_jenkins_03_residual(
                    jenkins_total_biomass,
                    data$species_reference |>
                        dplyr::pull(JENKINS_SPGRPCD))
        }
    }
    return(jenkins_total_biomass)
}

get_jenkins_foliage_biomass <- function(data) {
    if (!class(data) == 'crm_data') {
        stop("Only accepts objects of class 'crm_data'. Use prep_data() to build an object of class 'crm_data'. ")
    }
    if (!is.null(data$jenkins_foliage_biomass)) {
        jenkins_foliage_biomass <- data$jenkins_foliage_biomass
    } else {
        foliage_jenks_coefs <- data$species_reference |>
            dplyr::select(JENKINS_FOLIAGE_RATIO_B1, JENKINS_FOLIAGE_RATIO_B2)

        jenkins_foliage_biomass <- get_jenkins_component(
            data$dbh,
            foliage_jenks_coefs$JENKINS_FOLIAGE_RATIO_B1,
            foliage_jenks_coefs$JENKINS_FOLIAGE_RATIO_B2,
            get_jenkins_total_biomass(data)
        )
        return(jenkins_foliage_biomass)
    }

}

get_jenkins_bole_wood_biomass <- function(data) {
    if (!is.null(data$jenkins_bole_wood_biomass)) {
        jenkins_bole_wood_biomass <- data$jenkins_bole_wood_biomass
    } else {
        bole_wood_jenks_coefs <- data$species_reference |>
            dplyr::select(JENKINS_STEM_WOOD_RATIO_B1, JENKINS_STEM_WOOD_RATIO_B2)
        jenkins_bole_wood_biomass <- get_jenkins_component(
            data$dbh,
            bole_wood_jenks_coefs$JENKINS_STEM_WOOD_RATIO_B1,
            bole_wood_jenks_coefs$JENKINS_STEM_WOOD_RATIO_B2,
            get_jenkins_total_biomass(data)
        )
    }
    return(jenkins_bole_wood_biomass)
}

get_jenkins_bole_bark_biomass <- function(data) {
    if (!is.null(data$jenkins_bole_wood_biomass)) {
        jenkins_bole_bark_biomass <- data$jenkins_bole_bark_biomass
    } else {
        bole_bark_jenks_coefs <- data$species_reference |>
            dplyr::select(JENKINS_STEM_BARK_RATIO_B1, JENKINS_STEM_BARK_RATIO_B2)
        jenkins_bole_bark_biomass <- get_jenkins_component(
            data$dbh,
            bole_bark_jenks_coefs$JENKINS_STEM_BARK_RATIO_B1,
            bole_bark_jenks_coefs$JENKINS_STEM_BARK_RATIO_B2,
            get_jenkins_total_biomass(data)
        )
    }
    return(jenkins_bole_bark_biomass)
}

get_jenkins_bole_biomass <- function(data) {
    if (!is.null(data$jenkins_bole_biomass)) {
        jenkins_bole_biomass <- data$jenkins_bole_biomass
    } else {
        bole_wood_bio <- get_jenkins_bole_wood_biomass(data)
        bark_wood_bio <- get_jenkins_bole_bark_biomass(data)
        jenkins_bole_biomass <- bole_wood_bio + bark_wood_bio
    }

    return(jenkins_bole_biomass)
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

get_raile_stump_biomass <- function(data) {
    data$spec_grav_wood <- get_spec_grav_wood(data)
    data$spec_grav_bark <- get_spec_grav_bark(data)
    stump_coefs <- data$species_reference |>
        dplyr::select(RAILE_STUMP_DOB_B1, RAILE_STUMP_DIB_B1, RAILE_STUMP_DIB_B2)

    inside_A <- stump_coefs$RAILE_STUMP_DIB_B1
    inside_B <- stump_coefs$RAILE_STUMP_DIB_B2
    outside_B <-  stump_coefs$RAILE_STUMP_DOB_B1

    stump_vol_i <- get_stump_volume(inside_A, inside_B, data$dbh)
    stump_vol_o <- get_stump_volume(1, outside_B, data$dbh)

    stump_bio_i <- stump_vol_i * data$spec_grav_wood * data$w
    stump_bio_o <- (stump_vol_o - stump_vol_i) * data$spec_grav_bark * data$w
    total_stump_bio <- (stump_bio_i + stump_bio_o)
    return(total_stump_bio)

}

get_bole_wood_biomass <- function(data) {
    data$spec_grav_wood <- get_spec_grav_wood(data)
    if (data$is_dead) {
        data$sl_bole <- get_structural_loss(data, 'bole')
        data$drf <- get_density_reduction_factor(data)
    }

    data <- sample_residuals(data)

    data$snd_vol <- get_snd_vol(data)
    bw_biomass <- data$snd_vol * data$spec_grav_wood * data$w
    if (data$is_dead) {
        bw_biomass <- bw_biomass * data$sl_bole * data$drf
    }
    return(bw_biomass)
}

get_bole_bark_biomass <- function(data) {
    data$spec_grav_bark <- get_spec_grav_bark(data)
    data$bark_percent <- get_bark_percent(data)
    if (data$is_dead) {
        data$sl_bark <- get_structural_loss(data, 'bark')
        data$drf <- get_density_reduction_factor(data)
    }

    data <- sample_residuals(data)

    data$snd_vol <- get_snd_vol(data)
    bb_biomass <- data$snd_vol * (data$bark_percent / 100.0) * data$spec_grav_bark * data$w
    if (is_dead) {
        bb_biomass <- bb_biomass * data$sl_bark * data$drf
    }
    return(bb_biomass)
}

prep_data <- function(dbh, boleht, species,
                      cull = 0, dc = NA, is_dead = FALSE,
                      drf = NULL, sl_top = NULL, sl_bole = NULL, sl_bark = NULL,
                      residual = FALSE,
                      bark_spec_grav = NULL,
                      wood_spec_grav = NULL,
                      bark_percent = NULL,
                      volume_config = NULL, volume_coefficients = NULL,
                      species_reference = NULL,
                      grs_vol = NULL, snd_vol = NULL,
                      jenkins_total_biomass = NULL,
                      crm_adj_factor = NULL,
                      jenkins_bole_biomass = NULL,
                      jenkins_foliage_biomass = NULL,
                      raile_stump_biomass = NULL,
                      bole_biomass = NULL,
                      stump_biomass = NULL,
                      top_biomass = NULL, w = 62.40) {

    if (is.null(species_reference)) {
        species_reference <- read.csv(here::here("data/REF_SPECIES.csv")) |>
            dplyr::filter(SPCD == species)
    } else {
        species_reference <- species_reference |>
            dplyr::filter(SPCD == species)
    }
    if (is.null(volume_config)) {
        volume_config <- read.csv(here::here("data/NE_config_volcfgrs.csv"))
    }
    if (is.null(volume_coefficients)) {
        volume_coefficients <- read.csv(here::here("data/NE_coefs_volcfgrs.csv"))
    }

    structure(
        list(
            dbh = dbh,
            boleht = boleht,
            species = species,
            cull = cull,
            dc = dc,
            is_dead = ifelse(is.null(is_dead),
                             (!is.null(dc) && !is.na(dc)),
                             is_dead),
            drf = drf,
            sl_top = sl_top,
            sl_bark = sl_bark,
            sl_bole = sl_bole,
            residual = residual,
            bark_spec_grav = bark_spec_grav,
            wood_spec_grav = wood_spec_grav,
            bark_percent = bark_percent,
            grs_vol = grs_vol,
            snd_vol = snd_vol,
            jenkins_total_biomass = jenkins_total_biomass,
            jenkins_bole_biomass = jenkins_bole_biomass,
            jenkins_foliage_biomass = jenkins_foliage_biomass,
            raile_stump_biomass = raile_stump_biomass,
            stump_biomass = stump_biomass,
            bole_biomass = bole_biomass,
            top_biomass = top_biomass,
            crm_adj_factor = crm_adj_factor,
            w = w,
            volume_config = volume_config,
            species_reference = species_reference,
            volume_coefficients = volume_coefficients,
            needs_residuals = residual
        ),
        class = 'crm_data'
    )
}

get_snd_vol <- function(data = NULL, dbh = NULL, species = NULL, boleht = NULL,
                        cull = 0, volume_config = NULL, volume_coefficients = NULL,
                        residual = FALSE, ...) {

    if (class(data) != 'crm_data') {
        data <- prep_data(dbh, boleht, species, residual = residual,
                          volume_coefficients = volume_coefficients,
                          volume_config = volume_config, list(...))
    }
    if (!is.null(data$snd_vol)) {
        snd_vol <- data$snd_vol
    } else {
        if (data$needs_residual) {
            data$dbh <- data$dbh + get_dia_residual()
            data$boleht <- data$boleht + get_ht_residual()
            data$cull <- data$cull + get_cull_residual()
        }
        data$grs_vol <- get_grs_vol(data)
        snd_vol <- remove_cull(data$grs_vol, data$cull)
    }
    return(snd_vol)
}

get_grs_vol <- function(data = NULL, dbh = NULL, species = NULL, boleht = NULL,
                        volume_config = NULL, volume_coefficients = NULL,
                        residual = FALSE, ...) {
    if (class(data) != 'crm_data') {
        data <- prep_data(dbh, boleht, species, residual = residual,
                          volume_coefficients = volume_coefficients,
                          volume_config = volume_config, list(...))
    }
    if (!is.null(data$grs_vol)) {
        grs_vol <- data$grs_vol
    } else {
        if (data$needs_residual) {
            dbh <- data$dbh + get_dia_residual()
            boleht <- data$boleht + get_ht_residual()
        }

        coef_sp_cd <- data$volume_config |>
            dplyr::filter(SPECIES_NUM == data$species) |>
            dplyr::pull(COEF_TBL_SP)
        vol_coefs <- data$volume_coefficients |>
            dplyr::filter(Species == coef_sp_cd)

        grs_vol <- vol_coefs$B0 + vol_coefs$B1 * (data$dbh ^ vol_coefs$B2) +
            vol_coefs$B3 * ((data$dbh ^ vol_coefs$B4) * (data$boleht ^ vol_coefs$B5))
    }
    return(grs_vol)
}


get_bole_biomass <- function(data = NULL, dbh = NULL, boleht = NULL, species = NULL,
                             cull = 0, dc = NA, is_dead = FALSE, residual = FALSE,
                             drf = NULL, wood_spec_grav = NULL,
                             bark_spec_grav = NULL, volume_config = NULL,
                             volume_coefficients = NULL, species_reference = NULL,
                             ...) {
    if (class(data) != 'crm_data') {
        data <- prep_data(dbh, boleht, species, cull = cull, dc = dc,
                          is_dead = is_dead, residual = residual, drf = drf,
                          wood_spec_grav = wood_spec_grav,
                          volume_coefficients = volume_coefficients,
                          species_reference = species_reference, list(...))
    }
    if (is.na(data$dbh) | is.null(data$dbh) | is.na(data$boleht) | is.null(data$boleht)) {
        total_bole_biomass <- 0
    } else {
        data$spec_grav_wood <- get_spec_grav_wood(data)
        data$spec_grav_bark <- get_spec_grav_bark(data)
        if (data$is_dead) {
            data$drf <- get_density_reduction_factor(data)
            data$sl_bole <- get_structural_loss(data, 'bole')
            data$sl_bark <- get_structural_loss(data, 'bark')
        }

        data <- sample_residuals(data)

        data$grs_vol <- get_grs_vol(data)
        data$snd_vol <- get_snd_vol(data)

        bole_wood_biomass <- get_bole_wood_biomass(data)
        bole_bark_biomass <- get_bole_bark_biomass(data)

        total_bole_biomass <- bole_wood_biomass + bole_bark_biomass
    }
    return(total_bole_biomass)
}

get_stump_biomass <- function(data = NULL, dbh = NULL, boleht = NULL,
                              species = NULL, cull = 0, dc = NA,
                              is_dead = FALSE, residual = FALSE,
                              drf = NULL, wood_spec_grav = NULL,
                              bark_spec_grav = NULL, volume_config = NULL,
                              volume_coefficients = NULL, species_reference = NULL,
                              ...) {

    if (class(data) != 'crm_data') {
        data <- prep_data(dbh, boleht, species, cull = cull, dc = dc,
                          is_dead = is_dead, residual = residual, drf = drf,
                          wood_spec_grav = wood_spec_grav,
                          volume_coefficients = volume_coefficients,
                          species_reference = species_reference, list(...))
    }
    if (is.na(data$dbh) | is.null(data$dbh) | is.na(data$boleht) | is.null(data$boleht)) {
        stump_biomass <- 0
    } else {
        data$spec_grav_wood <- get_spec_grav_wood(data)
        data$spec_grav_bark <- get_spec_grav_bark(data)
        if (data$is_dead) {
            data$drf <- get_density_reduction_factor(data)
            data$sl_bole <- get_structural_loss(data, 'stump')
            data$sl_bole <- get_structural_loss(data, 'bole')
            data$sl_bark <- get_structural_loss(data, 'bark')
        }

        data <- sample_residuals(data)

        data$jenkins_total_biomass <- get_jenkins_total_biomass(data)
        data$raile_stump_biomass <- get_raile_stump_biomass(data)
        data$crm_adj_factor <- get_crm_adjustment(data)
        stump_biomass <- data$raile_stump_biomass * data$crm_adj_factor
        if (data$is_dead) {
            stump_biomass <- stump_biomass * data$sl_stump
        }
    }

    return(stump_biomass)
}

get_top_biomass <- function(data = NULL, dbh = NULL, boleht = NULL,
                            species = NULL, cull = 0, dc = NA,
                            is_dead = FALSE, residual = FALSE,
                            drf = NULL, wood_spec_grav = NULL,
                            bark_spec_grav = NULL, volume_config = NULL,
                            volume_coefficients = NULL, species_reference = NULL,
                            ...) {
    if (class(data) != 'crm_data') {
        data <- prep_data(dbh, boleht, species, cull = cull, dc = dc,
                          is_dead = is_dead, residual = residual, drf = drf,
                          wood_spec_grav = wood_spec_grav,
                          volume_coefficients = volume_coefficients,
                          species_reference = species_reference, list(...))
    }
    if (is.na(data$dbh) | is.null(data$dbh) | is.na(data$boleht) | is.null(data$boleht)) {
        top_biomass <- 0
    } else {
        data$spec_grav_wood <- get_spec_grav_wood(data)
        data$spec_grav_bark <- get_spec_grav_bark(data)
        if (data$is_dead) {
            data$drf <- get_density_reduction_factor(data)
            data$sl_bole <- get_structural_loss(data, 'bole')
            data$sl_bark <- get_structural_loss(data, 'bark')
            data$sl_bark <- get_structural_loss(data, 'top')
        }

        data <- sample_residuals(data)

        data$jenkins_total_biomass <- get_jenkins_total_biomass(data)
        data$jenkins_bole_biomass <- get_jenkins_bole_biomass(data)
        data$jenkins_foliage_biomass <- get_jenkins_foliage_biomass(data)
        data$raile_stump_biomass <- get_raile_stump_biomass(data)
        data$crm_adj_factor <- get_crm_adjustment(data)
        top_biomass <- (data$jenkins_total_biomass -
                            data$jenkins_bole_biomass -
                            data$raile_stump_biomass -
                            data$jenkins_foliage_biomass) *
            data$crm_adj_factor
        if (data$is_dead) {
            top_biomass <- top_biomass * data$sl_top
        }
    }
    return(top_biomass)
}

get_ag_biomass <- function(data = NULL, dbh = NULL, boleht = NULL, species = NULL,
                           cull = 0, dc = NA, is_dead = FALSE, residual = FALSE,
                           drf = NULL, wood_spec_grav = NULL,
                           bark_spec_grav = NULL, volume_config = NULL,
                           volume_coefficients = NULL, species_reference = NULL,
                           ...) {

    if (class(data) != 'crm_data') {
        data <- prep_data(dbh, boleht, species, cull = cull, dc = dc,
                          is_dead = is_dead, residual = residual, drf = drf,
                          wood_spec_grav = wood_spec_grav,
                          volume_coefficients = volume_coefficients,
                          species_reference = species_reference, list(...))
    }

    if (is.na(data$dbh)) {
        ag_biomass <- 0
    } else if (data$dbh < 5) {
        data <- sample_residuals(data)
        sapling_adj_fac <- data$species_reference |>
            dplyr::filter(SPCD == data$species) |>
            dplyr::pull(JENKINS_SAPLING_ADJUSTMENT)
        jenkins_total <- get_jenkins_total_biomass(data)
        jenkins_foliage <- get_foliage_jenkins_biomass(data)
        sapling_bio <- jenkins_total - foliage_jenkins

        ag_biomass <- sapling_bio * sapling_adj_fac
    } else if (is.na(data$boleht)) {
        ag_biomass <- 0
    } else {
        data$spec_grav_wood <- get_spec_grav_wood(data)
        data$spec_grav_bark <- get_spec_grav_bark(data)
        if (data$is_dead) {
            data$drf <- get_density_reduction_factor(data)
            data$sl_top <- get_structural_loss(data, 'top')
            data$sl_bole <- get_structural_loss(data, 'bole')
            data$sl_bark <- get_structural_loss(data, 'bark')
        }
        data <- sample_residuals(data)

        data$jenkins_total_biomass <- get_jenkins_total_biomass(data)
        data$jenkins_bole_biomass <- get_jenkins_bole_biomass(data)
        data$bole_biomass <- get_bole_biomass(data)
        data$crm_adj_factor <- get_crm_adjustment(data)
        data$raile_stump_bio <- get_raile_stump_biomass(data)
        data$stump_biomass <- get_stump_biomass(data)
        data$top_biomass <- get_top_biomass(data)
        ag_biomass <- data$bole_biomass + data$stump_biomass + data$top_biomass
    }

    return(ag_biomass)
}

