#'  Error message for crm_data type incompatibility
#'
#'  Produce a standardized error message for when a function only accepts an
#'  object of type 'crm_data'.
#'
#' @return Character string error message
#'
get_crm_data_type_error <- function() {

    return(paste0(
        "Only accepts objects of class 'crm_data'. ",
        "Use prep_data() to build an object of class 'crm_data'."))
}

#' Handle data masked arguments (column names)
#' @inheritParams prep_data
#'
#' @return a list of expressions to be used as column names
#'
ensym_cols <- function(grs_vol = NULL,
                       snd_vol = NULL,
                       jenkins_total_biomass = NULL,
                       crm_adj_factor = NULL,
                       jenkins_bole_biomass = NULL,
                       jenkins_foliage_biomass = NULL,
                       raile_stump_biomass = NULL,
                       bole_biomass = NULL,
                       stump_biomass = NULL,
                       top_biomass = NULL,
                       bark_spec_grav = NULL,
                       wood_spec_grav = NULL,
                       bark_percent = NULL,
                       drf = NULL,
                       sl_top = NULL,
                       sl_bole = NULL,
                       sl_bark = NULL,
                       sl_stump = NULL) {
    args <- list()
    if (!missing(grs_vol)) {
        args <- append(args, rlang::ensyms(grs_vol = grs_vol))
    }
    if (!missing(snd_vol)) {
        args <- append(args, rlang::ensyms(snd_vol = snd_vol))
    }
    if (!missing(jenkins_total_biomass)) {
        args <- append(args, rlang::ensyms(jenkins_total_biomass = jenkins_total_biomass))
    }
    if (!missing(crm_adj_factor)) {
        args <- append(args, rlang::ensyms(crm_adj_factor = crm_adj_factor))
    }
    if (!missing(jenkins_bole_biomass)) {
        args <- append(args, rlang::ensyms(jenkins_bole_biomass = jenkins_bole_biomass))
    }
    if (!missing(jenkins_foliage_biomass)) {
        args <- append(args, rlang::ensyms(jenkins_foliage_biomass = jenkins_foliage_biomass))
    }
    if (!missing(raile_stump_biomass)) {
        args <- append(args, rlang::ensyms(raile_stump_biomass = raile_stump_biomass))
    }
    if (!missing(bole_biomass)) {
        args <- append(args, rlang::ensyms(bole_biomass = bole_biomass))
    }
    if (!missing(stump_biomass)) {
        args <- append(args, rlang::ensyms(stump_biomass = stump_biomass))
    }
    if (!missing(top_biomass)) {
        args <- append(args, rlang::ensyms(top_biomass = top_biomass))
    }
    if (!missing(bark_spec_grav)) {
        args <- append(args, rlang::ensyms(bark_spec_grav = bark_spec_grav))
    }
    if (!missing(wood_spec_grav)) {
        args <- append(args, rlang::ensyms(wood_spec_grav = wood_spec_grav))
    }
    if (!missing(drf)) {
        args <- append(args, rlang::ensyms(drf = drf))
    }
    if (!missing(sl_top)) {
        args <- append(args, rlang::ensyms(sl_top = sl_top))
    }
    if (!missing(sl_bole)) {
        args <- append(args, rlang::ensyms(sl_bole = sl_bole))
    }
    if (!missing(sl_bark)) {
        args <- append(args, rlang::ensyms(sl_bark = sl_bark))
    }
    if (!missing(sl_stump)) {
        args <- append(args, rlang::ensyms(sl_stump = sl_stump))
    }
    return(args)
}

#' Look up bark percent (of volume) for a given species
#'
#' @inheritParams get_ag_biomass
#'
#' @return Float specific gravity
#'
get_bark_percent <- function(data) {
    if (class(data) != 'crm_data') {
        stop(get_crm_data_type_error())
    }
    ifelse(is.null(data$bark_percent),
           data$species_reference |>
               dplyr::pull(BARK_VOL_PCT),
           data$bark_percent)
}

#' Look up wood specific gravity for a given species
#'
#' @inheritParams get_ag_biomass
#'
#' @return Float specific gravity
#'
get_wood_spec_grav <- function(data) {
    if (class(data) != 'crm_data') {
        stop(get_crm_data_type_error())
    }
    ifelse(is.null(data$wood_spec_grav),
           data$species_reference |>
               dplyr::pull(WOOD_SPGR_GREENVOL_DRYWT),
           data$wood_spec_grav)

}

#' Look up bark specific gravity for a given species
#'
#' @inheritParams get_ag_biomass
#'
#' @return Float specific gravity
#'
get_bark_spec_grav <- function(data) {

    if (class(data) != 'crm_data') {
        stop("Only accepts objects of class 'crm_data'. Use prep_data() to build an object of class 'crm_data'. ")
    }
    ifelse(is.null(data$bark_spec_grav),
           data$species_reference |>
               dplyr::pull(BARK_SPGR_GREENVOL_DRYWT),
           data$bark_spec_grav)
}

#' Look up density reduction factor for a given decay code
#'
#' @inheritParams get_ag_biomass
#'
#' @return Float density reduction factor
#'
get_density_reduction_factor <- function(data) {
    if (class(data) != 'crm_data') {
        stop(get_crm_data_type_error())
    }
    if (!is.null(data$drf)) {
        drf <- data$drf
    } else {
        if (is.null(data$dc) || is.na(data$dc)) {
            drf <- 1
        } else {
            drf <- data$species_reference |>
                dplyr::pull(paste0("STANDING_DEAD_DECAY_RATIO", data$dc))
        }
    }
    return(drf)
}

#' Look up structural loss ratio for a given decay code and type
#'
#' @inheritParams get_ag_biomass
#' @param type one of c('bole', 'bark', 'stump', 'top')
#'
#' @return Float structural loss ratio
#'
get_structural_loss <- function(data, type) {

    if (class(data) != 'crm_data') {
        stop(get_crm_data_type_error())
    }
    if (!is.null(data[[paste0('sl_', type)]])) {
        slr <- data[[paste0('sl_', type)]]
    } else {
        if (is.null(data$dc) || is.na(data$dc)) {
            slr <- 1
        } else {
            if (!type %in% c('bole', 'bark', 'stump', 'top')) {
                stop("type must be one of: 'bole', 'bark', 'stump', 'top'")
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
    }
    return(slr)
}

#' Deduct rotten/missing cull from input
#'
#' @param input floating point volume or biomass.
#' @param percent_cull_rotten floating point percentage value representing
#' percent of the tree that is rotten or missing.
#'
#' @return Floating point cull-adjusted value
#'
remove_cull <- function(input, percent_cull_rotten) {

    if (is.null(percent_cull_rotten) || is.na(percent_cull_rotten)) {
        percent_cull_rotten <- 0
    } else if (percent_cull_rotten > 98) {
        percent_cull_rotten <- 100
    }
    input * (1 - (percent_cull_rotten / 100))
}

#' Compute CRM adjustment factor
#'
#' Get component ratio adjustment factor to convert jenkins component estimates
#' to CRM component estimates.
#' @inheritParams get_ag_biomass
#' @return Float adjustment factor
#'
get_crm_adjustment <- function(data) {

    if (class(data) != 'crm_data') {
        stop(get_crm_data_type_error())
    }
    if (!is.null(data$crm_adj_factor)) {
        crm_adj_factor <- data$crm_adj_factor
    } else {
        crm_adj_factor <- get_bole_biomass(data) /
            get_jenkins_bole_biomass(data)
    }
    return(crm_adj_factor)
}


#' Compute jenkins 2003 component using jenkins 2003 ratio equation
#'
#' @param dbh diameter at breast height
#' @param b0 model parameter 1
#' @param b1 model parameter 2
#' @param bio Biomass (total tree biomass) of which the component is a fraction
#' of
#'
#' @return biomass in lbs
#'
get_jenkins_component <- function(dbh, b0, b1, bio) {
    ratio <- exp(b0 + (b1 / measurements::conv_unit(dbh, "inch", "cm")))
    bio * ratio
}

#' Compute Jenkins et al. 2003 total tree (including foliage)
#'
#' @inheritParams get_ag_biomass
#'
#' @return A float indicating the total tree biomass in lbs.
#'
get_jenkins_total_biomass <- function(data) {
    if (class(data) != 'crm_data') {
        stop(get_crm_data_type_error())
    }
    if (!is.null(data$jenkins_total_biomass)) {
        jenkins_total_biomass <- data$jenkins_total_biomass
    } else {
        total_jenks_coefs <- data$species_reference |>
            dplyr::select(JENKINS_TOTAL_B1, JENKINS_TOTAL_B2)

        b0 <- total_jenks_coefs$JENKINS_TOTAL_B1
        b1 <- total_jenks_coefs$JENKINS_TOTAL_B2
        jenkins_total_biomass <- exp(b0 + b1 * log(measurements::conv_unit(data$dbh, "inch", "cm"))) |>
            measurements::conv_unit("kg", "lbs")
    }
    return(jenkins_total_biomass)
}

#' Compute Jenkins et al. 2003 foliage biomass
#'
#' @inheritParams get_ag_biomass
#'
#' @return A float indicating the foliage biomass in lbs.
#'
get_jenkins_foliage_biomass <- function(data) {
    if (class(data) != 'crm_data') {
        stop(get_crm_data_type_error())
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

#' Compute Jenkins et al. 2003 bole wood biomass
#'
#' @inheritParams get_ag_biomass
#'
#' @return A float indicating the bole wood biomass in lbs.
#'
get_jenkins_bole_wood_biomass <- function(data) {
    if (class(data) != 'crm_data') {
        stop(get_crm_data_type_error())
    }
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

#' Compute Jenkins et al. 2003 bole bark biomass
#'
#' @inheritParams get_ag_biomass
#'
#' @return A float indicating the bole bark biomass in lbs.
#'
get_jenkins_bole_bark_biomass <- function(data) {
    if (class(data) != 'crm_data') {
        stop(get_crm_data_type_error())
    }
    if (!is.null(data$jenkins_bole_bark_biomass)) {
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


#' Compute Jenkins et al. 2003 bole biomass (wood + bark)
#'
#' @inheritParams get_ag_biomass
#'
#' @return A float indicating the bole biomass in lbs.
#'
get_jenkins_bole_biomass <- function(data) {

    if (class(data) != 'crm_data') {
        stop(get_crm_data_type_error())
    }
    if (!is.null(data$jenkins_bole_biomass)) {
        jenkins_bole_biomass <- data$jenkins_bole_biomass
    } else {
        bole_wood_bio <- get_jenkins_bole_wood_biomass(data)
        bark_wood_bio <- get_jenkins_bole_bark_biomass(data)
        jenkins_bole_biomass <- bole_wood_bio + bark_wood_bio
    }

    return(jenkins_bole_biomass)
}

#' Raile 1982 Stump volume
#'
#' @param A first coefficient
#' @param B second coefficient
#' @param dbh diameter at breast height
#'
#' @return float value indicating stump bolume (0-1ft tall) in cubic feet.
#'
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

#' Compute Raile 1982 stump biomass
#'
#' Computes stump biomass (0 to 1ft tall) for individual tree's using the
#' Raile 1982 models.
#'
#' @inheritParams get_ag_biomass
#'
#' @return Float indicating oven-dry stump biomass in lbs.
#'
get_raile_stump_biomass <- function(data) {
    if (class(data) != 'crm_data') {
        stop(get_crm_data_type_error())
    }
    data$wood_spec_grav <- get_wood_spec_grav(data)
    data$bark_spec_grav <- get_bark_spec_grav(data)
    stump_coefs <- data$species_reference |>
        dplyr::select(RAILE_STUMP_DOB_B1, RAILE_STUMP_DIB_B1, RAILE_STUMP_DIB_B2)

    inside_A <- stump_coefs$RAILE_STUMP_DIB_B1
    inside_B <- stump_coefs$RAILE_STUMP_DIB_B2
    outside_B <-  stump_coefs$RAILE_STUMP_DOB_B1

    stump_vol_i <- get_stump_volume(inside_A, inside_B, data$dbh)
    stump_vol_o <- get_stump_volume(1, outside_B, data$dbh)

    stump_bio_i <- stump_vol_i * data$wood_spec_grav * 62.4
    stump_bio_o <- (stump_vol_o - stump_vol_i) * data$bark_spec_grav * 62.4
    total_stump_bio <- (stump_bio_i + stump_bio_o)
    return(total_stump_bio)

}

#' Compute bole wood biomass
#'
#' Computes tree-level bole wood biomass for individual tree's following the
#' Woodall et al. 2011 Component Ratio Method (CRM), and using Domke et al. 2011
#' density reduction and structural loss ratios for standing dead trees.
#'
#' @inheritParams get_ag_biomass
#'
#' @return Float indicating oven-dry bole wood biomass in lbs.
#'
get_bole_wood_biomass <- function(data) {
    if (class(data) != 'crm_data') {
        stop(get_crm_data_type_error())
    }
    data$wood_spec_grav <- get_wood_spec_grav(data)
    if (data$is_dead) {
        data$sl_bole <- get_structural_loss(data, 'bole')
        data$drf <- get_density_reduction_factor(data)
    }

    data$snd_vol <- get_snd_vol(data)
    bw_biomass <- data$snd_vol * data$wood_spec_grav * 62.4
    if (data$is_dead) {
        bw_biomass <- bw_biomass * data$sl_bole * data$drf
    }
    return(bw_biomass)
}

#' Compute bole bark biomass
#'
#' Computes bole bark biomass for individual tree's following the
#' Woodall et al. 2011 Component Ratio Method (CRM), and using Domke et al. 2011
#' density reduction and structural loss ratios for standing dead trees.
#'
#' @inheritParams get_ag_biomass
#'
#' @return Float indicating oven-dry bole bark biomass in lbs.
#'
get_bole_bark_biomass <- function(data) {
    if (class(data) != 'crm_data') {
        stop(get_crm_data_type_error())
    }
    data$bark_spec_grav <- get_bark_spec_grav(data)
    data$bark_percent <- get_bark_percent(data)
    if (data$is_dead) {
        data$sl_bark <- get_structural_loss(data, 'bark')
        data$drf <- get_density_reduction_factor(data)
    }

    data$snd_vol <- get_snd_vol(data)
    bb_biomass <- data$snd_vol * (data$bark_percent / 100.0) * data$bark_spec_grav * 62.4
    if (data$is_dead) {
        bb_biomass <- bb_biomass * data$sl_bark * data$drf
    }
    return(bb_biomass)
}


#' Convert arguments into a crm_data object for computing volumes and individual
#' biomass components.
#'
#' @param dbh Float - diameter at breast height
#' @param boleht Float - height of tree bole
#' @param species Integer - FIA species code
#' @param cull Integer - percent indicating percent of tree that is rotten or
#' missing
#' @param dc Integer (1-5) - FIA decay class code  (DECAYCD)
#' @param bark_spec_grav Float indicating tree bark specific gravity for
#' converting volume to biomass
#' @param wood_spec_grav Float indicating tree wood specific gravity for
#' converting volume to biomass
#' @param volume_config file path to FIA's NE_config_volcfgrs.csv
#' @param volume_coefficients file path to FIA's NE_coefs_volcfgrs.csv
#' @param species_reference file path to FIA REF_SPECIES.csv
#' @param bark_percent float indicating the percent of the tree bole volume
#' that is bark
#' @param drf  (density reduction factor) Float ratio to convert live biomass to
#' standing dead biomass
#' @param sl_top (structural loss factor - top) Float ratio to adjust standing
#' dead biomass to account for missing/damaged top
#' @param sl_bole (structural loss factor - bole) Float ratio to adjust standing
#' dead biomass to account for missing/damaged bole
#' @param sl_bark (structural loss factor - bark) Float ratio to adjust standing
#' dead biomass to account for missing/damaged bark
#' @param sl_stump (structural loss factor - stump) Float ratio to adjust
#' standing dead biomass to account for missing/damaged stump
#' @param grs_vol Float indicating gross volume of the tree (FIA VOLCFGRS)
#' @param snd_vol Float indicating sound volume of the tree (FIA VOLCFNET)
#' @param jenkins_total_biomass Float indicating jenkins et al. 2003 estimate
#' of total tree biomass.
#' @param crm_adj_factor (component ratio adjustment factor) Float indicating a
#' ratio to adjust jenkins
#' @param jenkins_bole_biomass Float indicating jenkins et al. 2003 estimate of
#' bole biomass
#' @param jenkins_foliage_biomass Float indicating jenkins et al. 2003 estimate
#' of foliage biomass
#' @param raile_stump_biomass Float indicating Raile 1982 estimate of
#' stump biomass
#' @param bole_biomass Float indicating CRM estimate of bole biomass
#' (FIA's DRYBIO_BOLE)
#' @param stump_biomass Float indicating CRM estimate of stump biomass
#' (FIA's DRYBIO_STUMP)
#' @param top_biomass Float indicating CRM estimate of top and branch biomass
#' (FIA'S DRYBIO_TOP)
#'
#' @return an object of type `crm_data`
#' @export
prep_data <- function(dbh, boleht, species,
                      cull = NULL,
                      dc = NULL,
                      bark_spec_grav = NULL,
                      wood_spec_grav = NULL,
                      volume_config = NULL,
                      volume_coefficients = NULL,
                      species_reference = NULL,
                      bark_percent = NULL,
                      drf = NULL,
                      sl_top = NULL,
                      sl_bole = NULL,
                      sl_bark = NULL,
                      sl_stump = NULL,
                      grs_vol = NULL,
                      snd_vol = NULL,
                      jenkins_total_biomass = NULL,
                      crm_adj_factor = NULL,
                      jenkins_bole_biomass = NULL,
                      jenkins_foliage_biomass = NULL,
                      raile_stump_biomass = NULL,
                      bole_biomass = NULL,
                      stump_biomass = NULL,
                      top_biomass = NULL) {

    if (is.null(dbh) | is.null(boleht) | is.null(species)) {
        stop("Must provide dbh, boleht, and species arguments.")
    }

    if (is.null(species_reference)) {
        species_reference <- species_reference_default |>
            dplyr::filter(SPCD == species)
    } else {
        species_reference <- species_reference |>
            dplyr::filter(SPCD == species)
    }
    if (is.null(volume_config)) {
        volume_config <- volume_config_northeast
    }
    if (is.null(volume_coefficients)) {
        volume_coefficients <- volume_coefficients_northeast
    }
    is_dead <- (!is.null(dc) && !is.na(dc))
    if (is_dead && !dc %in% 1:5) {
        stop("argument dc must be between 1 and 5.")
    }
    structure(
        list(
            dbh = dbh,
            boleht = boleht,
            species = species,
            cull = cull,
            dc = dc,
            is_dead = is_dead,
            drf = drf,
            sl_top = sl_top,
            sl_bark = sl_bark,
            sl_bole = sl_bole,
            sl_stump = sl_stump,
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
            volume_config = volume_config,
            species_reference = species_reference,
            volume_coefficients = volume_coefficients
        ),
        class = 'crm_data'
    )
}

#' Compute sound volume for individual trees
#'
#' Computes sound volume (VOLCFSND) for individual tree's using
#' the Scott 1981 models and a cull reduction.
#'
#' If `data` is null then dbh, boleht, and species are required and an object of
#' type 'crm_data' will be built for you. If `data` inherits from data.frame
#' then additional arguments are expected to be names of columns in the
#' data.frame and the computations will be carried out rowwise. In this case
#' a vector of values will be returned in place of a single float.
#'
#' @param data An object of type 'crm_data' (use `prep_data` to build) or a
#' data.frame.
#' @inheritParams get_ag_biomass
#'
#' @return Float value indicating cubic foot sound volume of the tree
#' @export
#'
get_snd_vol <- function(data = NULL, dbh = NULL, species = NULL, boleht = NULL,
                        cull = NULL, volume_config = NULL, volume_coefficients = NULL,
                        ...) {

    if (inherits(data, 'data.frame')) {
        if (any(c(missing(dbh),
                  missing(boleht),
                  missing(species),
                  missing(cull)))) {
            stop(paste0("When 'data' argument is a data.frame, 'dbh', ",
                        "'boleht', 'species', 'cull', 'dc' must all be ",
                        "supplied as column names."))
        }
        args <- c(rlang::ensyms(dbh = dbh, boleht = boleht, species = species,
                                cull = cull),
                  ensym_cols(...))
        snd_vol <- data |>
            dplyr::rowwise() |>
            dplyr::mutate(snd_vol = get_snd_vol(!!!args,
                                                volume_config = volume_config,
                                                volume_coefficients = volume_coefficients)) |>
            dplyr::ungroup() |>
            dplyr::pull(snd_vol)
        return(snd_vol)
    } else if (class(data) != 'crm_data') {
        data <- prep_data(dbh, boleht, species, cull = cull,
                          volume_coefficients = volume_coefficients,
                          volume_config = volume_config, list(...))
    }
    if (!is.null(data$snd_vol)) {
        snd_vol <- data$snd_vol
    } else {
        data$grs_vol <- get_grs_vol(data)
        snd_vol <- remove_cull(data$grs_vol, data$cull)
    }
    return(snd_vol)
}

#' Compute gross volume for individual trees
#'
#' Computes gross volume (VOLCFGRS) for individual tree's using
#' the Scott 1981 models.
#'
#' If `data` is null then dbh, boleht, and species are required and an object of
#' type 'crm_data' will be built for you. If `data` inherits from data.frame
#' then additional arguments are expected to be names of columns in the
#' data.frame and the computations will be carried out rowwise. In this case
#' a vector of values will be returned in place of a single float.
#'
#' @inheritParams get_ag_biomass
#'
#' @return Float value indicating cubic foot gross volume of the tree.
#' @export
#'
get_grs_vol <- function(data = NULL, dbh = NULL, species = NULL, boleht = NULL,
                        volume_config = NULL, volume_coefficients = NULL,
                        ...) {
    if (inherits(data, 'data.frame')) {
        if (any(c(missing(dbh),
                  missing(ht),
                  missing(species)))) {
            stop(paste0("When 'data' argument is a data.frame, 'dbh', ",
                        "'boleht', 'species' must all be ",
                        "supplied as column names."))
        }
        args <- c(rlang::ensyms(dbh = dbh, boleht = boleht, species = species),
                  ensym_cols(...))
        grs_vol <- data |>
            dplyr::rowwise() |>
            dplyr::mutate(grs_vol = get_grs_vol(!!!args,
                                                volume_config = volume_config,
                                                volume_coefficients = volume_coefficients,
                                                species_reference = species_reference)) |>
            dplyr::ungroup() |>
            dplyr::pull(grs_vol)
        return(grs_vol)
    } else if (class(data) != 'crm_data') {
        data <- prep_data(dbh, boleht, species,
                          volume_coefficients = volume_coefficients,
                          volume_config = volume_config, list(...))
    }
    if (!is.null(data$grs_vol)) {
        grs_vol <- data$grs_vol
    } else {

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

#' Compute oven-dry bole biomass for individual trees
#'
#' Computes stump biomass for individual tree's following the
#' Woodall et al. 2011 Component Ratio Method (CRM), and using Domke et al. 2011
#' density reduction and structural loss ratios for standing dead trees.
#'
#' If `data` is null then dbh, boleht, and species are required and an object of
#' type 'crm_data' will be built for you. If `data` inherits from data.frame
#' then additional arguments are expected to be names of columns in the
#' data.frame and the computations will be carried out rowwise. In this case
#' a vector of values will be returned in place of a single float.
#'
#' @inheritParams get_ag_biomass
#'
#' @return Float value indicating oven-dry bole biomass in lbs.
#' @export
#'
get_bole_biomass <- function(data = NULL, dbh = NULL, boleht = NULL, species = NULL,
                             cull = NULL, dc = NULL,
                             species_reference = NULL,
                             volume_coefficients = NULL,
                             volume_config = NULL, ...) {
    if (inherits(data, 'data.frame')) {
        if (any(c(missing(dbh),
                  missing(boleht),
                  missing(species),
                  missing(cull),
                  missing(dc)))) {
            stop(paste0("When 'data' argument is a data.frame, 'dbh', ",
                        "'boleht', 'species', 'cull', 'dc' must all be ",
                        "supplied as column names."))
        }
        args <- c(rlang::ensyms(dbh = dbh, boleht = boleht, species = species,
                                dc = dc, cull = cull),
                  ensym_cols(...))
        bole_biomass <- data |>
            dplyr::rowwise() |>
            dplyr::mutate(bole_biomass = get_bole_biomass(!!!args,
                                                            volume_config = volume_config,
                                                            volume_coefficients = volume_coefficients,
                                                            species_reference = species_reference)) |>
            dplyr::ungroup() |>
            dplyr::pull(bole_biomass)
        return(bole_biomass)

    } else if (class(data) != 'crm_data') {
        data <- do.call(prep_data,
                        c(list(dbh,
                               boleht,
                               species,
                               cull = cull,
                               dc = dc),
                          list(...)))
    }
    if (is.na(data$dbh) | is.null(data$dbh) | is.na(data$boleht) | is.null(data$boleht)) {
        total_bole_biomass <- 0
    } else {
        data$wood_spec_grav <- get_wood_spec_grav(data)
        data$bark_spec_grav <- get_bark_spec_grav(data)
        if (data$is_dead) {
            data$drf <- get_density_reduction_factor(data)
            data$sl_bole <- get_structural_loss(data, 'bole')
            data$sl_bark <- get_structural_loss(data, 'bark')
        }

        data$grs_vol <- get_grs_vol(data)
        data$snd_vol <- get_snd_vol(data)

        bole_wood_biomass <- get_bole_wood_biomass(data)
        bole_bark_biomass <- get_bole_bark_biomass(data)

        total_bole_biomass <- bole_wood_biomass + bole_bark_biomass
    }
    return(total_bole_biomass)
}

#' Compute oven-dry stump biomass for individual trees
#'
#' Computes stump biomass (0 to 1ft tall) for individual tree's following the
#' Woodall et al. 2011 Component Ratio Method (CRM), and using Domke et al. 2011
#' density reduction and structural loss ratios for standing dead trees.
#'
#' If `data` is null then dbh, boleht, and species are required and an object of
#' type 'crm_data' will be built for you. If `data` inherits from data.frame
#' then additional arguments are expected to be names of columns in the
#' data.frame and the computations will be carried out rowwise. In this case
#' a vector of values will be returned in place of a single float.
#'
#' @inheritParams get_ag_biomass
#'
#' @return Float value indicating oven-dry stump biomass in lbs.
#' @export
#'
get_stump_biomass <- function(data = NULL, dbh = NULL, boleht = NULL,
                              species = NULL, cull = NULL, dc = NULL,
                              species_reference = NULL,
                              volume_config = NULL,
                              volume_coefficients = NULL, ...) {
    if (inherits(data, 'data.frame')) {
        if (any(c(missing(dbh),
                  missing(boleht),
                  missing(species),
                  missing(cull),
                  missing(dc)))) {
            stop(paste0("When 'data' argument is a data.frame, 'dbh', ",
                        "'boleht', 'species', 'cull', 'dc' must all be ",
                        "supplied as column names."))
        }
        args <- c(rlang::ensyms(dbh = dbh, boleht = boleht, species = species,
                                dc = dc, cull = cull),
                  ensym_cols(...))
        stump_biomass <- data |>
            dplyr::rowwise() |>
            dplyr::mutate(stump_biomass = get_stump_biomass(!!!args,
                                                      volume_config = volume_config,
                                                      volume_coefficients = volume_coefficients,
                                                      species_reference = species_reference)) |>
            dplyr::ungroup() |>
            dplyr::pull(stump_biomass)
        return(stump_biomass)

    } else if (class(data) != 'crm_data') {
        data <- do.call(prep_data,
                        c(list(dbh,
                               boleht,
                               species,
                               cull = cull,
                               dc = dc),
                          list(...)))
    }
    if (is.na(data$dbh) | is.null(data$dbh) | is.na(data$boleht) | is.null(data$boleht)) {
        stump_biomass <- 0
    } else {
        data$wood_spec_grav <- get_wood_spec_grav(data)
        data$bark_spec_grav <- get_bark_spec_grav(data)
        if (data$is_dead) {

            # bole biomass needs to be recomputed without a DRF for stump biomass
            # not sure why... it goes against FIA prescription.
            # but it's the only way to match FIA data.
            data$bole_biomass <- NULL
            data$crm_adj_factor <- NULL
            data$sl_bole <- 1
            data$sl_bark <- 1
            data$drf <- 1

            data$sl_stump <- get_structural_loss(data, 'stump')

        }

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

#' Compute oven-dry top and branch biomass for individual trees
#'
#' Computes top and branch biomass for individual tree's following the
#' Woodall et al. 2011 Component Ratio Method (CRM), and using Domke et al. 2011
#' density reduction and structural loss ratios for standing dead trees.
#'
#' If `data` is null then dbh, boleht, and species are required and an object of
#' type 'crm_data' will be built for you. If `data` inherits from data.frame
#' then additional arguments are expected to be names of columns in the
#' data.frame and the computations will be carried out rowwise. In this case
#' a vector of values will be returned in place of a single float.
#'
#' @inheritParams get_ag_biomass
#'
#' @return Float value indicating oven-dry top and branch (sans foliage)
#' biomass in lbs.
#' @export
#'
get_top_biomass <- function(data = NULL, dbh = NULL, boleht = NULL,
                            species = NULL, cull = NULL, dc = NULL,
                            volume_config = NULL,
                            volume_coefficients = NULL,
                            species_reference = NULL, ...) {
    if (inherits(data, 'data.frame')) {
        if (any(c(missing(dbh),
                  missing(boleht),
                  missing(species),
                  missing(cull),
                  missing(dc)))) {
            stop(paste0("When 'data' argument is a data.frame, 'dbh', ",
                        "'boleht', 'species', 'cull', 'dc' must all be ",
                        "supplied as column names."))
        }
        args <- c(rlang::ensyms(dbh = dbh, boleht = boleht, species = species,
                                dc = dc, cull = cull),
                  ensym_cols(...))
        top_biomass <- data |>
            dplyr::rowwise() |>
            dplyr::mutate(top_biomass = get_top_biomass(!!!args,
                                                        volume_config = volume_config,
                                                        volume_coefficients = volume_coefficients,
                                                        species_reference = species_reference)) |>
            dplyr::ungroup() |>
            dplyr::pull(top_biomass)
        return(top_biomass)

    } else if (class(data) != 'crm_data') {
        data <- do.call(prep_data,
                        c(list(dbh,
                               boleht,
                               species,
                               cull = cull,
                               dc = dc),
                          list(...)))
    }
    if (is.na(data$dbh) | is.null(data$dbh) | is.na(data$boleht) | is.null(data$boleht)) {
        top_biomass <- 0
    } else {
        data$wood_spec_grav <- get_wood_spec_grav(data)
        data$bark_spec_grav <- get_bark_spec_grav(data)
        if (data$is_dead) {
            data$drf <- get_density_reduction_factor(data)
            data$sl_bole <- get_structural_loss(data, 'bole')
            data$sl_bark <- get_structural_loss(data, 'bark')
            data$sl_top <- get_structural_loss(data, 'top')
        }

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

#' Compute oven-dry aboveground biomass for individual trees
#'
#' Computes aboveground biomass for individual tree's following the
#' Woodall et al. 2011 Component Ratio Method (CRM), and using Domke et al. 2011
#' density reduction and structural loss ratios for standing dead trees.
#'
#' If `data` is null then dbh, boleht, and species are required and an object of
#' type 'crm_data' will be built for you. If `data` inherits from data.frame
#' then additional arguments are expected to be names of columns in the
#' data.frame and the computations will be carried out rowwise. In this case
#' a vector of values will be returned in place of a single float.
#'
#' @inheritParams prep_data
#' @param data an object of type `crm_data` (see prep_data), or a data.frame
#' @param ... optional arguments to be used in place of computed / looked up
#' arguments. See prep_data for full list of arguments.
#'
#' @return Float value indicating oven-dry total tree (sans foliage) aboveground
#' biomass in lbs.
#' @export
#'
get_ag_biomass <- function(data = NULL, dbh = NULL, boleht = NULL, species = NULL,
                           cull = NULL, dc = NULL,
                           volume_config = NULL, volume_coefficients = NULL,
                           species_reference = NULL, ...) {
    if (inherits(data, 'data.frame')) {
        if (any(c(missing(dbh),
                  missing(boleht),
                  missing(species),
                  missing(cull),
                  missing(dc)))) {
            stop(paste0("When 'data' argument is a data.frame, 'dbh', ",
                        "'boleht', 'species', 'cull', 'dc' must all be ",
                        "supplied as column names."))
        }
        args <- c(rlang::ensyms(dbh = dbh, boleht = boleht, species = species,
                                dc = dc, cull = cull),
                  ensym_cols(...))
        ag_biomass <- data |>
            dplyr::rowwise() |>
            dplyr::mutate(ag_biomass = get_ag_biomass(!!!args,
                                                      volume_config = volume_config,
                                                      volume_coefficients = volume_coefficients,
                                                      species_reference = species_reference)) |>
            dplyr::ungroup() |>
            dplyr::pull(ag_biomass)
        return(ag_biomass)

    } else if (class(data) != 'crm_data') {
        data <- do.call(prep_data,
                        c(list(dbh,
                               boleht,
                               species,
                               cull = cull,
                               dc = dc),
                          list(...)))
    }
    if (is.na(data$dbh)) {
        ag_biomass <- 0
    } else if (data$dbh < 5) {
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
        data$wood_spec_grav <- get_wood_spec_grav(data)
        data$bark_spec_grav <- get_bark_spec_grav(data)
        if (data$is_dead) {
            data$drf <- get_density_reduction_factor(data)
            data$sl_top <- get_structural_loss(data, 'top')
            data$sl_bole <- get_structural_loss(data, 'bole')
            data$sl_bark <- get_structural_loss(data, 'bark')
        }

        data$jenkins_total_biomass <- get_jenkins_total_biomass(data)
        data$jenkins_bole_biomass <- get_jenkins_bole_biomass(data)
        data$bole_biomass <- ifelse(is.null(data$bole_biomass),
                                            get_bole_biomass(data),
                                            data$bole_biomass)
        data$crm_adj_factor <- get_crm_adjustment(data)
        data$raile_stump_bio <- get_raile_stump_biomass(data)
        data$stump_biomass <- ifelse(is.null(data$stump_biomass),
                                     get_stump_biomass(data),
                                     data$stump_biomass)
        data$top_biomass <- ifelse(is.null(data$top_biomass),
                                   get_top_biomass(data),
                                   data$top_biomass)
        ag_biomass <- data$bole_biomass + data$stump_biomass + data$top_biomass
    }

    return(ag_biomass)
}

