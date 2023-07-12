get_dia_residual <- function(mean = -0.004, sd = 0.55, units = 'inch') {
    res <- rnorm(1, mean = mean, sd = sd)
    if (units != 'cm') {
        res <- measurements::conv_unit(res, 'cm', units)
    }
    return(res)
}

get_ht_residual <- function(mean = -0.05, sd = 1.52, units = 'ft') {
    res <- rnorm(1, mean = mean, sd = sd)
    if (units != 'm') {
        res <- measurements::conv_unit(res, 'm', units)
    }
    return(res)
}

get_cull_residual <- function(mean = 0.1, sd = 3.5) {
    rnorm(1, mean = mean, sd = sd)
}

get_decaycd_residual <- function(threshold = 0.59) {
    if (runif(1) <= threshold) {
        return(0)
    } else {
        sample(c(-1, 1), 1)
    }
}

hardwood_or_softwood <- function(MAJOR_SPGRPCD) {
    switch(
        MAJOR_SPGRPCD,
        'hardwood',
        'hardwood',
        'softwood',
        'softwood'
    )
}

get_drf_residual <- function(drf,
                             DECAYCD,
                             MAJOR_SPGRPCD,
                             drf_sd_csv_path = here::here('data/harmon_2011_drf_sd.csv')) {

    wood_type <- hardwood_or_softwood(MAJOR_SPGRPCD)
    sd <- read.csv(drf_sd_csv_path) |>
        dplyr::filter(type == wood_type) |>
        dplyr::filter(class == DECAYCD) |>
        dplyr::pull(drf_sd)
    drf - rnorm(1, mean = drf, sd = sd)

}

get_specific_grav_residual <- function(x) {
    x - rnorm(1, mean = x, sd = (.1 * x))
}

get_jenkins_03_residual <- function(agb,
                                    jenkins_grp,
                                    jenkins_error_csv_path = here::here('data/jenkins_error_dist.csv')) {
    z_score <- 1.282 # 80% CI (10th - 90th)
    relative_residual <- read.csv(jenkins_error_csv_path) |>
        dplyr::filter(code == jenkins_grp) |>
        dplyr::mutate(
            mean_error = (p90 + p10) / 2,
            std_dev = (p90 - mean_error) / z_score,
            residual = rnorm(1, mean = mean_error, sd = std_dev)
        ) |> dplyr::pull(residual)
    agb + (relative_residual * agb)
}
