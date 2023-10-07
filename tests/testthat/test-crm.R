get_dummy_data <- function(dc = NULL, cull = 0, ...) {
    # 10 inch, 20 foot bole, sugar maple
    do.call(prep_data, c(list(10, 20, 318, dc = dc, cull = cull), ...))
}

get_dummy_df <- function(dc = NA, cull = 0, ...) {
    # 10 inch, 20 foot bole, sugar maple
    data.frame(
        DIA = c(10, 15),
        BOLEHT = c(20, 35),
        SPCD = c(318, 802),
        DC = dc,
        CULL = cull,
        ...
    )
}

test_that("default get_bark_percent works", {
    dd <- get_dummy_data()
    expect_equal(get_bark_percent(dd), 15.6)

    dd <- get_dummy_data(bark_percent = 1000)
    expect_equal(get_bark_percent(dd), 1000)
})

test_that("default get_wood_spec_grav works", {
    dd <- get_dummy_data()
    expect_equal(get_wood_spec_grav(dd), 0.56)

    dd <- get_dummy_data(wood_spec_grav = 1000)
    expect_equal(get_wood_spec_grav(dd), 1000)
})

test_that("default get_bark_spec_grav works", {
    dd <- get_dummy_data()
    expect_equal(get_bark_spec_grav(dd), 0.54)

    dd <- get_dummy_data(bark_spec_grav = 1000)
    expect_equal(get_bark_spec_grav(dd), 1000)
})

test_that("default get_bark_spec_grav works", {
    dd <- get_dummy_data()
    expect_equal(get_bark_spec_grav(dd), 0.54)

    dd <- get_dummy_data(bark_spec_grav = 1000)
    expect_equal(get_bark_spec_grav(dd), 1000)
})

test_that("default get_density_reduction_factor works", {
    # alive
    dd <- get_dummy_data()
    expect_equal(get_density_reduction_factor(dd), 1)

    # dead - decaycd lookups
    dd <- get_dummy_data(dc = 1)
    expect_equal(get_density_reduction_factor(dd), 0.956)
    dd <- get_dummy_data(dc = 2)
    expect_equal(get_density_reduction_factor(dd), 0.761)
    dd <- get_dummy_data(dc = 3)
    expect_equal(get_density_reduction_factor(dd), 0.526)
    dd <- get_dummy_data(dc = 4)
    expect_equal(get_density_reduction_factor(dd), 0.45)
    dd <- get_dummy_data(dc = 5)
    expect_equal(get_density_reduction_factor(dd), 0.45)

    # override the decaycd lookup
    dd <- get_dummy_data(dc = 5, drf = 0)
    expect_equal(get_density_reduction_factor(dd), 0)
})


test_that("default get_structural_loss works", {
    dd <- get_dummy_data()
    expect_equal(get_structural_loss(dd, 'bole'), 1)
    expect_equal(get_structural_loss(dd, 'bark'), 1)
    expect_equal(get_structural_loss(dd, 'stump'), 1)
    expect_equal(get_structural_loss(dd, 'top'), 1)
    expect_equal(get_structural_loss(dd, 'crap'), 1)

    dd <- get_dummy_data(dc = 1)
    expect_error(get_structural_loss(dd, 'crap'))

    dd <- get_dummy_data(dc = 3)
    expect_equal(get_structural_loss(dd, 'bole'), 1)
    dd <- get_dummy_data(dc = 3)
    expect_equal(get_structural_loss(dd, 'stump'), 1)
    dd <- get_dummy_data(dc = 3)
    expect_equal(get_structural_loss(dd, 'top'), 0.2)
    dd <- get_dummy_data(dc = 3)
    expect_equal(get_structural_loss(dd, 'bark'), 0.39)
})

test_that("remove_cull works", {
    expect_equal(remove_cull(100, 5), 95)
    expect_equal(remove_cull(100, 97), 3)
    expect_equal(remove_cull(100, 98), 2)
    expect_equal(remove_cull(100, 99), 0)
    expect_equal(remove_cull(100, 100), 0)
})

test_that("get_jenkins_component works", {
    dbh <- 1.07019
    b0 <- -1.693147
    b1 <- exp(1)
    expect_equal(get_jenkins_component(dbh, b0, b1, 100), 50, tolerance = 0.000001)
})

test_that("get_jenkins_total_biomass works", {
    dd <- get_dummy_data()
    expect_equal(get_jenkins_total_biomass(dd), 774.24059)

    dd$jenkins_total_biomass <- -77
    expect_equal(get_jenkins_total_biomass(dd), -77)

    expect_equal(get_jenkins_total_biomass(dbh = dd$dbh, species = dd$species),
                 774.24059)

    # df support
    df <- get_dummy_df()
    expect_equal(get_jenkins_total_biomass(df, DIA, SPCD),
                 c(774.240585, 2077.386361))
    df <- get_dummy_df(cull = c(50, 30))
    expect_equal(get_jenkins_total_biomass(df, DIA, SPCD, cull = CULL),
                 c(774.240585, 2077.386361))
    df <- get_dummy_df(snd_vol = c(0, 24.435094))
    expect_equal(get_jenkins_total_biomass(df, DIA, SPCD, snd_vol = snd_vol),
                 c(774.240585, 2077.386361))
    df <- get_dummy_df(jenkins_total_biomass = c(0, 10))
    expect_equal(get_jenkins_total_biomass(df, DIA, SPCD,
                                           jenkins_total_biomass = jenkins_total_biomass),
                 c(0, 10))
})

test_that("get_jenkins_foliage_biomass works", {
    dd <- get_dummy_data()
    expect_equal(get_jenkins_foliage_biomass(dd), 16.4799042)

    # df support
    df <- get_dummy_df()
    expect_equal(get_jenkins_foliage_biomass(df, DIA, SPCD),
                 c(16.4799042, 40.9330796))

    # snd vol and cull not used in the computation
    df <- get_dummy_df(cull = c(50, 30))
    expect_equal(get_jenkins_foliage_biomass(df, DIA, SPCD, cull = CULL),
                 c(16.4799042, 40.9330796))
    df <- get_dummy_df(snd_vol = c(0, 24.435094))
    expect_equal(get_jenkins_foliage_biomass(df, DIA, SPCD, snd_vol = snd_vol),
                 c(16.4799042, 40.9330796))

    # Fraction of total boimass
    df <- get_dummy_df(jenkins_total_biomass = c(0, 2077.386361))
    expect_equal(get_jenkins_foliage_biomass(df, DIA, SPCD,
                                             jenkins_total_biomass = jenkins_total_biomass),
                 c(0, 40.9330796))

    # foliage already exists
    df <- get_dummy_df(jenkins_foliage_biomass = c(100, -99))
    expect_equal(get_jenkins_foliage_biomass(df, DIA, SPCD,
                                             jenkins_foliage_biomass = jenkins_foliage_biomass),
                 c(100, -99))
})

test_that("get_jenkins_bole_wood_biomass works", {
    dd <- get_dummy_data()
    expect_equal(get_jenkins_bole_wood_biomass(dd), 460.282)

    dd$jenkins_bole_wood_biomass <- -77
    expect_equal(get_jenkins_bole_wood_biomass(dd), -77)

    # df support
    df <- get_dummy_df()
    expect_equal(get_jenkins_bole_wood_biomass(df, DIA, SPCD),
                 c(460.282, 1326.108))

    # snd vol and cull not used in the computation
    df <- get_dummy_df(cull = c(50, 30))
    expect_equal(get_jenkins_bole_wood_biomass(df, DIA, SPCD, cull = CULL),
                 c(460.282, 1326.108))
    df <- get_dummy_df(snd_vol = c(0, 24.435094))
    expect_equal(get_jenkins_bole_wood_biomass(df, DIA, SPCD, snd_vol = snd_vol),
                 c(460.282, 1326.108))

    # Fraction of total boimass
    df <- get_dummy_df(jenkins_total_biomass = c(0, 1000))
    expect_equal(get_jenkins_bole_wood_biomass(df, DIA, SPCD,
                                             jenkins_total_biomass = jenkins_total_biomass),
                 c(0, 638.35405))

    # bole wood already exists
    df <- get_dummy_df(jenkins_bole_wood_biomass = c(100, -99))
    expect_equal(get_jenkins_bole_wood_biomass(df, DIA, SPCD,
                                             jenkins_bole_wood_biomass = jenkins_bole_wood_biomass),
                 c(100, -99))
})

test_that("get_jenkins_bole_bark_biomass works", {
    dd <- get_dummy_data()
    expect_equal(get_jenkins_bole_bark_biomass(dd), 96.81687)

    dd$jenkins_bole_bark_biomass <- -77
    expect_equal(get_jenkins_bole_bark_biomass(dd), -77)

    # df support
    df <- get_dummy_df()
    expect_equal(get_jenkins_bole_bark_biomass(df, DIA, SPCD),
                 c(96.81687, 265.56462))

    # snd vol and cull not used in the computation
    df <- get_dummy_df(cull = c(50, 30))
    expect_equal(get_jenkins_bole_bark_biomass(df, DIA, SPCD, cull = CULL),
                 c(96.81687, 265.56462))
    df <- get_dummy_df(snd_vol = c(0, 24.435094))
    expect_equal(get_jenkins_bole_bark_biomass(df, DIA, SPCD, snd_vol = snd_vol),
                 c(96.81687, 265.56462))

    # Fraction of total boimass
    df <- get_dummy_df(jenkins_total_biomass = c(0, 1000))
    expect_equal(get_jenkins_bole_bark_biomass(df, DIA, SPCD,
                                               jenkins_total_biomass = jenkins_total_biomass),
                 c(0, 127.835933))

    # bole wood already exists
    df <- get_dummy_df(jenkins_bole_bark_biomass = c(100, -99))
    expect_equal(get_jenkins_bole_bark_biomass(df, DIA, SPCD,
                                               jenkins_bole_bark_biomass = jenkins_bole_bark_biomass),
                 c(100, -99))
})

test_that("get_jenkins_bole_biomass works", {
    dd <- get_dummy_data()
    expect_equal(get_jenkins_bole_biomass(dd), 557.09887)

    dd$jenkins_bole_bark_biomass <- 0
    expect_equal(get_jenkins_bole_biomass(dd), 460.282)

    dd$jenkins_bole_bark_biomass <- 100
    dd$jenkins_bole_wood_biomass <- 0
    expect_equal(get_jenkins_bole_biomass(dd), 100)


    dd$jenkins_bole_biomass <- -88
    expect_equal(get_jenkins_bole_biomass(dd), -88)

    # df support
    df <- get_dummy_df()
    expect_equal(get_jenkins_bole_biomass(df, DIA, SPCD),
                 c(557.09887, 1591.67263))

    # snd vol and cull not used in the computation
    df <- get_dummy_df(cull = c(50, 30))
    expect_equal(get_jenkins_bole_biomass(df, DIA, SPCD, cull = CULL),
                 c(557.09887, 1591.67263))
    df <- get_dummy_df(snd_vol = c(0, 24.435094))
    expect_equal(get_jenkins_bole_biomass(df, DIA, SPCD, snd_vol = snd_vol),
                 c(557.09887, 1591.67263))

    # Fraction of total boimass
    df <- get_dummy_df(jenkins_total_biomass = c(0, 1000))
    expect_equal(get_jenkins_bole_biomass(df, DIA, SPCD,
                                               jenkins_total_biomass = jenkins_total_biomass),
                 c(0, 766.18999))

    # bole already exists
    df <- get_dummy_df(jenkins_bole_biomass = c(100, -99))
    expect_equal(get_jenkins_bole_biomass(df, DIA, SPCD,
                                          jenkins_bole_biomass = jenkins_bole_biomass),
                 c(100, -99))

    # bole bark and bole wood already exist
    df$jenkins_bole_wood_biomass = c(1, 10)
    df$jenkins_bole_bark_biomass = c(-1, -5)
    expect_equal(get_jenkins_bole_biomass(df, DIA, SPCD,
                                          jenkins_bole_wood_biomass = jenkins_bole_wood_biomass,
                                          jenkins_bole_bark_biomass = jenkins_bole_bark_biomass),
                 c(0, 5))
})

test_that("get_stump_volume works", {
    expect_equal(get_stump_volume(0, 0, 0), 0)
    expect_equal(get_stump_volume(1, 1, 1), 0.08249408, tolerance = 0.000001)
})

test_that("get_raile_stump_biomass works", {
    dd <- get_dummy_data()
    expect_equal(get_raile_stump_biomass(dd), 35.278237)

    dd$bark_spec_grav <- 0
    dd$wood_spec_grav <- 0
    expect_equal(get_raile_stump_biomass(dd), 0)

    # df support
    df <- get_dummy_df()
    expect_equal(get_raile_stump_biomass(df, DIA, SPCD),
                 c(35.2782367, 84.7246607))

    # snd vol and cull not used in the computation
    df <- get_dummy_df(cull = c(50, 30))
    expect_equal(get_raile_stump_biomass(df, DIA, SPCD, cull = CULL),
                 c(35.2782367, 84.7246607))
    df <- get_dummy_df(snd_vol = c(0, 24.435094))
    expect_equal(get_raile_stump_biomass(df, DIA, SPCD, snd_vol = snd_vol),
                 c(35.2782367, 84.7246607))

    # Fraction of total boimass
    df <- get_dummy_df(jenkins_total_biomass = c(0, 1000))
    expect_equal(get_raile_stump_biomass(df, DIA, SPCD,
                                          jenkins_total_biomass = jenkins_total_biomass),
                 c(35.2782367, 84.7246607))

    # bole already exists
    df <- get_dummy_df(raile_stump_biomass = c(100, -99))
    expect_equal(get_raile_stump_biomass(df, DIA, SPCD,
                                          raile_stump_biomass = raile_stump_biomass),
                 c(100, -99))
})

test_that('get_bole_biomass works', {
    dd <- get_dummy_data()
    expect_equal(get_bole_biomass(dd), 323.56297)
    expect_equal(get_bole_biomass(dbh = dd$dbh,
                                  boleht = dd$boleht,
                                  species = dd$species),
                 323.56297)

    dd <- get_dummy_data(cull = 50)
    expect_equal(get_bole_biomass(dd), 323.56297 / 2)

    dd <- get_dummy_data(dc = 3)
    expect_equal(get_bole_biomass(dd), 156.61896)

    dd <- get_dummy_data(dc = 3, sl_bole = 0)
    expect_equal(get_bole_biomass(dd), 8.679203)

    dd <- get_dummy_data(snd_vol = 100)
    expect_equal(get_bole_biomass(dd), 4020.0576)

    # df support
    df <- get_dummy_df()
    expect_equal(get_bole_biomass(df, DIA, BOLEHT, SPCD, CULL, DC),
                 c(323.56297, 1051.46752))
    df <- get_dummy_df(cull = c(50, 30))
    expect_equal(get_bole_biomass(df, DIA, BOLEHT, SPCD, CULL, DC),
                 c(323.56297 * .5, 1051.46752 * .7))
    df <- get_dummy_df(snd_vol = c(10, 0))
    expect_equal(get_bole_biomass(df, DIA, BOLEHT, SPCD, CULL, DC,
                                   snd_vol = snd_vol),
                 c(402.00576, 1006.3656))

})


test_that('get_stump_biomass works', {
    dd <- get_dummy_data()
    expect_equal(get_stump_biomass(dd), 20.489597)
    expect_equal(get_stump_biomass(dbh = dd$dbh,
                                   boleht = dd$boleht,
                                   species = dd$species),
                 20.489597)

    dd <- get_dummy_data(cull = 50)
    expect_equal(get_stump_biomass(dd), 20.489597 / 2)

    dd <- get_dummy_data(dc = 3)
    expect_equal(get_stump_biomass(dd), 9.9178818)

    # bark/bole slrs not used when doing stump biomass
    dd <- get_dummy_data(dc = 3, sl_bark = .5, sl_bole = .5)
    expect_equal(get_stump_biomass(dd), 5.388764)

    dd <- get_dummy_data(dc = 3, sl_stump = 0)
    expect_equal(get_stump_biomass(dd), 0)

    dd <- get_dummy_data(snd_vol = 100)
    expect_equal(get_stump_biomass(dd), 254.569792)

    # df support
    df <- get_dummy_df()
    expect_equal(get_stump_biomass(df, DIA, BOLEHT, SPCD, CULL, DC),
                 c(20.489597, 55.969568))
    df <- get_dummy_df(cull = c(50, 30))
    expect_equal(get_stump_biomass(df, DIA, BOLEHT, SPCD, CULL, DC),
                 c(20.489597 * .5, 55.969568 * .7))

    # When snd_vol is 0, roll over to grs_vol * pct_snd
    df <- get_dummy_df(snd_vol = c(10, 0), grs_vol = c(-1, 0))
    expect_equal(get_stump_biomass(df, DIA, BOLEHT, SPCD, CULL, DC,
                                 snd_vol = snd_vol, grs_vol = grs_vol),
                 c(25.4569792, 0))

})

test_that('get_top_biomass works', {

    dd <- get_dummy_data()
    expect_equal(get_top_biomass(dd), 96.054764)
    expect_equal(get_top_biomass(dbh = dd$dbh,
                                boleht = dd$boleht,
                                species = dd$species),
                 96.054764)

    dd <- get_dummy_data(cull = 40)
    expect_equal(get_top_biomass(dd), 96.054764 * .6)

    dd <- get_dummy_data(dc = 5)
    expect_equal(get_top_biomass(dd), 0)
    dd <- get_dummy_data(dc = 4)
    expect_equal(get_top_biomass(dd), 3.8759564)
    dd <- get_dummy_data(dc = 4, sl_top = 0)
    expect_equal(get_top_biomass(dd), 0)
    dd <- get_dummy_data(dc = 4, cull = 50)
    expect_equal(get_top_biomass(dd), 3.8759564 * 0.5)
    dd <- get_dummy_data(snd_vol = 100)
    expect_equal(get_top_biomass(dd), 1193.4174)

    # df support
    df <- get_dummy_df()
    expect_equal(get_top_biomass(df, DIA, BOLEHT, SPCD, CULL, DC),
                 c(96.054764, 237.854928))
    df <- get_dummy_df(cull = c(50, 30))
    expect_equal(get_top_biomass(df, DIA, BOLEHT, SPCD, CULL, DC),
                 c(96.054764 * .5, 237.854928 * .7))
    df <- get_dummy_df(snd_vol = c(0, 24.435094), grs_vol = c(0, NULL))
    expect_equal(get_top_biomass(df, DIA, BOLEHT, SPCD, CULL, DC,
                                 snd_vol = snd_vol, grs_vol = grs_vol),
                 c(0, 237.854925))

})

test_that('get_ag_biomass works', {

    dd <- get_dummy_data()
    expect_equal(get_ag_biomass(dd), 440.10734)
    expect_equal(get_ag_biomass(dbh = dd$dbh,
                                boleht = dd$boleht,
                                species = dd$species),
                 440.10734)

    dd <- get_dummy_data(cull = 40)
    expect_equal(get_ag_biomass(dd), 440.10734 * .6)

    dd <- get_dummy_data(bole_biomass = 10, stump_biomass = -5, top_biomass = 2)
    expect_equal(get_ag_biomass(dd), 7)

    dd <- get_dummy_data(dc = 3)
    expect_equal(get_ag_biomass(dd),
                 get_top_biomass(dd) +
                     get_stump_biomass(dd) +
                     get_bole_biomass(dd))

    dd <- get_dummy_data(dc = 3)
    expect_equal(get_ag_biomass(dd), 175.835804)

    # df support
    df <- get_dummy_df()
    expect_equal(get_ag_biomass(df, DIA, BOLEHT, SPCD, CULL, DC),
                 c(440.10734, 1345.29202))
    df <- get_dummy_df(cull = c(50, 30))
    expect_equal(get_ag_biomass(df, DIA, BOLEHT, SPCD, CULL, DC),
                 c(440.10734 * .5, 1345.29202 * .7))

    df <- get_dummy_df(bb = c(-10, 0),
                       tb = c(-10, 0),
                       sb = c(-10, 0))
    expect_equal(get_ag_biomass(df, DIA, BOLEHT, SPCD, CULL, DC,
                                bole_biomass = bb,
                                top_biomass = tb,
                                stump_biomass = sb),
                 c(-30, 0))

    df <- get_dummy_df(snd_vol = c(0, 10), grs_vol = c(0, -500))
    expect_equal(get_ag_biomass(df, DIA, BOLEHT, SPCD, CULL, DC,
                                grs_vol = grs_vol, snd_vol = snd_vol),
                 c(0, 550.55733))

})

test_that('Brian Walters calcs match ours', {
    #' most of script that Brian F Walters sent me. His version to cook up
    #' corrected AGB/Carbon for standing dead trees.
    refspp <- crm::species_reference_default |>
        dplyr::as_tibble() |>
        dplyr::rename_with(tolower)
    rapscr <- crm::ref_avg_pct_snd_default |>
        dplyr::as_tibble() |>
        dplyr::rename_with(tolower)
    max_spgrp_dia <- rapscr %>%
        dplyr::filter(diameter_round != 999) %>%
        dplyr::group_by(spgrpcd) %>%
        dplyr::summarise(max_spgrp_dia = max(diameter_round)) %>%
        dplyr::ungroup()
    drf <- refspp %>%
        dplyr::select(spcd, standing_dead_decay_ratio1:standing_dead_decay_ratio5) %>%
        tidyr::pivot_longer(standing_dead_decay_ratio1:standing_dead_decay_ratio5,
                            names_to = "decaycd", values_to = "standing_dead_decay_ratio") %>%
        dplyr::mutate(decaycd = as.numeric(stringr::str_sub(decaycd, -1))) %>%
        dplyr::arrange(spcd, decaycd)
    # Make the structural loss adjustments table
    sla <- dplyr::tibble(decaycd = 1:5,
                  bole_sla = rep(1,5),
                  bark_sla = c(0.92,0.66,0.39,0.21,0),
                  top_sla = c(1,0.5,0.2,0.1,0),
                  stump_sla = rep(1,5),
                  roots_sla = c(1,0.95,0.8,0.65,0.5))
    std_trees <- brian_f_walters_check |>
        dplyr::as_tibble() %>%
        dplyr::rename_with(tolower)
    std_trees <- std_trees %>%
        dplyr::inner_join(refspp %>%
                       dplyr::select(spcd, sftwd_hrdwd, woodland, jenkins_spgrpcd:wood_spgr_greenvol_drywt,
                              bark_spgr_greenvol_drywt, mc_pct_green_bark, mc_pct_green_wood,
                              wood_spgr_mc12vol_drywt, bark_vol_pct, raile_stump_dob_b1:raile_stump_dib_b2),
                   by = "spcd") %>%
        dplyr::inner_join(drf, by = c("spcd","decaycd")) %>%
        dplyr::inner_join(sla, by = "decaycd") %>%
        dplyr::inner_join(max_spgrp_dia, by = c("jenkins_spgrpcd" = "spgrpcd")) %>%
        dplyr::mutate(diameter_round = floor(dia),
               diameter_round = dplyr::case_when(diameter_round > max_spgrp_dia ~ 999,
                                          dia < 5 ~ 5,
                                          TRUE ~ diameter_round)) %>%
        dplyr::inner_join(rapscr, by = c("jenkins_spgrpcd" = "spgrpcd", "diameter_round"))
    new_std_tree <- std_trees %>%
        dplyr::filter(diahtcd == 1 & dia >= 5.0) %>%
        dplyr::mutate(volume_to_use = dplyr::case_when(volcfsnd > 0 ~ volcfsnd,
                                         volcfgrs > 0 ~ volcfgrs * pct_snd,
                                         volcfnet > 0 ~ volcfnet * ratio_cfsnd_cfnet,
                                         TRUE ~ NA_real_),
               total_AG_biomass_Jenkins = exp(jenkins_total_b1 + jenkins_total_b2 * log(dia * 2.54)) * 2.2046,
               stem_ratio = exp(jenkins_stem_wood_ratio_b1 + jenkins_stem_wood_ratio_b2 / (dia * 2.54)),
               bark_ratio = exp(jenkins_stem_bark_ratio_b1 + jenkins_stem_bark_ratio_b2 / (dia * 2.54)),
               foliage_ratio = exp(jenkins_foliage_ratio_b1 + jenkins_foliage_ratio_b2 / (dia * 2.54)),
               root_ratio = exp(jenkins_root_ratio_b1 + jenkins_root_ratio_b2 / (dia * 2.54)),
               stem_biomass_Jenkins = total_AG_biomass_Jenkins * stem_ratio,
               bark_biomass_Jenkins = total_AG_biomass_Jenkins * bark_ratio,
               bole_biomass_Jenkins = stem_biomass_Jenkins + bark_biomass_Jenkins,
               foliage_biomass_Jenkins = total_AG_biomass_Jenkins * foliage_ratio,
               root_biomass_Jenkins = total_AG_biomass_Jenkins * root_ratio,
               stump_vosb = (pi * dia^2)/(4*144) *
                   (((1 - raile_stump_dob_b1)^2 * 1 + 11 * raile_stump_dob_b1 * (1 - raile_stump_dob_b1) * log(1 + 1) - (30.25/(1 + 1)) * raile_stump_dob_b1^2) -
                        ((1 - raile_stump_dob_b1)^2 * 0 + 11 * raile_stump_dob_b1 * (1 - raile_stump_dob_b1) * log(0 + 1) - (30.25/(0 + 1)) * raile_stump_dob_b1^2)),
               stump_visb = (pi * dia^2)/(4*144) *
                   (((raile_stump_dib_b1 - raile_stump_dib_b2)^2 * 1 + 11 * raile_stump_dib_b2 * (raile_stump_dib_b1 - raile_stump_dib_b2) * log(1 + 1) - (30.25/(1 + 1)) * raile_stump_dib_b2^2) -
                        ((raile_stump_dib_b1 - raile_stump_dib_b2)^2 * 0 + 11 * raile_stump_dib_b2 * (raile_stump_dib_b1 - raile_stump_dib_b2) * log(0 + 1) - (30.25/(0 + 1)) * raile_stump_dib_b2^2)),
               stump_biomass_Raile = (stump_visb * wood_spgr_greenvol_drywt * 62.4) + ((stump_vosb - stump_visb) * bark_spgr_greenvol_drywt * 62.4),
               top_biomass_Jenkins = total_AG_biomass_Jenkins - bole_biomass_Jenkins - foliage_biomass_Jenkins - stump_biomass_Raile,
               drybio_bole_calc = ifelse(!is.na(volume_to_use),
                                         (volume_to_use * wood_spgr_greenvol_drywt * 62.4 * standing_dead_decay_ratio * bole_sla) +
                                             (volume_to_use * (bark_vol_pct/100) * bark_spgr_greenvol_drywt * 62.4 * standing_dead_decay_ratio * bark_sla),
                                         ((stem_biomass_Jenkins * standing_dead_decay_ratio * bole_sla) +
                                              (bark_biomass_Jenkins * standing_dead_decay_ratio * bark_sla)) * crm_bole_over_jenkins_tot),
               CRMadj = drybio_bole_calc/bole_biomass_Jenkins,
               drybio_stump_calc = stump_biomass_Raile * CRMadj * stump_sla,
               drybio_top_calc = top_biomass_Jenkins * CRMadj * top_sla,
               drybio_bg_calc = root_biomass_Jenkins * CRMadj * roots_sla,
               drybio_ag_calc = drybio_bole_calc + drybio_top_calc + drybio_stump_calc,
               carbon_ag_calc = drybio_ag_calc * 0.5,
               carbon_bg_calc = drybio_bg_calc * 0.5,
               drybio_sapling_calc = NA_real_,
               drybio_wdld_spp_calc = NA_real_)


    expect_equal(new_std_tree |> dplyr::pull(drybio_ag_calc) |> round(1),
                 get_ag_biomass(brian_f_walters_check, dbh = DIA,
                                boleht = BOLEHT, species = SPCD, cull = CULL,
                                dc = DECAYCD,  grs_vol = VOLCFGRS,
                                snd_vol = VOLCFSND) |> round(1))
})



