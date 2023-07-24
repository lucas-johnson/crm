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
                 c(402.00576, 0))

    # TODO: add dead tests when dead biomass for top is fixed
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

    # DRFs not used when doing stump biomass
    dd <- get_dummy_data(dc = 3)
    expect_equal(get_stump_biomass(dd), 20.489597)

    # bark/bole slrs not used when doing stump biomass
    dd <- get_dummy_data(dc = 3, sl_bark = 0, sl_bole = 0)
    expect_equal(get_stump_biomass(dd), 20.489597)

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
    df <- get_dummy_df(snd_vol = c(10, 0))
    expect_equal(get_stump_biomass(df, DIA, BOLEHT, SPCD, CULL, DC,
                                 snd_vol = snd_vol),
                 c(25.4569792, 0))

    # TODO: add dead tests when dead biomass for top is fixed
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

    # df support
    df <- get_dummy_df()
    expect_equal(get_top_biomass(df, DIA, BOLEHT, SPCD, CULL, DC),
                 c(96.054764, 237.854928))
    df <- get_dummy_df(cull = c(50, 30))
    expect_equal(get_top_biomass(df, DIA, BOLEHT, SPCD, CULL, DC),
                 c(96.054764 * .5, 237.854928 * .7))
    df <- get_dummy_df(snd_vol = c(0, 24.435094))
    expect_equal(get_top_biomass(df, DIA, BOLEHT, SPCD, CULL, DC,
                                 snd_vol = snd_vol),
                 c(0, 237.854925))

    # TODO: add dead tests when dead biomass for top is fixed
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

    # TODO: add dead tests when dead biomass for top is fixed

})



