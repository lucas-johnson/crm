url <- 'https://www.fs.usda.gov/nrs/pubs/gtr/nrs_gtr88/nrs_gtr88.zip'
zip_file <- tempfile()
download.file(url, zip_file)
files <- unzip(zip_file, exdir = tempdir())
coef_file <- file.path(dirname(files[1]), "volcfgrs_eqn_coefs.xlsx")
volume_config_northeast <- readxl::read_excel(coef_file, sheet = 'NE_config')
usethis::use_data(volume_config_northeast, overwrite = TRUE)
