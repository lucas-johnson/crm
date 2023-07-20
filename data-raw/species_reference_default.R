## code to prepare `species_reference` dataset goes here
species_ref_file <- tempfile()
download.file("https://apps.fs.usda.gov/fia/datamart/CSV/REF_SPECIES.csv",
              species_ref_file)
species_reference_default <- read.csv(species_ref_file)
usethis::use_data(species_reference_default, overwrite = TRUE)
