## code to prepare `bluesprings_waggle_ela` dataset goes here
bluesprings_waggle_ela <- haven::read_sav(paste0(getwd(), "/rawdatafiles/Blue Springs Waggle ELA MAP.sav"))

usethis::use_data(bluesprings_waggle_ela, overwrite = TRUE)
