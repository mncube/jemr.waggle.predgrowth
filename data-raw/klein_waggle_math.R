## code to prepare `klein_waggle_math` dataset goes here
klein_waggle_math <- haven::read_sav(paste0(getwd(), "/rawdatafiles/Klein Waggle Math.sav"))

usethis::use_data(klein_waggle_math, overwrite = TRUE)
