clean <- function(dat){
  dat1 <- dat |>
    dplyr::filter(grade %in% c("K", "1", "2", "3", "4", "5")) |>
    dplyr::mutate(grade_grp = ifelse(grade %in% c("K", "1", "2"), "GK2", "G35"))
}
