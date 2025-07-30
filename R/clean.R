clean <- function(dat){
  dat1 <- dat |>
    dplyr::filter(grade %in% c("K", "1", "2", "3", "4", "5")) |>
    dplyr::mutate(grade_grp = ifelse(grade %in% c("K", "1", "2"), "GK2", "G35")) |>
    dplyr::mutate(grade = factor(grade, levels = c("K", "1", "2", "3", "4", "5"),
                                 labels = c("K", "1", "2", "3", "4", "5"))) |>
    dplyr::mutate(grade_grp = factor(grade_grp, levels = c("GK2", "G35"),
                                     labels = c("GK2", "G35"))) |>
    dplyr::mutate(outcome = mapm_f_projected_spring_rit - mapm_f_assess_score)
}
