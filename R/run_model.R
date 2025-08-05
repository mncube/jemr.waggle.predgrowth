get_analytic_sample <- function(dat,
                                # .analysis = c("growth", "growth_std", "badj"),
                                .group = NULL
){

  # .analysis <- match.arg(.analysis)

  # if (.analysis == "growth"){
    # dat <- dat |> dplyr::mutate(outcome = mapm_f_projected_spring_rit - mapm_f_assess_score)
    # .assess <- c("outcome")
  # } else if (.analysis == "growth_std") {
  #   dat <- dat |> dplyr::mutate(outcome = ASSESS_SCORE_fstd_out - ASSESS_SCORE_fstd)
  #   .assess <- c("outcome")
  # } else if (.analysis == "badj") {
  #   dat <- dat |>
  #     dplyr::mutate(outcome = ASSESS_SCORE_out) |>
  #     dplyr::mutate(baseline = ASSESS_SCORE)
  #   .assess <- c("outcome", "baseline")
  # } else {
  #   stop("must use 'growth', 'growth_std', or 'badj' for .analysis")
  # }

  # Filter data down to sub-group of interest
  if(!is.null(.group)){
    if(is.character(.group)) {
      # Parse string expression and evaluate it
      dat <- dat |>
        dplyr::filter(!!rlang::parse_expr(.group))
    } else {
      # Fallback for other input types
      dat <- dat |>
        dplyr::filter(!!.group)
    }
  }

  # # Base selection - columns that are always needed
  # dat_indices <- dat |>
  #   dplyr::select(dplyr::all_of(.assess), grade, grade_grp, NCES_PUBLIC_PCTFRLUNCH_ROUNDED,
  #                 GENDER_MALE, GENDER_FEMALE,ETHNICITY_White, ETHNICITY_Hispanic,
  #                 ETHNICITY_Black, ETHNICITY_Other,URBAN_LOC_21) |>
  #   # dplyr::mutate(
  #   #   GRADE = factor(
  #   #     dplyr::if_else(GRADE == 13, "K", as.character(GRADE)),
  #   #     levels = c("K", as.character(1:8))
  #   #   )) |>
  #   dplyr::mutate(GRADE = as.numeric(as.character(GRADE))) |>
  #   dplyr::mutate(GRADE = dplyr::if_else(GRADE == 13, 0, GRADE)) |>
  #   # dplyr::mutate(GENDER_MALE = factor(GENDER_MALE, levels = c(0, 1)),
  #   #               GENDER_FEMALE = factor(GENDER_FEMALE, levels = c(0, 1)),
  #   #               ETHNICITY_White = factor(ETHNICITY_White, levels = c(0, 1)),
  #   #               ETHNICITY_Hispanic = factor(ETHNICITY_Hispanic, levels = c(0, 1)),
  #   #               ETHNICITY_Black = factor(ETHNICITY_Black, levels = c(0, 1)),
  #   #               ETHNICITY_Other = factor(ETHNICITY_Other, levels = c(0, 1)),
  #   #               URBAN_LOC_21 = factor(URBAN_LOC_21, levels = c(0, 1)),
  #   #               treat = factor(treat, levels = c(0, 1), labels = c("Control", "Treatment"))) |>
  #   dplyr::mutate(treat = factor(treat, levels = c(0, 1), labels = c("Control", "Treatment")))
  #
  #
  # return(dat_indices)

    return(dat)
}


run_analysis <- function(
    .dat,
    .mod = list("full"),
    .rhs = list("waggle_math_assignments_completed_sytd + waggle_math_session_count_sytd + waggle_math_total_time_sytd_minutes + grade"),
    .terms = list("waggle_math_assignments_completed_sytd"),
    .setting = "MM"
    ){

  cat(paste("\nDescriptive Statistics:", "\n-------------------------\n"))
  tableone::CreateTableOne(vars = c("outcome",
                                    "mapm_s_assess_score",
                                    "mapm_f_assess_score",
                                    "waggle_math_assignments_completed_sytd",
                                    "waggle_math_session_count_sytd",
                                    "waggle_math_total_time_sytd_minutes"),
                           data = .dat) |> print()

  tableone::CreateTableOne(vars = c("outcome",
                                    "mapm_s_assess_score",
                                    "mapm_f_assess_score",
                                    "waggle_math_assignments_completed_sytd",
                                    "waggle_math_session_count_sytd",
                                    "waggle_math_total_time_sytd_minutes"),
                           data = .dat) |> print(nonnormal = TRUE)


  cat(paste("\nExploratory Plots:", "\n-------------------------\n"))
  p1 <- ggplot2::ggplot(.dat) +
    ggplot2::geom_jitter(ggplot2::aes(x = grade, y = outcome), alpha = 0.1, height = 0) +
    ggplot2::geom_boxplot(ggplot2::aes(x = grade, y = outcome), outliers = FALSE) +
    ggplot2::labs(title = "Outcome by Grade") +
    ggplot2::xlab("Grade") +
    ggplot2::ylab("Outcome") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position="none")
  print(p1)

  p2 <- ggplot2::ggplot(.dat) +
    ggplot2::geom_smooth(ggplot2::aes(x = waggle_math_assignments_completed_sytd, y = outcome)) +
    ggplot2::labs(title = "Outcome") +
    ggplot2::xlab("Assignments Completed") +
    ggplot2::ylab("Outcome") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position="none")
  print(p2)

  p3 <- ggplot2::ggplot(.dat) +
    ggplot2::geom_smooth(ggplot2::aes(x = waggle_math_session_count_sytd, y = outcome)) +
    ggplot2::labs(title = "Outcome") +
    ggplot2::xlab("Session Count") +
    ggplot2::ylab("Outcome") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position="none")
  print(p3)

  p4 <- ggplot2::ggplot(.dat) +
    ggplot2::geom_smooth(ggplot2::aes(x = waggle_math_total_time_sytd_minutes, y = outcome)) +
    ggplot2::labs(title = "Outcome") +
    ggplot2::xlab("Total Time") +
    ggplot2::ylab("Outcome") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position="none")
  print(p4)

  cat(paste("\nCorrelations:", "\n-------------------------\n"))
  .cor <- stats::cor(.dat |> dplyr::select(outcome,
                            waggle_math_assignments_completed_sytd,
                            waggle_math_session_count_sytd,
                            waggle_math_total_time_sytd_minutes) |>
                dplyr::rename(out = outcome,
                              assignments = waggle_math_assignments_completed_sytd,
                              sessions = waggle_math_session_count_sytd,
                              time = waggle_math_total_time_sytd_minutes))#|> print()

  # corrplot::corrplot.mixed(.cor, method = 'number') |> print()
  corrplot::corrplot.mixed(.cor) |> print()

  # Build model fomula
  .formula <- "outcome ~ "
  # results <- vector(mode = "list", length = length(.mod))
  for (i in 1:length(.mod)){
    .formula <- paste0(.formula, .rhs[i])
    print(.formula)
    tryCatch({
      mod <- stats::lm(stats::as.formula(.formula), data = .dat)
    }, error = function(e) {
      warning(paste("Error in model:", .mod, e$message))
      return(NULL)
    })

    cat(paste("\n-----Results for: ", .mod[[i]], "-----\n"))
    cat(paste("\nDiagnostic Plots\n: "))
    plot(mod)

    cat(paste("\nVariance Inflation Factos\n: "))
    print(car::vif(mod))


    cat(paste("\nParameter Estimates\n: "))
    .sum <- summary(mod)
    print(.sum)
    print(broom.mixed::tidy(mod, conf.int = TRUE))

    cat(paste("\nPlot Effects\n: "))
    predictions <- ggeffects::ggpredict(mod, terms = .terms[[i]])
    p <- plot(predictions) +
      ggplot2::theme_minimal()
    print(p)

    cat(paste("\nEstimated Marginal Means\n: "))
    print(emmeans::emmeans(mod, specs = stats::as.formula(paste0("~ ", .rhs[i]))))
    # print(emmeans::emmeans(mod, specs = as.formula(paste0("~ ", gsub(" + grade", "", .rhs[i], fixed = TRUE)))))

    cat(paste("\nStandardized Parameters\n: "))
    tryCatch({
      spmod <- parameters::standardize_parameters(mod,
                                                  method = "posthoc")
      print(spmod)
    }, error = function(e) {
      cat("Error calculating standardized effect size:", e$message, "\n")
    })

    cat(paste("\nRobust Parameter Estimates\n: "))

      tryCatch({
        print(.formula)
        modrob <- robustbase::lmrob(stats::as.formula(.formula), data = .dat, method= .setting)
        .sumrob <- summary(modrob)
        print(.sumrob)
      }, error = function(e) {
        warning(paste("Error in model:", .mod, e$message))
        return(NULL)
      })
  }

}


run_analysis_mixed <- function(
    .dat,
    .mod = list("full"),
    .rhs = list("waggle_math_assignments_completed_sytd + waggle_math_session_count_sytd + waggle_math_total_time_sytd_minutes + grade + (1|school_pid)"),
    .terms = list("waggle_math_assignments_completed_sytd")
){

  cat(paste("\nDescriptive Statistics:", "\n-------------------------\n"))
  tableone::CreateTableOne(vars = c("outcome",
                                    "mapm_f_projected_spring_rit",
                                    "mapm_f_assess_score",
                                    "waggle_math_assignments_completed_sytd",
                                    "waggle_math_session_count_sytd",
                                    "waggle_math_total_time_sytd_minutes"),
                           data = .dat) |> print()

  tableone::CreateTableOne(vars = c("outcome",
                                    "mapm_f_projected_spring_rit",
                                    "mapm_f_assess_score",
                                    "waggle_math_assignments_completed_sytd",
                                    "waggle_math_session_count_sytd",
                                    "waggle_math_total_time_sytd_minutes"),
                           data = .dat) |> print(nonnormal = TRUE)


  cat(paste("\nExploratory Plots:", "\n-------------------------\n"))
  p1 <- ggplot2::ggplot(.dat) +
    ggplot2::geom_jitter(ggplot2::aes(x = grade, y = outcome), alpha = 0.1, height = 0) +
    ggplot2::geom_boxplot(ggplot2::aes(x = grade, y = outcome), outliers = FALSE) +
    ggplot2::labs(title = "Outcome by Grade") +
    ggplot2::xlab("Grade") +
    ggplot2::ylab("Outcome") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position="none")
  print(p1)

  p2 <- ggplot2::ggplot(.dat) +
    ggplot2::geom_smooth(ggplot2::aes(x = waggle_math_assignments_completed_sytd, y = outcome)) +
    ggplot2::labs(title = "Outcome") +
    ggplot2::xlab("Assignments Completed") +
    ggplot2::ylab("Outcome") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position="none")
  print(p2)

  p3 <- ggplot2::ggplot(.dat) +
    ggplot2::geom_smooth(ggplot2::aes(x = waggle_math_session_count_sytd, y = outcome)) +
    ggplot2::labs(title = "Outcome") +
    ggplot2::xlab("Session Count") +
    ggplot2::ylab("Outcome") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position="none")
  print(p3)

  p4 <- ggplot2::ggplot(.dat) +
    ggplot2::geom_smooth(ggplot2::aes(x = waggle_math_total_time_sytd_minutes, y = outcome)) +
    ggplot2::labs(title = "Outcome") +
    ggplot2::xlab("Session Count") +
    ggplot2::ylab("Outcome") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position="none")
  print(p4)

  cat(paste("\nCorrelations:", "\n-------------------------\n"))
  stats::cor(.dat |> dplyr::select(outcome,
                            waggle_math_assignments_completed_sytd,
                            waggle_math_session_count_sytd,
                            waggle_math_total_time_sytd_minutes))|> print()

  # Build model fomula
  .formula <- "outcome ~ "
  # results <- vector(mode = "list", length = length(.mod))
  for (i in 1:length(.mod)){
    .formula <- paste0(.formula, .rhs[i])
    print(.formula)
    tryCatch({
      mod <- lmerTest::lmer(stats::as.formula(.formula), data = .dat)
    }, error = function(e) {
      warning(paste("Error in model:", .mod, e$message))
      return(NULL)
    })

    cat(paste("\nResults for: ", .mod[[i]]))
    cat(paste("\nDiagnostic Plots\n: "))
    plot(mod)

    cat(paste("\nParameter Estimates\n: "))
    .sum <- summary(mod)
    print(.sum)
    print(broom.mixed::tidy(mod, conf.int = TRUE))

    cat(paste("\nPlot Effects\n: "))
    predictions <- ggeffects::ggpredict(mod, terms = .terms[[i]])
    p <- plot(predictions) +
      ggplot2::theme_minimal()
    print(p)

    cat(paste("\nEstimated Marginal Means\n: "))
    # print(emmeans::emmeans(mod, specs = as.formula(paste0("~ ", .rhs[i]))))
    print(emmeans::emmeans(mod, specs = stats::as.formula(paste0("~ ", gsub("+ (1|school_pid)", "", .rhs[i], fixed = TRUE)))))

    cat(paste("\nStandardized Parameters\n: "))
    tryCatch({
      spmod <- parameters::standardize_parameters(mod,
                                                  method = "posthoc")
      print(spmod)
    }, error = function(e) {
      cat("Error calculating standardized effect size:", e$message, "\n")
    })


  }

}







