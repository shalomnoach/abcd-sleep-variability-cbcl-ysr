# Model Diagnostics ------------------------------------------------------------

## Parameter Summaries ---------------------------------------------------------

covariate_labels <- function() {
  tibble::tibble(
    variable = c(paste0("beta_race[", 1:4, "]"), "beta_ethnicity[1]",
                 paste0("beta_income[", 1:4, "]")),
    covariate = c(rep("Race", 4), "Ethnicity", rep("Household income", 4)),
    category = c("Black (African American)", "Asian", "More than One Race",
                 "American Indian/Alaska Native, Native Hawaiian/Pacific Islander, or Other",
                 "Hispanic", "$50K-$100K", "< $50K", "Don't know", "Decline to answer"),
    reference = c(rep("White", 4), "Non-Hispanic", rep("> $100K", 4)),
    units = "Adjusted outcome T-score difference from reference"
  )
}

summarise_parameters <- function(draws) {
  parameters <- posterior::variables(draws)[!grepl("__$", posterior::variables(draws))]
  posterior::summarise_draws(
    posterior::subset_draws(draws, variable = parameters),
    "mean", "median", "sd", "mcse_mean", "mcse_sd", "rhat", "ess_bulk", "ess_tail",
    ~ posterior::quantile2(.x, probs = c(.025, .05, .5, .95, .975))
  )
}

## Trace And Rank Plots --------------------------------------------------------

save_diagnostic_plots <- function(draws, outcome_name, directory) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)

  variables <- posterior::variables(draws)
  parameters <- variables[!grepl("__$", variables)]
  varying <- vapply(parameters, function(variable) {
    diff(range(as.numeric(draws[, , variable, drop = FALSE]))) > 0
  }, logical(1))
  groups <- list(
    sleep_effects = grep("^beta_sleep_", parameters, value = TRUE),
    covariates = grep("^beta_(race|ethnicity|income)\\[", parameters, value = TRUE),
    sleep_distribution = grep("^(gamma_sleep_|tau\\[)", parameters, value = TRUE),
    outcome_scales = intersect(c("beta0", "sigma_participant", "sigma_outcome"), parameters),
    sleep_cholesky = grep("^L_Omega\\[", parameters, value = TRUE)
  )
  group_titles <- c(
    sleep_effects = "Between/within sleep coefficients",
    covariates = "Covariate coefficients",
    sleep_distribution = "Sleep population means, age slopes and SDs",
    outcome_scales = "Outcome intercept and SDs",
    sleep_cholesky = "Sleep correlation Cholesky parameters"
  )
  bayesplot::color_scheme_set(c("#0072B2", "#E69F00", "#009E73", "#D55E00",
                                "#CC79A7", "#000000"))
  subtitle <- paste(posterior::nchains(draws), "chains;",
                    posterior::niterations(draws), "post-warmup draws per chain")

  save_pdf <- function(type) {
    grDevices::pdf(file.path(directory, paste0(type, ".pdf")),
                  width = 12, height = 14, onefile = TRUE)
    on.exit(grDevices::dev.off())
    for (group in names(groups)) {
      pars <- intersect(groups[[group]], parameters[varying])
      if (!length(pars)) next
      x <- posterior::subset_draws(draws, variable = pars)

      plot <- if (type == "trace") {
        bayesplot::mcmc_trace(x, facet_args = list(ncol = 2))
      } else {
        bayesplot::mcmc_rank_overlay(x, ref_line = TRUE, facet_args = list(ncol = 2))
      }
      print(plot + ggplot2::labs(
        title = paste(outcome_name, "-", group_titles[[group]]), subtitle = subtitle
      ) + ggplot2::theme(
        plot.title = ggplot2::element_text(size = 13),
        plot.subtitle = ggplot2::element_text(size = 10)
      ))
    }
  }
  save_pdf("trace")
  save_pdf("trace_rank")
}

## Parameter Tables ------------------------------------------------------------

parameter_table <- function(data) {
  sleep_labels <- c(
    "Mean duration (hours)", "Duration log variance",
    "Mean midpoint (hours)", "Midpoint log variance"
  )
  labels <- bind_rows(
    tibble(
      variable = c(
        paste0("gamma_sleep_mean[", 1:4, "]"),
        paste0("gamma_sleep_age[", 1:4, "]"),
        paste0("tau[", 1:4, "]"),
        paste0("beta_sleep_between[", 1:4, "]"),
        paste0("beta_sleep_within[", 1:4, "]")
      ),
      group = rep(c("Sleep population mean", "Sleep age slope (per year)",
                    "Sleep residual SD", "Between-person coefficient",
                    "Within-person coefficient"), each = 4),
      parameter = rep(sleep_labels, 5)
    ),
    covariate_labels() |>
      transmute(variable, group = paste(covariate, "coefficient"),
                parameter = paste(category, "vs", reference)),
    tibble(
      variable = c("beta0", "sigma_participant", "sigma_outcome"),
      group = "Outcome distribution",
      parameter = c("Intercept (T-score)", "Participant intercept SD (T-score)",
                    "Residual SD (T-score)")
    ),
    tidyr::expand_grid(row = 1:4, column = 1:4) |>
      transmute(
        variable = paste0("L_Omega[", row, ",", column, "]"),
        group = "Sleep correlation Cholesky factor",
        parameter = paste(sleep_labels[row], sleep_labels[column], sep = " / ")
      )
  )
  data |>
    left_join(labels, by = "variable", relationship = "many-to-one") |>
    transmute(
      Outcome = outcome, Group = group, Parameter = parameter,
      Mean = round(mean, 3), Median = round(median, 3),
      `Posterior SD` = round(sd, 3),
      `95% CrI lower (equal-tailed)` = round(q2.5, 3),
      `95% CrI upper (equal-tailed)` = round(q97.5, 3),
      across(any_of(c("hdi_lower_95", "hdi_upper_95")), ~ round(.x, 3)),
      `R-hat` = round(rhat, 3), `Bulk ESS` = round(ess_bulk),
      `Tail ESS` = round(ess_tail), `MCSE (mean)` = round(mcse_mean, 4),
      `MCSE (SD)` = round(mcse_sd, 4)
    ) |>
    rename_with(~ c(hdi_lower_95 = "95% HDI lower",
                    hdi_upper_95 = "95% HDI upper")[.x],
                any_of(c("hdi_lower_95", "hdi_upper_95")))
}
