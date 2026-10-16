# Posterior Predictive Checks --------------------------------------------------

## Conditional Replications ----------------------------------------------------

decomp_ppc <- function(csv_files, d, n_draws = 100L, seed = 12345L, shared_static = TRUE) {
  configuration <- stan_csv_configuration(csv_files[1])
  n <- ceiling(as.integer(configuration$num_samples) / as.integer(configuration$thin))
  per_chain <- min(n, max(1L, ceiling(n_draws / length(csv_files))))
  selected <- unique(as.integer(round(seq(1L, n, length.out = per_chain))))
  variables <- c("mu_base", "u_participant", "sigma_outcome",
                 "beta_race", "beta_ethnicity", "beta_income",
                 "dur_mean", "dur_logvar", "mid_mean", "mid_logvar")
  draws <- posterior::as_draws_matrix(read_stan_draws(csv_files, variables, selected))
  set.seed(seed)
  get <- function(draw, name, count) {
    values <- unname(draw[paste0(name, "[", seq_len(count), "]")])
    if (any(!is.finite(values))) stop("Missing or invalid PPC parameter: ", name)
    values
  }
  n_nights <- tabulate(d$ps_id, d$N_ps)
  sd_eligible <- n_nights >= 2L
  session_sd <- function(x) {
    sums <- as.numeric(rowsum(x, d$ps_id))
    squares <- as.numeric(rowsum(x * x, d$ps_id))

    result <- rep(NA_real_, d$N_ps)
    result[sd_eligible] <- sqrt(pmax(0,
      (squares[sd_eligible] - sums[sd_eligible]^2 / n_nights[sd_eligible]) /
        (n_nights[sd_eligible] - 1L)))
    result
  }
  mean_session_sd <- function(x) {
    if (!any(sd_eligible)) return(NA_real_)
    mean(session_sd(x)[sd_eligible])
  }
  groups <- split(seq_len(d$N_ps), d$participant_id_ps)
  repeated <- groups[lengths(groups) > 1L]
  first <- vapply(repeated, function(x) x[1], integer(1))
  last <- vapply(repeated, function(x) x[length(x)], integer(1))
  repeated_correlation <- function(x) {
    if (length(first) < 3L || sd(x[first]) == 0 || sd(x[last]) == 0) return(NA_real_)
    cor(x[first], x[last])
  }
  metrics <- function(y, duration, midpoint) c(
    outcome_mean = mean(y), outcome_sd = sd(y),
    outcome_q10 = unname(quantile(y, .1)), outcome_q90 = unname(quantile(y, .9)),
    outcome_repeat_correlation = repeated_correlation(y),
    duration_mean = mean(duration), duration_mean_session_sd = mean_session_sd(duration),
    midpoint_mean = mean(midpoint), midpoint_mean_session_sd = mean_session_sd(midpoint)
  )
  observed <- metrics(d$outcome, d$sleep_duration, d$sleep_midpoint)
  replicated <- matrix(NA_real_, nrow(draws), length(observed), dimnames = list(NULL, names(observed)))
  for (s in seq_len(nrow(draws))) {
    draw <- setNames(as.numeric(draws[s, ]), colnames(draws))
    sigma <- unname(draw["sigma_outcome"])
    mu <- get(draw, "mu_base", d$N_ps) +
      get(draw, "u_participant", d$N_participants)[d$participant_id_ps]
    category_effects <- lapply(c("race", "ethnicity", "income"), function(name) {
      c(0, get(draw, paste0("beta_", name), d[[paste0("K_", name)]] - 1L))
    })
    codes <- cbind(d$race_code, d$ethnicity_code, d$income_code)
    probabilities <- list(d$pi_race, d$pi_ethnicity, d$pi_income)

    static_groups <- if (shared_static) groups else as.list(seq_len(d$N_ps))
    for (rows in static_groups) {
      for (k in 1:2) {
        known <- unique(codes[rows, k][codes[rows, k] > 0])
        if (length(known) > 1L) stop("Conflicting known static categories in PPC input.")
        if (length(known)) codes[rows, k] <- known
      }
    }
    complete_static <- rowSums(codes[, 1:2, drop = FALSE] == 0L) == 0L
    for (k in 1:2) mu[complete_static] <- mu[complete_static] + category_effects[[k]][codes[complete_static, k]]
    for (rows in static_groups) {
      if (all(complete_static[rows])) next
      categories <- lapply(1:2, function(k) if (codes[rows[1], k] == 0L) seq_along(category_effects[[k]]) else codes[rows[1], k])
      grid <- expand.grid(categories)
      log_weight <- rep(0, nrow(grid))
      static_effect <- rep(0, nrow(grid))
      for (k in 1:2) {
        static_effect <- static_effect + category_effects[[k]][grid[[k]]]
        if (codes[rows[1], k] == 0L) log_weight <- log_weight + log(probabilities[[k]][grid[[k]]])
      }
      for (candidate in seq_len(nrow(grid))) {
        for (i in rows) {
          means <- mu[i] + static_effect[candidate]
          if (codes[i, 3] > 0L) {
            log_weight[candidate] <- log_weight[candidate] +
              dnorm(d$outcome[i], means + category_effects[[3]][codes[i, 3]], sigma, log = TRUE)
          } else {
            lp <- log(probabilities[[3]]) + dnorm(d$outcome[i], means + category_effects[[3]], sigma, log = TRUE)
            maximum <- max(lp)
            log_weight[candidate] <- log_weight[candidate] + maximum + log(sum(exp(lp - maximum)))
          }
        }
      }
      weight <- exp(log_weight - max(log_weight))
      if (!all(is.finite(weight)) || sum(weight) <= 0) stop("Invalid static-category PPC weights.")
      mu[rows] <- mu[rows] + static_effect[sample.int(nrow(grid), 1L, prob = weight)]
    }
    income_known <- codes[, 3] > 0L
    mu[income_known] <- mu[income_known] + category_effects[[3]][codes[income_known, 3]]
    for (i in which(!income_known)) {
      candidate <- mu[i] + category_effects[[3]]
      lp <- log(probabilities[[3]]) + dnorm(d$outcome[i], candidate, sigma, log = TRUE)
      weight <- exp(lp - max(lp))
      mu[i] <- candidate[sample.int(length(candidate), 1L, prob = weight)]
    }
    duration <- rnorm(d$N_obs, get(draw, "dur_mean", d$N_ps)[d$ps_id],
                      exp(get(draw, "dur_logvar", d$N_ps)[d$ps_id] / 2))
    midpoint <- rnorm(d$N_obs, get(draw, "mid_mean", d$N_ps)[d$ps_id],
                      exp(get(draw, "mid_logvar", d$N_ps)[d$ps_id] / 2))
    replicated[s, ] <- metrics(rnorm(d$N_ps, mu, sigma), duration, midpoint)
  }
  data.frame(
    statistic = names(observed), observed = unname(observed),
    replicated_mean = colMeans(replicated),
    replicated_q025 = apply(replicated, 2, quantile, .025, na.rm = TRUE),
    replicated_q975 = apply(replicated, 2, quantile, .975, na.rm = TRUE),
    posterior_predictive_p = colMeans(sweep(replicated, 2, observed, ">=")),
    posterior_draws = nrow(draws), chains = length(csv_files),
    participant_sessions = d$N_ps,
    sessions_with_at_least_two_nights = sum(sd_eligible),
    singleton_sessions = sum(n_nights == 1L), row.names = NULL
  )
}
