// SLEEP AND MENTAL HEALTH MODEL -----------------------------------------------

functions {
  real outcome_income_lpdf(real y, real mu, real sigma, int income,
                           vector beta_income, vector pi_income) {
    if (income > 0) {
      real effect = income == 1 ? 0 : beta_income[income - 1];
      return normal_lpdf(y | mu + effect, sigma);
    } else {
      vector[num_elements(pi_income)] lps;
      for (k in 1:num_elements(pi_income)) {
        real effect = k == 1 ? 0 : beta_income[k - 1];
        lps[k] = log(pi_income[k]) + normal_lpdf(y | mu + effect, sigma);
      }
      return log_sum_exp(lps);
    }
  }
}

data {
  int<lower=0> N_obs;
  int<lower=0> N_ps;
  int<lower=1> N_participants;

  array[N_obs] int<lower=1, upper=N_ps> ps_id;
  vector[N_obs] sleep_duration;
  vector[N_obs] sleep_midpoint;

  vector[N_ps] outcome;
  array[N_ps] int<lower=1, upper=N_participants> participant_id_ps;
  vector[N_ps] age_time_c;

  int<lower=1> K_race;
  array[N_ps] int<lower=0, upper=K_race> race_code;
  simplex[K_race] pi_race;

  int<lower=1> K_ethnicity;
  array[N_ps] int<lower=0, upper=K_ethnicity> ethnicity_code;
  simplex[K_ethnicity] pi_ethnicity;

  int<lower=1> K_income;
  array[N_ps] int<lower=0, upper=K_income> income_code;
  simplex[K_income] pi_income;
}

transformed data {
  array[N_participants] int counts = rep_array(0, N_participants);
  array[N_participants] int used = rep_array(0, N_participants);
  array[N_participants + 1] int offsets;
  array[N_ps] int session_order;
  array[N_participants] int race_person = rep_array(0, N_participants);
  array[N_participants] int ethnicity_person = rep_array(0, N_participants);

  for (i in 1:N_ps) {
    int p = participant_id_ps[i];
    counts[p] += 1;
    if (race_code[i] > 0) {
      if (race_person[p] > 0 && race_person[p] != race_code[i])
        reject("Conflicting race categories within participant");
      race_person[p] = race_code[i];
    }
    if (ethnicity_code[i] > 0) {
      if (ethnicity_person[p] > 0 && ethnicity_person[p] != ethnicity_code[i])
        reject("Conflicting ethnicity categories within participant");
      ethnicity_person[p] = ethnicity_code[i];
    }
  }

  offsets[1] = 1;
  for (p in 1:N_participants) {
    if (counts[p] == 0) reject("Participant has no sessions");
    offsets[p + 1] = offsets[p] + counts[p];
  }

  for (i in 1:N_ps) {
    int p = participant_id_ps[i];
    session_order[offsets[p] + used[p]] = i;
    used[p] += 1;
  }
}

parameters {
  // Duration mean/log-variance, then midpoint mean/log-variance
  vector[4] gamma_sleep_mean;
  vector[4] gamma_sleep_age;

  array[N_ps] vector[4] eta_raw;

  cholesky_factor_corr[4] L_Omega;
  vector<lower=0>[4] tau;

  real beta0;
  vector[4] beta_sleep_between;
  vector[4] beta_sleep_within;

  vector[K_race - 1] beta_race;
  vector[K_ethnicity - 1] beta_ethnicity;
  vector[K_income - 1] beta_income;

  vector[N_participants] u_participant_raw;
  real<lower=0> sigma_participant;
  real<lower=0> sigma_outcome;
}

transformed parameters {
  array[N_ps] vector[4] eta;
  array[N_ps] vector[4] eta_detrended;

  vector[N_ps] dur_mean;
  vector[N_ps] dur_logvar;
  vector[N_ps] mid_mean;
  vector[N_ps] mid_logvar;

  vector[N_obs] dur_mean_obs;
  vector[N_obs] dur_sd_obs;
  vector[N_obs] mid_mean_obs;
  vector[N_obs] mid_sd_obs;

  vector[N_participants] u_participant;

  vector[N_participants] n_ps_per_participant;
  vector[N_participants] dur_mean_bp_participant;
  vector[N_participants] dur_logvar_bp_participant;
  vector[N_participants] mid_mean_bp_participant;
  vector[N_participants] mid_logvar_bp_participant;

  vector[N_ps] dur_mean_bp_ps;
  vector[N_ps] dur_logvar_bp_ps;
  vector[N_ps] mid_mean_bp_ps;
  vector[N_ps] mid_logvar_bp_ps;

  vector[N_ps] dur_mean_wp;
  vector[N_ps] dur_logvar_wp;
  vector[N_ps] mid_mean_wp;
  vector[N_ps] mid_logvar_wp;

  vector[N_ps] mu_base;

  {
    matrix[4, 4] Sigma_chol = diag_pre_multiply(tau, L_Omega);

    for (i in 1:N_ps) {
      eta[i] = gamma_sleep_mean + gamma_sleep_age * age_time_c[i]
               + Sigma_chol * eta_raw[i];
      eta_detrended[i] = eta[i] - gamma_sleep_age * age_time_c[i];
    }
  }

  for (i in 1:N_ps) {
    dur_mean[i] = eta[i][1];
    dur_logvar[i] = eta[i][2];
    mid_mean[i] = eta[i][3];
    mid_logvar[i] = eta[i][4];
  }

  u_participant = sigma_participant * u_participant_raw;

  dur_mean_obs = dur_mean[ps_id];
  dur_sd_obs = sqrt(exp(dur_logvar[ps_id]));
  mid_mean_obs = mid_mean[ps_id];
  mid_sd_obs = sqrt(exp(mid_logvar[ps_id]));

  n_ps_per_participant = rep_vector(0, N_participants);
  dur_mean_bp_participant = rep_vector(0, N_participants);
  dur_logvar_bp_participant = rep_vector(0, N_participants);
  mid_mean_bp_participant = rep_vector(0, N_participants);
  mid_logvar_bp_participant = rep_vector(0, N_participants);

  for (i in 1:N_ps) {
    int pid = participant_id_ps[i];
    n_ps_per_participant[pid] += 1;
    dur_mean_bp_participant[pid] += eta_detrended[i][1];
    dur_logvar_bp_participant[pid] += eta_detrended[i][2];
    mid_mean_bp_participant[pid] += eta_detrended[i][3];
    mid_logvar_bp_participant[pid] += eta_detrended[i][4];
  }

  // Participant means give equal weight to each age-detrended session
  for (p in 1:N_participants) {
    dur_mean_bp_participant[p] /= n_ps_per_participant[p];
    dur_logvar_bp_participant[p] /= n_ps_per_participant[p];
    mid_mean_bp_participant[p] /= n_ps_per_participant[p];
    mid_logvar_bp_participant[p] /= n_ps_per_participant[p];
  }

  for (i in 1:N_ps) {
    int pid = participant_id_ps[i];
    dur_mean_bp_ps[i] = dur_mean_bp_participant[pid];
    dur_logvar_bp_ps[i] = dur_logvar_bp_participant[pid];
    mid_mean_bp_ps[i] = mid_mean_bp_participant[pid];
    mid_logvar_bp_ps[i] = mid_logvar_bp_participant[pid];

    dur_mean_wp[i] = eta_detrended[i][1] - dur_mean_bp_ps[i];
    dur_logvar_wp[i] = eta_detrended[i][2] - dur_logvar_bp_ps[i];
    mid_mean_wp[i] = eta_detrended[i][3] - mid_mean_bp_ps[i];
    mid_logvar_wp[i] = eta_detrended[i][4] - mid_logvar_bp_ps[i];
  }

  mu_base = beta0
            + beta_sleep_between[1] * dur_mean_bp_ps
            + beta_sleep_between[2] * dur_logvar_bp_ps
            + beta_sleep_between[3] * mid_mean_bp_ps
            + beta_sleep_between[4] * mid_logvar_bp_ps
            + beta_sleep_within[1] * dur_mean_wp
            + beta_sleep_within[2] * dur_logvar_wp
            + beta_sleep_within[3] * mid_mean_wp
            + beta_sleep_within[4] * mid_logvar_wp;
}

model {
  gamma_sleep_mean[1] ~ normal(8, 1.5);
  gamma_sleep_mean[2] ~ normal(0, 1);
  gamma_sleep_mean[3] ~ normal(3, 2);
  gamma_sleep_mean[4] ~ normal(0, 1.5);

  gamma_sleep_age[1] ~ normal(0, 0.5);
  gamma_sleep_age[2] ~ normal(0, 0.35);
  gamma_sleep_age[3] ~ normal(0, 0.75);
  gamma_sleep_age[4] ~ normal(0, 0.35);

  L_Omega ~ lkj_corr_cholesky(2);
  tau[1] ~ normal(0, 1.5);
  tau[2] ~ normal(0, 1.5);
  tau[3] ~ normal(0, 2);
  tau[4] ~ normal(0, 2);

  for (i in 1:N_ps)
    eta_raw[i] ~ std_normal();

  beta0 ~ normal(50, 20);
  beta_sleep_between ~ normal(0, 5);
  beta_sleep_within ~ normal(0, 5);
  beta_race ~ normal(0, 5);
  beta_ethnicity ~ normal(0, 5);
  beta_income ~ normal(0, 5);
  u_participant_raw ~ std_normal();
  sigma_participant ~ exponential(1);
  sigma_outcome ~ exponential(0.1);

  sleep_duration ~ normal(dur_mean_obs, dur_sd_obs);
  sleep_midpoint ~ normal(mid_mean_obs, mid_sd_obs);

  // Marginalize missing race and ethnicity across each person's sessions
  // Missing income is marginalized separately within each session
  for (p in 1:N_participants) {
    int n_race = race_person[p] == 0 ? K_race : 1;
    int n_ethnicity = ethnicity_person[p] == 0 ? K_ethnicity : 1;
    vector[n_race * n_ethnicity] lps;
    int candidate = 0;

    for (r_index in 1:n_race) {
      int r = race_person[p] == 0 ? r_index : race_person[p];
      real race_effect = r == 1 ? 0 : beta_race[r - 1];

      for (e_index in 1:n_ethnicity) {
        int e = ethnicity_person[p] == 0 ? e_index : ethnicity_person[p];
        real ethnicity_effect = e == 1 ? 0 : beta_ethnicity[e - 1];

        candidate += 1;
        lps[candidate] =
          (race_person[p] == 0 ? log(pi_race[r]) : 0)
          + (ethnicity_person[p] == 0 ? log(pi_ethnicity[e]) : 0);

        for (position in offsets[p]:(offsets[p + 1] - 1)) {
          int i = session_order[position];
          lps[candidate] += outcome_income_lpdf(
            outcome[i] | mu_base[i] + u_participant[p]
                         + race_effect + ethnicity_effect,
            sigma_outcome, income_code[i], beta_income, pi_income
          );
        }
      }
    }

    target += log_sum_exp(lps);
  }
}
