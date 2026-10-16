# Manuscript Tables ------------------------------------------------------------

## Formatting ------------------------------------------------------------------

table_text <- function(x) {
  paste0("#text(", encodeString(as.character(x), quote = '"'), ")")
}

table_number <- function(x, digits = 2, bounded = FALSE) {
  x <- round(as.numeric(x), digits)
  x[x == 0] <- 0
  value <- formatC(x, format = "f", digits = digits)
  if (bounded) {
    value <- sub("^(-?)0\\.", "\\1.", value)
  }
  sub("^-", "−", value)
}

table_p <- function(x) {
  ifelse(x < .001, "< .001", sub("^0", "", sprintf("%.3f", x)))
}

table_integer <- function(x) {
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
}

table_bold <- function(x, bold = FALSE) {
  ifelse(rep_len(bold, length(x)), paste0("#strong[", x, "]"), x)
}

table_interval <- function(low, high, bold = FALSE, bounded = FALSE) {
  interval <- paste0(
    "[", table_number(low, bounded = bounded), ", ",
    table_number(high, bounded = bounded), "]"
  )
  table_bold(table_text(interval), bold)
}

table_excludes_zero <- function(low, high) low > 0 | high < 0

table_cells <- function(x) paste0("[", x, "]", collapse = ", ")

table_group <- function(label, columns) {
  c(table_bold(label, TRUE), rep("", columns - 1L))
}

table_figure <- function(
  rows,
  headers,
  widths,
  caption,
  number,
  label,
  note = NULL,
  font_size = 10.5,
  header_rows = 1L,
  gutter = NULL,
  row_inset = 4,
  note_label_font_size = font_size,
  note_body_font_size = font_size
) {
  columns <- length(widths)
  stopifnot(all(lengths(rows) == columns), all(lengths(headers) == columns))
  last_row <- length(rows) + header_rows - 1L
  header_rule <- if (is.null(gutter)) {
    paste0("y == ", header_rows - 1L)
  } else {
    paste0("y == ", header_rows - 1L, " and x != 0 and x != ", gutter)
  }
  paste0(
    "#figure(kind: table, supplement: [Table], ",
    "numbering: (..nums) => ",
    encodeString(number, quote = '"'),
    ",\n",
    "caption: [",
    caption,
    "])[\n",
    "#set text(size: ",
    font_size,
    "pt, top-edge: 0.8em, bottom-edge: -0.2em)\n",
    "#set par(leading: 2pt, spacing: 0pt, first-line-indent: 0pt, justify: false)\n",
    "#table(\ncolumns: (",
    paste(widths, collapse = ", "),
    "),\n",
    "align: (left, ",
    paste(rep("center", columns - 1L), collapse = ", "),
    "),\n",
    "inset: (x: 3pt, y: ",
    row_inset,
    "pt),\n",
    "stroke: (x, y) => (top: if y == 0 { 0.5pt } else { none }, ",
    "bottom: if (",
    header_rule,
    ") or y == ",
    last_row,
    " { 0.5pt } else { none }),\n",
    "table.header(\n",
    paste(vapply(headers, table_cells, character(1)), collapse = ",\n"),
    "\n),\n",
    paste(vapply(rows, table_cells, character(1)), collapse = ",\n"),
    "\n)\n",
    if (!is.null(note)) paste0(
      "#v(5pt)\n#align(left)[#text(size: ", note_label_font_size,
      "pt)[_Note._] #text(size: ", note_body_font_size, "pt)[", note, "]]\n"
    ),
    "] <",
    label,
    ">\n"
  )
}

table_block <- function(content, landscape = FALSE, heading = NULL, heading_align = "center") {
  cat("\n```{=typst}\n")
  if (landscape) {
    cat("#set page(flipped: true)\n")
  } else {
    cat("#pagebreak(weak: true)\n")
  }
  cat(
    "#[\n#set par(leading: 2pt, spacing: 0pt, first-line-indent: 0pt, justify: false)\n"
  )
  cat("#show figure.where(kind: table): set figure.caption(position: top)\n")
  cat("#show figure.caption: set align(left)\n")
  cat(
    "#show figure.caption: set par(leading: 2pt, spacing: 0pt, first-line-indent: 0pt)\n"
  )
  cat("#set figure(gap: 8pt)\n")
  if (!is.null(heading)) {
    cat("#align(", heading_align, ")[#strong[", heading, "]]\n#v(12pt)\n", sep = "")
  }
  cat(content, "]\n", sep = "")
  if (landscape) {
    cat("#set page(flipped: false)\n")
  }
  cat("```\n\n")
}

## Demographics ----------------------------------------------------------------

render_demographics_table <- function(demographics_raw) {
  demographics <- demographics_raw[
    demographics_raw$unit == "participant" &
      demographics_raw$model_name == "total_problems_reference",
  ]
  sizes <- demographics |>
    dplyr::distinct(instrument, participants)
  n <- function(instrument) {
    format(
      sizes$participants[sizes$instrument == instrument],
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    )
  }
  row <- function(key, label, bold = FALSE) {
    entry <- demographics[demographics$table_row == key, ]
    entry <- entry[match(c("CBCL", "YSR"), entry$instrument), ]
    stopifnot(
      nrow(entry) == 2L,
      identical(as.character(entry$instrument), c("CBCL", "YSR"))
    )
    values <- if (all(entry$row_type == "continuous")) {
      paste0(table_number(entry$mean), " (", table_number(entry$sd), ")")
    } else {
      paste0(
        format(entry$n, big.mark = ",", scientific = FALSE, trim = TRUE),
        " (",
        table_number(100 * entry$proportion),
        ")"
      )
    }
    c(
      if (bold) {
        table_bold(table_text(label), TRUE)
      } else {
        paste0("#h(1em)", table_text(label))
      },
      table_text(values)
    )
  }
  rows <- list(row("Age (years)", "Age (years)", TRUE))
  groups <- list(
    Sex = c("Male", "Female"),
    Race = c(
      "White",
      "Black (African American)",
      "Asian",
      "American Indian/Alaska Native",
      "Native Hawaiian or Other Pacific Islander",
      "More than One Race",
      "Other (Unknown or not reported)"
    ),
    Ethnicity = c("Hispanic", "Non-Hispanic", "Missing"),
    `Family income` = c(
      "< $50K",
      "$50K-$100K",
      "> $100K",
      "Decline to answer",
      "Don't know"
    )
  )
  for (group in names(groups)) {
    rows <- append(rows, list(table_group(table_text(group), 3)))
    for (level in groups[[group]]) {
      key <- paste0(group, ": ", level)
      if (!key %in% demographics$table_row) {
        next
      }
      label <- switch(
        level,
        `Black (African American)` = "Black/African American",
        `Native Hawaiian or Other Pacific Islander` = "Native Hawaiian/Pacific Islander",
        `More than One Race` = "More than one race",
        level
      )
      rows <- append(rows, list(row(key, label)))
    }
  }
  headers <- list(c(
    "",
    paste0("*CBCL Subsample*\\ _n_ = ", n("CBCL")),
    paste0("*YSR Subsample*\\ _n_ = ", n("YSR"))
  ))
  note <- paste(
    "Values for age are _M_ (_SD_). All other values are _n_ (%).",
    "CBCL = Child Behavior Checklist; YSR = Youth Self-Report."
  )
  table_block(table_figure(
    rows,
    headers,
    c("45%", "30%", "25%"),
    "Sample Demographics",
    "1",
    "tbl-demographics",
    note,
    font_size = 12
  ))
}

## Correlations ----------------------------------------------------------------

render_correlation_table <- function(correlations, correlation_descriptives) {
  variables <- c(
    "sleep_duration_mean",
    "sleep_duration_isd",
    "midpoint_mean",
    "midpoint_isd",
    "cbcl_total",
    "cbcl_internalizing",
    "cbcl_externalizing",
    "ysr_total",
    "ysr_internalizing",
    "ysr_externalizing",
    "age"
  )
  labels <- c(
    "Sleep duration (#emph[iM])",
    "Sleep duration (#emph[iSD])",
    "Sleep midpoint (#emph[iM])",
    "Sleep midpoint (#emph[iSD])",
    "CBCL Total",
    "CBCL Internalizing",
    "CBCL Externalizing",
    "YSR Total",
    "YSR Internalizing",
    "YSR Externalizing",
    "Age"
  )
  descriptives <- correlation_descriptives[
    match(variables, correlation_descriptives$variable),
  ]
  stopifnot(identical(as.character(descriptives$variable), variables))
  rows <- lapply(seq_along(variables), function(i) {
    values <- rep("", length(variables))
    values[i] <- "—"
    if (i > 1L) {
      for (j in seq_len(i - 1L)) {
        pair <- correlations[
          (correlations$variable1 == variables[i] &
            correlations$variable2 == variables[j]) |
            (correlations$variable1 == variables[j] &
              correlations$variable2 == variables[i]),
        ]
        stopifnot(nrow(pair) == 1L)
        values[j] <- table_bold(
          table_text(table_number(pair$correlation, bounded = TRUE)),
          pair$p_value < .01
        )
      }
    }
    c(
      paste0(i, ". ", labels[i]),
      table_text(table_number(descriptives$mean[i])),
      table_text(table_number(descriptives$sd[i])),
      values[seq_len(length(variables) - 1L)]
    )
  })
  note <- paste(
    "Values are Pearson correlation coefficients.",
    "Sleep duration is measured in hours, sleep midpoint is measured in hours after midnight,",
    "and age is measured in years. _M_ = mean; _SD_ = standard deviation;",
    "#emph[iM] = intraindividual mean; #emph[iSD] = intraindividual standard deviation;",
    "CBCL = Child Behavior Checklist; YSR = Youth Self-Report.",
    "Bold indicates _p_ < .01."
  )
  headers <- list(c("*Variable*", "*_M_*", "*_SD_*", paste0("*", 1:10, ".*")))
  table_block(
    table_figure(
      rows,
      headers,
      c("25%", "6%", "6%", rep("6.3%", 10)),
      "Zero-Order Correlations Between Sleep, Psychopathology, and Age",
      "2",
      "tbl-correlations",
      note,
      font_size = 12
    ),
    landscape = TRUE
  )
}

## Between-Person Sleep Effects ------------------------------------------------

sleep_parameter_labels <- c(
  "Sleep Duration ($mu$)",
  "Sleep Duration ($ln sigma^2$)",
  "Sleep Midpoint ($mu$)",
  "Sleep Midpoint ($ln sigma^2$)"
)

render_sleep_effects_table <- function(parameter_summary, sample_sizes) {
  required_samples <- c("CBCL", "YSR")
  stopifnot(
    is.numeric(sample_sizes),
    !is.null(names(sample_sizes)),
    all(required_samples %in% names(sample_sizes)),
    all(is.finite(sample_sizes[required_samples]))
  )
  rows <- list()
  for (outcome in c("Externalizing", "Internalizing", "Total")) {
    rows <- append(rows, list(table_group(paste(outcome, "Problems"), 8)))
    for (j in 1:4) {
      cells <- paste0("#h(0.5em)", sleep_parameter_labels[j])
      for (instrument in c("CBCL", "YSR")) {
        entry <- parameter_summary[
          parameter_summary$instrument == instrument &
            parameter_summary$outcome ==
              paste(instrument, outcome, "Problems") &
            parameter_summary$variable == paste0("beta_sleep_between[", j, "]"),
        ]
        stopifnot(nrow(entry) == 1L)
        bold <- table_excludes_zero(entry$q2.5, entry$q97.5)
        cells <- c(
          cells,
          table_bold(table_text(table_number(c(entry$mean, entry$sd))), bold),
          table_interval(entry$q2.5, entry$q97.5, bold)
        )
        if (instrument == "CBCL") cells <- c(cells, "")
      }
      rows <- append(rows, list(cells))
    }
  }

  headers <- list(
    c("", "*CBCL*", "", "", "", "*YSR*", "", ""),
    c(
      "*Parameter*",
      "*Estimate*",
      "*_SD_*",
      "*95% CrI*",
      "",
      "*Estimate*",
      "*_SD_*",
      "*95% CrI*"
    )
  )
  note <- paste(
    "Estimates indicate the posterior mean of regression coefficients; _SD_ indicates the",
    "posterior standard deviation. Variability parameters are estimated on the log-variance scale.",
    "All coefficients shown here are the estimated between-person effects.",
    "CBCL = Child Behavior Checklist; YSR = Youth Self-Report; CrI = credible interval.",
    paste0(
      "CBCL _n_ = ", table_integer(sample_sizes[["CBCL"]]),
      "; YSR _n_ = ", table_integer(sample_sizes[["YSR"]]), "."
    ),
    "Bold indicates 95% credible intervals excluding zero."
  )
  content <- table_figure(
    rows,
    headers,
    c("24%", "11%", "9%", "16%", "4%", "11%", "9%", "16%"),
    "Sleep Parameters as Predictors of CBCL and YSR Scores",
    "3",
    "tbl-combined",
    note,
    font_size = 12,
    header_rows = 2L,
    gutter = 4L
  )
  content <- sub(
    "\\[\\], \\[\\*CBCL\\*\\], \\[\\], \\[\\], \\[\\], \\[\\*YSR\\*\\], \\[\\], \\[\\]",
    "[], table.cell(colspan: 3)[*CBCL*], [], table.cell(colspan: 3)[*YSR*]",
    content
  )
  table_block(content, landscape = TRUE)
}

## Parameter Estimates ---------------------------------------------------------

manuscript_parameter_groups <- function() {
  group <- function(title, variables, labels) {
    list(title = title, variables = variables, labels = labels)
  }
  indexed <- function(name, n = 4) paste0(name, "[", seq_len(n), "]")
  list(
    group(
      "Sleep Effects (Between-Person)",
      indexed("beta_sleep_between"),
      sleep_parameter_labels
    ),
    group(
      "Sleep Effects (Within-Person)",
      indexed("beta_sleep_within"),
      sleep_parameter_labels
    ),
    group(
      "Covariates",
      c(indexed("beta_race"), "beta_ethnicity[1]", indexed("beta_income")),
      table_text(c(
        "Race: Black/African American",
        "Race: Asian",
        "Race: More than one race",
        "Race: Other",
        "Ethnicity: Hispanic",
        "Income: $50K–$100K",
        "Income: < $50K",
        "Income: Don't know",
        "Income: Decline to answer"
      ))
    ),
    group(
      "Population Sleep Parameters",
      indexed("gamma_sleep_mean"),
      sleep_parameter_labels
    ),
    group(
      "Age Slopes (per Year)",
      indexed("gamma_sleep_age"),
      sleep_parameter_labels
    ),
    group(
      "Session-Level Latent SDs ($tau$)",
      indexed("tau"),
      sleep_parameter_labels
    ),
    group(
      "Outcome Intercept and SDs",
      c("beta0", "sigma_participant", "sigma_outcome"),
      c(
        "Intercept ($beta_0$)",
        "Participant SD ($sigma_(upright(p))$)",
        "Residual SD ($sigma_y$)"
      )
    ),
    group(
      "Correlation Structure (Free Cholesky Entries)",
      c(
        "L_Omega[2,1]",
        "L_Omega[3,1]",
        "L_Omega[3,2]",
        "L_Omega[4,1]",
        "L_Omega[4,2]",
        "L_Omega[4,3]"
      ),
      c(
        "$L_(Omega,21)$",
        "$L_(Omega,31)$",
        "$L_(Omega,32)$",
        "$L_(Omega,41)$",
        "$L_(Omega,42)$",
        "$L_(Omega,43)$"
      )
    )
  )
}

render_parameter_tables <- function(
  parameter_summary,
  heading = TRUE,
  table_prefix = "S",
  start_number = 2L
) {
  groups <- manuscript_parameter_groups()
  outcomes <- rep(c("Externalizing", "Internalizing", "Total"), 2)
  instruments <- rep(c("CBCL", "YSR"), each = 3)
  pages <- character()
  headers <- list(c(
    "*Parameter*",
    "*Estimate*",
    "*_SD_*",
    "*95% CrI*",
    "*95% HDI*",
    "$hat(R)$",
    "*ESS#sub[bulk]*",
    "*ESS#sub[tail]*"
  ))
  for (i in 1:6) {
    outcome <- paste(instruments[i], outcomes[i], "Problems")
    parameters <- parameter_summary[parameter_summary$outcome == outcome, ]
    number <- paste0(table_prefix, start_number + i - 1L)
    for (page in 1:2) {
      rows <- list()
      for (group in groups[if (page == 1L) 1:3 else 4:8]) {
        rows <- append(rows, list(table_group(group$title, 8)))
        for (j in seq_along(group$variables)) {
          entry <- parameters[parameters$variable == group$variables[j], ]
          stopifnot(nrow(entry) == 1L)
          bold_cri <- table_excludes_zero(entry$q2.5, entry$q97.5)
          bold_hdi <- table_excludes_zero(
            entry$hdi_lower_95,
            entry$hdi_upper_95
          )
          bounded <- startsWith(entry$variable, "L_Omega[")
          rows <- append(
            rows,
            list(c(
              paste0("#h(0.5em)", group$labels[j]),
              table_bold(
                table_text(c(
                  table_number(entry$mean, bounded = bounded),
                  table_number(entry$sd)
                )),
                bold_cri
              ),
              table_interval(entry$q2.5, entry$q97.5, bold_cri, bounded),
              table_interval(
                entry$hdi_lower_95,
                entry$hdi_upper_95,
                bold_hdi,
                bounded
              ),
              table_text(table_number(entry$rhat, 3)),
              table_text(format(
                round(c(entry$ess_bulk, entry$ess_tail)),
                big.mark = ",",
                scientific = FALSE,
                trim = TRUE
              ))
            ))
          )
        }
      }
      note <- if (page == 1L) {
        NULL
      } else {
        paste(
          "_SD_ = posterior standard deviation; CrI = credible interval; HDI = highest density",
          "interval; ESS = effective sample size. Reference categories for covariates are White,",
          "non-Hispanic, and income > \\$100K. Bold values indicate 95% credible intervals",
          "excluding zero."
        )
      }
      caption <- paste0(
        "Parameter Estimates: ",
        outcome,
        if (page == 2L) " (continued)" else ""
      )
      label <- paste0("tbl-", number, if (page == 2L) "-continued" else "")
      pages <- c(
        pages,
        table_figure(
          rows,
          headers,
          c("29%", "9%", "7%", "14%", "14%", "7%", "10%", "10%"),
          caption,
          number,
          label,
          note,
          font_size = 9,
          row_inset = 2,
          note_label_font_size = if (page == 2L && startsWith(number, "S2.")) 10 else 9,
          note_body_font_size = if (page == 2L && startsWith(number, "S2.")) 11 else 9
        )
      )
    }
  }
  table_block(
    paste(pages, collapse = "\n#pagebreak()\n"),
    landscape = TRUE,
    heading = if (heading) "Appendix S2: Complete Parameter Estimates" else NULL,
    heading_align = "left"
  )
}

## Frequentist Sensitivity Estimates -------------------------------------------

render_sensitivity_table <- function(
  sensitivity_coefficients,
  sensitivity_samples,
  heading = FALSE,
  table_number = "S8"
) {
  sample_max <- function(instrument, statistic) {
    stopifnot(statistic %in% names(sensitivity_samples))
    values <- sensitivity_samples[[statistic]][
      sensitivity_samples$instrument == instrument
    ]
    stopifnot(length(values) > 0L, all(is.finite(values)))
    max(values)
  }
  predictors <- c(
    "duration_mean_between",
    "duration_isd_between",
    "midpoint_mean_between",
    "midpoint_isd_between"
  )
  labels <- c(
    "Sleep Duration (Mean)",
    "Sleep Duration (i#emph[SD])",
    "Sleep Midpoint (Mean)",
    "Sleep Midpoint (i#emph[SD])"
  )
  rows <- list()
  for (outcome in c("externalizing", "internalizing", "total")) {
    title <- paste0(
      toupper(substr(outcome, 1, 1)),
      substring(outcome, 2),
      " Problems"
    )
    rows <- append(rows, list(table_group(title, 10)))
    for (j in seq_along(predictors)) {
      cells <- paste0("#h(0.5em)", labels[j])
      for (instrument in c("CBCL", "YSR")) {
        entry <- sensitivity_coefficients[
          sensitivity_coefficients$instrument == instrument &
            sensitivity_coefficients$outcome_key == outcome &
            sensitivity_coefficients$term == predictors[j],
        ]
        stopifnot(nrow(entry) == 1L)
        bold <- table_excludes_zero(entry$conf_low, entry$conf_high)
        cells <- c(
          cells,
          table_bold(
            table_text(table_number(c(entry$estimate, entry$std_error))),
            bold
          ),
          table_interval(entry$conf_low, entry$conf_high, bold),
          table_text(table_p(entry$p_value))
        )
        if (instrument == "CBCL") cells <- c(cells, "")
      }
      rows <- append(rows, list(cells))
    }
  }
  headers <- list(
    c("", "*CBCL*", "", "", "", "", "*YSR*", "", "", ""),
    c(
      "*Parameter*",
      "*Estimate*",
      "*_SE_*",
      "*95% CI*",
      "*_p_*",
      "",
      "*Estimate*",
      "*_SE_*",
      "*95% CI*",
      "*_p_*"
    )
  )
  note <- paste(
    "Estimates are unstandardized regression coefficients. CBCL and YSR estimates are from",
    "decomposed random-intercept models; only between-person effects are shown.",
    "i#emph[SD] = intraindividual",
    "standard deviation; CBCL = Child Behavior Checklist; YSR = Youth Self-Report;",
    paste0(
      "CI = confidence interval. CBCL _n_ = ",
      table_integer(sample_max("CBCL", "analysis_participants")),
      " participants and ",
      table_integer(sample_max("CBCL", "sessions_analysis_complete")),
      " participant-sessions; YSR _n_ = ",
      table_integer(sample_max("YSR", "analysis_participants")),
      " participants and ",
      table_integer(sample_max("YSR", "sessions_analysis_complete")),
      " participant-sessions."
    ),
    "Bold indicates 95% CIs excluding zero."
  )
  content <- table_figure(
    rows,
    headers,
    c("24%", "8%", "6%", "16%", "7%", "2%", "8%", "6%", "16%", "7%"),
    "Frequentist Sensitivity Analysis: Sleep Parameters as Predictors of CBCL and YSR Scores",
    table_number,
    paste0("tbl-", table_number),
    note,
    font_size = 10,
    header_rows = 2L,
    gutter = 5L,
    note_body_font_size = if (startsWith(table_number, "S3.")) 11 else 10
  )
  content <- sub(
    "\\[\\], \\[\\*CBCL\\*\\], \\[\\], \\[\\], \\[\\], \\[\\], \\[\\*YSR\\*\\], \\[\\], \\[\\], \\[\\]",
    "[], table.cell(colspan: 4)[*CBCL*], [], table.cell(colspan: 4)[*YSR*]",
    content
  )
  table_block(
    content,
    landscape = TRUE,
    heading = if (heading) {
      "Appendix S3: Frequentist Sensitivity Analysis"
    } else {
      NULL
    }
  )
}

## Prior Distributions ---------------------------------------------------------

render_prior_distributions_table <- function(table_number = "S1", note = NULL) {
  rows <- list(
    c("*Population sleep means at centered age*", ""),
    c("Mean duration ($gamma_1$)", "$\"Normal\"(8, 1.5)$"),
    c("Duration log-variance ($gamma_2$)", "$\"Normal\"(0, 1)$"),
    c("Mean midpoint ($gamma_3$)", "$\"Normal\"(3, 2)$"),
    c("Midpoint log-variance ($gamma_4$)", "$\"Normal\"(0, 1.5)$"),
    c("*Age slopes*", ""),
    c("Mean duration ($gamma_(\"age\",1)$)", "$\"Normal\"(0, 0.5)$"),
    c("Duration log-variance ($gamma_(\"age\",2)$)", "$\"Normal\"(0, 0.35)$"),
    c("Mean midpoint ($gamma_(\"age\",3)$)", "$\"Normal\"(0, 0.75)$"),
    c("Midpoint log-variance ($gamma_(\"age\",4)$)", "$\"Normal\"(0, 0.35)$"),
    c("*Residual session-level SDs*", ""),
    c("Duration parameters ($tau_1$, $tau_2$)", "$\"Half-Normal\"(0, 1.5)$"),
    c("Midpoint parameters ($tau_3$, $tau_4$)", "$\"Half-Normal\"(0, 2)$"),
    c("*Latent correlation and standardized effects*", ""),
    c("Correlation matrix ($bold(Omega)$)", "$\"LKJ\"(2)$ via $bold(upright(L))_Omega$"),
    c("Standardized session vectors ($bold(upright(z))_i$)", "$\"Normal\"_4(bold(0), bold(upright(I))_4)$"),
    c("*Outcome model*", ""),
    c("Intercept ($beta_0$)", "$\"Normal\"(50, 20)$"),
    c("Between-person sleep coefficients", "$\"Normal\"(0, 5)$"),
    c("Within-person sleep coefficients", "$\"Normal\"(0, 5)$"),
    c("Nonreference covariate coefficients", "$\"Normal\"(0, 5)$"),
    c("Standardized participant effects ($v_p$)", "$\"Normal\"(0, 1)$"),
    c("Participant random-intercept SD ($sigma_(upright(p))$)", "$\"Exponential\"(1)$"),
    c("Outcome residual SD ($sigma_y$)", "$\"Exponential\"(0.1)$")
  )
  for (i in setdiff(seq_along(rows), c(1, 6, 11, 14, 17))) {
    rows[[i]][1] <- paste0("#h(0.5em)", rows[[i]][1])
  }
  knitr::asis_output(paste0(
    "\n```{=typst}\n",
    table_figure(
      rows,
      list(c("*Parameter*", "*Prior*")),
      c("55%", "45%"),
      "Prior Distributions for All Model Parameters",
      table_number,
      "tbl-priors",
      note,
      font_size = 12,
      row_inset = 2.4
    ),
    "\n```\n"
  ))
}
