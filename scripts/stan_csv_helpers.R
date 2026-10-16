# CmdStan CSV Helpers ----------------------------------------------------------

## Headers ---------------------------------------------------------------------

stan_csv_header <- function(con, path) {
  metadata <- list()
  repeat {
    line <- readLines(con, n = 1L, warn = FALSE)
    line <- trimws(line)
    if (!nzchar(line)) next
    if (!startsWith(line, "#")) break
    entry <- regmatches(line, regexec(
      "^#[[:space:]]*([[:alnum:]_]+)[[:space:]]*=[[:space:]]*([^[:space:]]+)",
      line
    ))[[1L]]
    if (length(entry)) metadata[[entry[2L]]] <- entry[3L]
  }
  header <- strsplit(line, ",", fixed = TRUE)[[1L]]
  list(metadata = metadata, header = header)
}

stan_csv_configuration <- function(path) {
  con <- file(path, "r")
  on.exit(close(con))
  stan_csv_header(con, path)$metadata
}

stan_csv_settings <- function(metadata) {
  fixed <- identical(metadata$algorithm, "fixed_param")
  for (field in c("num_warmup", "save_warmup")) {
    if (fixed && is.null(metadata[[field]])) metadata[[field]] <- "0"
  }
  samples <- as.integer(metadata$num_samples)
  warmup <- as.integer(metadata$num_warmup)
  thin <- as.integer(metadata$thin)
  saved <- tolower(metadata$save_warmup) %in% c("1", "true")

  list(
    metadata = metadata,
    settings = list(
      num_samples = samples, num_warmup = if (fixed) 0L else warmup,
      thin = thin, save_warmup = !fixed && saved,
      algorithm = metadata$algorithm, model = metadata$model
    ),
    warmup_rows = if (!fixed && saved) ceiling(warmup / thin) else 0L,
    sampling_rows = ceiling(samples / thin)
  )
}

## Selected Draws --------------------------------------------------------------

read_stan_draws <- function(csv_files, variables, draw_ids = NULL) {
  metadata <- vector("list", length(csv_files))
  out <- NULL

  read_chain <- function(path, ids, chain) {
    con <- file(path, "r")
    on.exit(close(con))
    info <- stan_csv_header(con, path)
    configuration <- stan_csv_settings(info$metadata)
    header <- info$header
    canonical <- sub("\\.([0-9.]+)$", "[\\1]", header)
    canonical <- gsub(".", ",", canonical, fixed = TRUE)
    base <- sub("\\..*$", "", header)
    selected <- which(canonical %in% variables | base %in% variables)

    expected <- configuration$sampling_rows
    if (!is.null(ids)) ids <- sort(as.integer(ids))
    n_selected <- if (is.null(ids)) expected else length(ids)
    current_metadata <- configuration$metadata
    if (chain == 1L) {
      out <<- array(
        NA_real_, c(n_selected, length(csv_files), length(selected)),
        dimnames = list(iteration = NULL, chain = NULL, variable = canonical[selected])
      )
    }

    rows <- 0L
    written <- 0L
    repeat {
      line <- readLines(con, n = 1L, warn = FALSE)
      if (!length(line)) break
      if (!nzchar(line)) next
      if (startsWith(line, "#")) next
      if (!nzchar(trimws(line))) next
      rows <- rows + 1L

      commas <- gregexpr(",", line, perl = TRUE, useBytes = TRUE)[[1L]]
      if (commas[1L] == -1L) commas <- integer()
      row_length <- nchar(line, type = "bytes")
      iteration <- rows - configuration$warmup_rows
      if (iteration <= 0L ||
          (!is.null(ids) && (written == length(ids) || iteration != ids[written + 1L]))) {
        next
      }
      starts <- c(1L, commas + 1L)
      ends <- c(commas - 1L, row_length)
      values <- suppressWarnings(as.numeric(substring(line, starts[selected], ends[selected])))
      written <- written + 1L
      out[written, chain, ] <<- values
    }
    current_metadata$sampling_rows <- expected
    current_metadata$selected_rows <- written
    metadata[[chain]] <<- current_metadata
  }

  for (chain in seq_along(csv_files)) {
    ids <- if (is.list(draw_ids)) draw_ids[[chain]] else draw_ids
    read_chain(csv_files[chain], ids, chain)
  }
  draws <- posterior::as_draws_array(out)
  attr(draws, "cmdstan_metadata") <- metadata
  draws
}

## Summaries -------------------------------------------------------------------

sampler_diagnostics <- function(draws, max_treedepth = 12L) {
  x <- posterior::as_draws_array(draws)
  get <- function(variable, chain) as.numeric(x[, chain, variable])
  lapply(seq_len(dim(x)[2L]), function(chain) {
    energy <- get("energy__", chain)
    dplyr::tibble(
      chain = chain, post_warmup_draws = dim(x)[1L],
      divergences = sum(get("divergent__", chain)),
      treedepth_saturations = sum(get("treedepth__", chain) >= max_treedepth),

      ebfmi = if (length(energy) < 3L) NA_real_ else
        (sum(diff(energy)^2) / length(energy)) / stats::var(energy)
    )
  }) |>
    dplyr::bind_rows()
}
