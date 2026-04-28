

# ── SHELF: convert expert percentiles to mean + SD ──────────────────────────
# p_low, p50, p_high: the three elicited percentiles
# ci_level: the probability mass between p_low and p_high (e.g. 0.90 for P5-P95)
# Returns list(mean, sd) using:
#   mean = p50  (median as central estimate)
#   sd   = (p_high - p_low) / (2 * qnorm((1 + ci_level) / 2))
shelf_to_mean_sd <- function(p_low, p50, p_high, ci_level = 0.90) {
  ci_level <- as.numeric(ci_level)
  z <- qnorm((1 + ci_level) / 2)
  sd_est <- (p_high - p_low) / (2 * z)
  list(mean = p50, sd = max(sd_est, 1e-6))  # guard against zero/negative SD
}

# ── Resolve mean + SD for one measurement point ─────────────────────────────
# If data_type == "Expert elicited", derive from SHELF inputs;
# otherwise use the directly entered mean/sd values.
resolve_mean_sd <- function(data_type, direct_mean, direct_sd,
                             p_low, p50, p_high, ci_level) {
  if (!is.null(data_type) && data_type == "Expert elicited") {
    shelf_to_mean_sd(p_low, p50, p_high, ci_level)
  } else {
    list(mean = direct_mean, sd = direct_sd)
  }
}

validate_and_run_simulation <- function(input, results, numeric_ids, session, confidence_levels) {

  # Build distribution-appropriate required numeric fields
  dist <- input$distribution
  base_ids <- c("benchmark_value", "impact_area", "offset_area",
                "discount_rate", "time_till_end")

  # Helper: for one measurement point, return the IDs that must be numeric.
  # If the data_type is "Expert elicited", require the SHELF inputs.
  # Otherwise require mean + distribution-appropriate secondary param.
  point_ids <- function(data_type_val, mean_id, sd_id, poisson_n_id, nb_var_id,
                        shelf_p_low, shelf_p50, shelf_p_high) {
    if (!is.null(data_type_val) && data_type_val == "Expert elicited") {
      c(shelf_p_low, shelf_p50, shelf_p_high)
    } else if (dist == "Normal" || is.null(dist) || dist == "") {
      c(mean_id, sd_id)
    } else if (dist == "Poisson") {
      c(mean_id, poisson_n_id)
    } else if (dist == "Negative Binomial") {
      c(mean_id, nb_var_id)
    } else {
      c(mean_id, sd_id)
    }
  }

  active_ids <- c(
    base_ids,
    point_ids(input$prior_impact_data_type,
              "mx_prior_impact_mean", "mx_prior_impact_sd",
              "impact_sample_size_prior", "mx_prior_impact_var",
              "prior_impact_p_low", "prior_impact_p50", "prior_impact_p_high"),
    point_ids(input$post_impact_data_type,
              "mx_post_impact_mean", "mx_post_impact_sd",
              "impact_sample_size_post", "mx_post_impact_var",
              "post_impact_p_low", "post_impact_p50", "post_impact_p_high"),
    point_ids(input$prior_offset_data_type,
              "mx_prior_offset_mean", "mx_prior_offset_sd",
              "offset_sample_size_prior", "mx_prior_offset_var",
              "prior_offset_p_low", "prior_offset_p50", "prior_offset_p_high"),
    point_ids(input$post_offset_data_type,
              "mx_post_offset_mean", "mx_post_offset_sd",
              "offset_sample_size_post", "mx_post_offset_var",
              "post_offset_p_low", "post_offset_p50", "post_offset_p_high")
  )

  # Clear any leftover red outlines from previously required fields
  all_possible <- c(numeric_ids,
                    "impact_sample_size_prior","impact_sample_size_post",
                    "offset_sample_size_prior","offset_sample_size_post",
                    "mx_prior_impact_k","mx_post_impact_k",
                    "mx_prior_offset_k","mx_post_offset_k")
  session$sendCustomMessage("clearAllInvalidInputs", all_possible)

  invalid <- validate_numeric_inputs(active_ids, input, session)

  if (length(invalid) > 0) {
    showModal(modalDialog(
      title = "Invalid inputs: Please enter valid values",
      # paste(
      #   "The following inputs must be valid numbers:",
      #   tags$ul(lapply(invalid, tags$li)),
      #   sep = ""
      # ),
      easyClose = TRUE
    ))
    return(FALSE)  # stop calculation
  }

  # If we reach here → all inputs valid
  sim_results <- simulate_impact_scores(input, confidence_levels)

  results$impact <- sim_results$impact
  results$offset <- sim_results$offset
  results$npbv   <- sim_results$npbv

  enable("save_and_reset")
  TRUE
}

validate_numeric_inputs <- function(ids, input, session) {
  invalid <- c()

  for (id in ids) {
    val <- input[[id]]
    is_bad <- is.null(val) || is.na(suppressWarnings(as.numeric(val)))

    # Add/remove red outline
    if (is_bad) {
      invalid <- c(invalid, id)
      session$sendCustomMessage("addInvalidClass", id)
    } else {
      session$sendCustomMessage("removeInvalidClass", id)
    }
  }

    return(invalid)
  }

simulate_impact_scores <- function(input, confidence_levels) {

  confidence_score <- confidence_levels$scores[confidence_levels$levels == input$selected_confidence]
  discount_factor <- (1 + input$discount_rate / 100)^input$time_till_end

  set.seed(2548)
  n_sims <- 10000
  dist <- input$distribution

  # ── Helper: draw n samples from the selected distribution ──────────────────
  # For Poisson:        user supplies mean (λ) + sample_size (n).
  #                     MoM: population SD = √λ; uncertainty of estimate = √λ/√n.
  #                     We simulate count data then normalise.
  # For Neg-Binomial:   user supplies mean (μ) + size/dispersion (k).
  #                     MoM: var = μ + μ²/k  →  SD = √(μ + μ²/k).
  #                     We use rnbinom(mu, size = k).
  # For Normal (default): mean + SD supplied directly.
  draw_samples <- function(mean_val, sd_or_size, is_poisson_n = FALSE) {
    if (dist == "Poisson") {
      lambda <- max(mean_val, 0.001)   # λ must be > 0
      n      <- max(sd_or_size, 1)     # sample size
      # Simulate counts; sampling distribution of the mean ~ Poisson(λ)/n
      rpois(n_sims, lambda = lambda)
    } else if (dist == "Negative Binomial") {
      mu <- max(mean_val, 0.001)
      k  <- max(sd_or_size, 0.001)    # size / dispersion parameter
      rnbinom(n_sims, size = k, mu = mu)
    } else {
      # Normal (default)
      rnorm(n_sims, mean = mean_val, sd = sd_or_size)
    }
  }

  # ── Resolve mean + SD (direct entry OR SHELF derivation) ───────────────────
  prior_impact_params <- resolve_mean_sd(
    input$prior_impact_data_type,
    input$mx_prior_impact_mean, input$mx_prior_impact_sd,
    input$prior_impact_p_low, input$prior_impact_p50, input$prior_impact_p_high,
    input$prior_impact_ci_level
  )
  post_impact_params <- resolve_mean_sd(
    input$post_impact_data_type,
    input$mx_post_impact_mean, input$mx_post_impact_sd,
    input$post_impact_p_low, input$post_impact_p50, input$post_impact_p_high,
    input$post_impact_ci_level
  )
  prior_offset_params <- resolve_mean_sd(
    input$prior_offset_data_type,
    input$mx_prior_offset_mean, input$mx_prior_offset_sd,
    input$prior_offset_p_low, input$prior_offset_p50, input$prior_offset_p_high,
    input$prior_offset_ci_level
  )
  post_offset_params <- resolve_mean_sd(
    input$post_offset_data_type,
    input$mx_post_offset_mean, input$mx_post_offset_sd,
    input$post_offset_p_low, input$post_offset_p50, input$post_offset_p_high,
    input$post_offset_ci_level
  )

  # ── Pull distribution-specific secondary parameters ────────────────────────
  # For Poisson/NegBin we still need the secondary param (n or k).
  # For expert-elicited points we treat as Normal using SHELF-derived mean+SD.
  eff_dist_prior_impact  <- if (!is.null(input$prior_impact_data_type)  && input$prior_impact_data_type  == "Expert elicited") "Normal" else dist
  eff_dist_post_impact   <- if (!is.null(input$post_impact_data_type)   && input$post_impact_data_type   == "Expert elicited") "Normal" else dist
  eff_dist_prior_offset  <- if (!is.null(input$prior_offset_data_type)  && input$prior_offset_data_type  == "Expert elicited") "Normal" else dist
  eff_dist_post_offset   <- if (!is.null(input$post_offset_data_type)   && input$post_offset_data_type   == "Expert elicited") "Normal" else dist

  prior_impact_sec  <- if (eff_dist_prior_impact  == "Normal") prior_impact_params$sd
                       else if (eff_dist_prior_impact  == "Poisson") input$impact_sample_size_prior
                       else input$mx_prior_impact_var   # variance; k derived in draw_samples_eff

  post_impact_sec   <- if (eff_dist_post_impact   == "Normal") post_impact_params$sd
                       else if (eff_dist_post_impact   == "Poisson") input$impact_sample_size_post
                       else input$mx_post_impact_var

  prior_offset_sec  <- if (eff_dist_prior_offset  == "Normal") prior_offset_params$sd
                       else if (eff_dist_prior_offset  == "Poisson") input$offset_sample_size_prior
                       else input$mx_prior_offset_var

  post_offset_sec   <- if (eff_dist_post_offset   == "Normal") post_offset_params$sd
                       else if (eff_dist_post_offset   == "Poisson") input$offset_sample_size_post
                       else input$mx_post_offset_var

  # ── Simulate attribute-level values ────────────────────────────────────────
  # draw_samples uses `dist` from outer scope; override locally for expert points
  draw_samples_eff <- function(eff_d, mean_val, sec_val) {
    if (eff_d == "Poisson") {
      lambda <- max(mean_val, 0.001); n <- max(sec_val, 1)
      rpois(n_sims, lambda = lambda)
    } else if (eff_d == "Negative Binomial") {
      mu  <- max(mean_val, 0.001)
      var <- max(sec_val, mu + 0.001)          # variance must exceed mean
      k   <- mu^2 / (var - mu)                 # MoM: k = mu^2 / (s^2 - mu)
      rnbinom(n_sims, size = k, mu = mu)
    } else {
      rnorm(n_sims, mean = mean_val, sd = max(sec_val, 1e-6))
    }
  }

  mx_prior_impact <- draw_samples_eff(eff_dist_prior_impact,  prior_impact_params$mean,  prior_impact_sec)
  mx_post_impact  <- draw_samples_eff(eff_dist_post_impact,   post_impact_params$mean,   post_impact_sec)
  mx_prior_offset <- draw_samples_eff(eff_dist_prior_offset,  prior_offset_params$mean,  prior_offset_sec)
  mx_post_offset  <- draw_samples_eff(eff_dist_post_offset,   post_offset_params$mean,   post_offset_sec)

  # For Poisson: the draws are counts; adjust by sample size to get mean estimate.
  # Only apply for points that are actually using Poisson (not expert-elicited overrides).
  if (eff_dist_prior_impact  == "Poisson") mx_prior_impact <- mx_prior_impact / max(input$impact_sample_size_prior, 1)
  if (eff_dist_post_impact   == "Poisson") mx_post_impact  <- mx_post_impact  / max(input$impact_sample_size_post,  1)
  if (eff_dist_prior_offset  == "Poisson") mx_prior_offset <- mx_prior_offset / max(input$offset_sample_size_prior, 1)
  if (eff_dist_post_offset   == "Poisson") mx_post_offset  <- mx_post_offset  / max(input$offset_sample_size_post,  1)

  # Calculate raw changes
  raw_impact_change <- mx_post_impact - mx_prior_impact
  raw_offset_change <- mx_post_offset - mx_prior_offset

  # Convert to proportion of benchmark
  impact_proportion <- raw_impact_change / input$benchmark_value
  offset_proportion <- raw_offset_change / input$benchmark_value

  # Apply area weighting and other multipliers to the full simulation vectors.
  # Uncertainty is represented by the distribution of these vectors — 
  # prediction intervals are derived via quantiles at the display/save layer.
  impact_score <- impact_proportion * input$impact_area
  offset_score <- offset_proportion * confidence_score / discount_factor * input$offset_area
  npbv_score   <- impact_score + offset_score

  list(
    impact = impact_score,
    offset = offset_score,
    npbv   = npbv_score
  )
}

make_summary_datatable <- function(df) {

  if (nrow(df) == 0) {
    return(DT::datatable(df))
  }

  DT::datatable(
    df,
    escape = FALSE,
    selection = 'none',
    options = list(dom = 't', paging = FALSE)
  )
}

compute_summary_df <- function(df, fmt_pi, npbv_colorize, draw_store = NULL) {

  if (nrow(df) == 0) return(df)

  summary_df <- compute_summary(df, draw_store)

  summary_df %>%
    dplyr::mutate(
      Impact = fmt_pi(Impact_Mean, Impact_P5, Impact_P95),
      Offset = fmt_pi(Offset_Mean, Offset_P5, Offset_P95),
      NPBV   = npbv_colorize(NPBV_Mean, NPBV_P5, NPBV_P95,
                             fmt_pi(NPBV_Mean, NPBV_P5, NPBV_P95))
    ) %>%
    dplyr::select(Component, Impact, Offset, NPBV) %>%
    clean_names()
}



compute_summary <- function(df, draw_store = NULL) {
  if (is.null(df) || !nrow(df)) {
    return(data.frame(
      Component   = character(),
      Impact_Mean = numeric(), Impact_P5 = numeric(), Impact_P95 = numeric(),
      Offset_Mean = numeric(), Offset_P5 = numeric(), Offset_P95 = numeric(),
      NPBV_Mean   = numeric(), NPBV_P5   = numeric(), NPBV_P95   = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  # ── Pooled-draws aggregation (Option 5: equal-weight, asymmetry-preserving) ──

  # For each component, average the per-attribute draw vectors element-wise,
  # then take empirical quantiles of the resulting component-level distribution.
  # This gives an equally-weighted mean (per the BOAM manual) with honestly

  # asymmetric prediction intervals — no back-solving SDs, no Normality assumption.
  #
  # Fallback: if draw_store is NULL or missing entries (e.g. report render before
  # re-simulation on draft import), use simple unweighted mean of summary stats
  # with approximate uncertainty via averaging P5s and P95s. This is less precise
  # but directionally correct and avoids the inverse-variance weighting problem.

  # Determine which rows have draws available
  has_draws <- function(row_idx) {
    if (is.null(draw_store)) return(FALSE)
    key <- paste0("row_", row_idx)
    !is.null(draw_store[[key]])
  }

  # Add original row indices to df for draw_store lookup
  df$.row_idx <- seq_len(nrow(df))

  components <- unique(df$biodiversity_component)
  out_list <- vector("list", length(components))

  for (ci in seq_along(components)) {
    comp <- components[ci]
    rows <- df[df$biodiversity_component == comp, , drop = FALSE]
    row_indices <- rows$.row_idx

    # Check if ALL rows in this component have draws
    all_have_draws <- all(vapply(row_indices, has_draws, logical(1)))

    if (all_have_draws && length(row_indices) > 0) {
      # ── Pooled-draws path ──────────────────────────────────────────────────
      # Stack draw vectors into matrices (n_attributes × n_sims), take colMeans
      impact_mat <- do.call(rbind, lapply(row_indices, function(ri) {
        draw_store[[paste0("row_", ri)]]$impact
      }))
      offset_mat <- do.call(rbind, lapply(row_indices, function(ri) {
        draw_store[[paste0("row_", ri)]]$offset
      }))
      npbv_mat <- do.call(rbind, lapply(row_indices, function(ri) {
        draw_store[[paste0("row_", ri)]]$npbv
      }))

      # Equal-weight component mean: colMeans across attributes
      comp_impact <- colMeans(impact_mat)
      comp_offset <- colMeans(offset_mat)
      comp_npbv   <- colMeans(npbv_mat)

      out_list[[ci]] <- data.frame(
        Component   = comp,
        Impact_Mean = round(mean(comp_impact), 2),
        Impact_P5   = round(quantile(comp_impact, 0.05), 2),
        Impact_P95  = round(quantile(comp_impact, 0.95), 2),
        Offset_Mean = round(mean(comp_offset), 2),
        Offset_P5   = round(quantile(comp_offset, 0.05), 2),
        Offset_P95  = round(quantile(comp_offset, 0.95), 2),
        NPBV_Mean   = round(mean(comp_npbv), 2),
        NPBV_P5     = round(quantile(comp_npbv, 0.05), 2),
        NPBV_P95    = round(quantile(comp_npbv, 0.95), 2),
        stringsAsFactors = FALSE
      )
    } else {
      # ── Fallback: simple unweighted mean of summary stats ──────────────────
      out_list[[ci]] <- data.frame(
        Component   = comp,
        Impact_Mean = round(mean(rows$Impact_Mean), 2),
        Impact_P5   = round(mean(rows$Impact_P5),   2),
        Impact_P95  = round(mean(rows$Impact_P95),  2),
        Offset_Mean = round(mean(rows$Offset_Mean), 2),
        Offset_P5   = round(mean(rows$Offset_P5),   2),
        Offset_P95  = round(mean(rows$Offset_P95),  2),
        NPBV_Mean   = round(mean(rows$NPBV_Mean),   2),
        NPBV_P5     = round(mean(rows$NPBV_P5),     2),
        NPBV_P95    = round(mean(rows$NPBV_P95),    2),
        stringsAsFactors = FALSE
      )
    }
  }

  # Clean up temp column
  df$.row_idx <- NULL

  result <- do.call(rbind, out_list)
  result[is.na(result)] <- 0
  rownames(result) <- NULL
  result
}

save_and_reset_logic <- function(input, results, project_calc_fields,
                                 saved_results, editing, edit_row_idx,
                                 summary_results, session, draw_store = NULL) {

  # --- 1. Safety check
  if (is.null(results$impact) || is.null(results$offset) || is.null(results$npbv)) {
    showNotification("Please run or re-run simulations before saving.", type = "error")
    return(NULL)
  }

  # --- 2. Compute stats: use empirical prediction intervals from simulation vectors
  pi_low  <- function(x) round(quantile(x, 0.05), 2)
  pi_high <- function(x) round(quantile(x, 0.95), 2)

  stats <- list(
    Impact_Mean = round(mean(results$impact), 2),
    Impact_P5   = pi_low(results$impact),
    Impact_P95  = pi_high(results$impact),
    Offset_Mean = round(mean(results$offset), 2),
    Offset_P5   = pi_low(results$offset),
    Offset_P95  = pi_high(results$offset),
    NPBV_Mean   = round(mean(results$npbv), 2),
    NPBV_P5     = pi_low(results$npbv),
    NPBV_P95    = pi_high(results$npbv)
  )

  # --- 3. Collect raw input values
  vals <- lapply(project_calc_fields, function(id) input[[id]])
  names(vals) <- project_calc_fields

  # Row to write into the table
  new_row <- as.data.frame(
    c(vals, stats),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # --- 4. Write logic: overwrite or append
  df <- saved_results()
  if (is.null(df)) df <- data.frame(stringsAsFactors = FALSE)

  # Capture draws for draw_store before they get nulled
  current_draws <- list(
    impact = results$impact,
    offset = results$offset,
    npbv   = results$npbv
  )

  if (isTRUE(editing()) && !is.null(edit_row_idx()) && nrow(df) > 0) {

    # Add any missing columns to df (typed NA)
    for (nm in setdiff(names(new_row), names(df))) {
      cls <- class(new_row[[nm]])
      if ("character" %in% cls) {
        df[[nm]] <- rep(NA_character_, nrow(df))
      } else if (any(c("numeric", "double") %in% cls)) {
        df[[nm]] <- rep(NA_real_, nrow(df))
      } else if ("integer" %in% cls) {
        df[[nm]] <- rep(NA_integer_, nrow(df))
      } else {
        df[[nm]] <- NA
      }
    }

    # Add any missing columns to new_row (typed NA)
    for (nm in setdiff(names(df), names(new_row))) {
      cls <- class(df[[nm]])
      if ("character" %in% cls) {
        new_row[[nm]] <- NA_character_
      } else if (any(c("numeric", "double") %in% cls)) {
        new_row[[nm]] <- NA_real_
      } else if ("integer" %in% cls) {
        new_row[[nm]] <- NA_integer_
      } else {
        new_row[[nm]] <- NA
      }
    }

    # Align columns
    new_row <- new_row[, names(df), drop = FALSE]

    # Overwrite the row
    i <- edit_row_idx()
    if (!is.na(i) && i >= 1 && i <= nrow(df)) {
      df[i, ] <- new_row[1, ]
      # Update draw_store at the edited index
      if (!is.null(draw_store)) {
        draw_store[[paste0("row_", i)]] <- current_draws
      }
    } else {
      showNotification("Edit index out of range; appending new row.", type = "warning")
      df <- dplyr::bind_rows(df, new_row)
      # Stash draws for the new row
      if (!is.null(draw_store)) {
        draw_store[[paste0("row_", nrow(df))]] <- current_draws
      }
    }

    # Exit edit mode
    editing(FALSE)
    edit_row_idx(NULL)

    updateActionButton(session, "calculate",      label = "Run Simulations")
    updateActionButton(session, "save_and_reset", label = "Save to Report & Reset")

  } else {
    # Simple append
    df <- dplyr::bind_rows(df, new_row)
    # Stash draws for the new row
    if (!is.null(draw_store)) {
      draw_store[[paste0("row_", nrow(df))]] <- current_draws
    }
  }

  # Store updated table
  saved_results(df)

  # --- 5. Update summary and reset UI
  summary_results(compute_summary(saved_results(), draw_store))

  results$impact <- results$offset <- results$npbv <- NULL

  reset_project_calc_inputs(session)
  disable("save_and_reset")

  session$sendCustomMessage("scrollToBottom", "now")

  print_saved_results(saved_results(), title = "SAVED RESULTS TABLE AFTER SAVE")

  return(df)
}

# * remove underscores in table column names
clean_names <- function(df) {
  if (is.null(df)) return(df)
  names(df) <- gsub("_", " ", names(df))
  df
}


# * Format mean with asymmetric prediction interval: "mean (P5, P95)"
fmt_pi <- function(m, p5, p95) {
  ifelse(is.na(m) | is.na(p5) | is.na(p95), "",
         sprintf("%.2f (%.2f, %.2f)", m, p5, p95))
}

# Backwards-compatible alias so any remaining fmt_pm references still work
fmt_pm <- fmt_pi

# Wrap NPBV text red if lower PI bound (P5) <= 0 — offset unlikely to cover impact
npbv_colorize <- function(mean, p5, p95, text) {
  ifelse(p5 <= 0,
         sprintf('<span class="npbv-alert">%s</span>', text),
         text)
}

prepare_saved_results_df <- function(df) {

  if (nrow(df) == 0) {
    return(df)
  }

  # Delete button
  delete_btns <- sprintf(
    '<button class="btn btn-sm btn-danger delete-btn" data-row="%s">Delete</button>',
    seq_len(nrow(df))
  )

  # Edit button
  edit_btns <- sprintf(
    '<button class="btn btn-sm btn-danger edit-btn" data-row="%s">Edit</button>',
    seq_len(nrow(df))
  )

  # Add columns
  df <- cbind(Delete = delete_btns, Edit = edit_btns, df)

  # Apply formatters + select columns
  df <- df %>%
    dplyr::mutate(
      Impact = fmt_pi(Impact_Mean, Impact_P5, Impact_P95),
      Offset = fmt_pi(Offset_Mean, Offset_P5, Offset_P95),
      NPBV   = npbv_colorize(
        NPBV_Mean, NPBV_P5, NPBV_P95,
        fmt_pi(NPBV_Mean, NPBV_P5, NPBV_P95)
      )
    ) %>%
    dplyr::select(
      Delete, Edit,
      biodiversity_type, biodiversity_component, biodiversity_attribute,
      measurement_unit,
      Impact, Offset, NPBV
    )

  df <- clean_names(df)

  df
}

make_saved_results_datatable <- function(df, edit_row_idx) {

  # Empty table case
  if (nrow(df) == 0) {
    return(DT::datatable(df))
  }

  col_labels <- c(
    "", "",                     # Delete, Edit
    "Biodiversity Type",
    "Component",
    "Attribute",
    "Measurement Unit",
    "Impact", "Offset", "NPBV"
  )

  js_idx <- ifelse(is.null(edit_row_idx), "null", edit_row_idx)

  DT::datatable(
    df,
    colnames = col_labels,
    escape = FALSE,
    selection = "none",
    options = list(
      dom = "t",
      paging = FALSE,
      columnDefs = list(
        list(targets = c(1,2), orderable = FALSE, searchable = FALSE)
      ),
      rowCallback = DT::JS(sprintf(
        "function(row, data, displayNum, displayIndex, dataIndex){
           var editIdx = %s;
           var thisIdx = dataIndex + 1;
           var $row = $(row);
           if (editIdx !== null && thisIdx === editIdx){
             $row.addClass('editing-row');
             $row.find('.edit-btn').text('EDITING');
           } else {
             $row.removeClass('editing-row');
             $row.find('.edit-btn').text('Edit');
           }
         }",
        js_idx
      ))
    ),
    callback = DT::JS("
      table.on('click', '.delete-btn', function() {
        Shiny.setInputValue('delete_row', this.getAttribute('data-row'), {priority:'event'});
      });
      table.on('click', '.edit-btn', function() {
        var $tr = $(this).closest('tr');
        table.$('tr').removeClass('editing-row');
        table.$('.edit-btn').text('Edit');
        $tr.addClass('editing-row');
        $(this).text('EDITING');
        var rowIdx = table.row($tr).index() + 1;
        Shiny.setInputValue('edit_row', rowIdx, {priority:'event'});
      });
    ")
  )
}




# ── Re-simulate draws from a saved row's inputs ─────────────────────────────
# Used on draft import to repopulate draw_store from persisted input values.
# Each row in saved_results contains all the input fields needed to re-run
# simulate_impact_scores(). We build a mock input list from those fields.
resimulate_row <- function(row_list, confidence_levels) {
  # row_list is a named list (one row of saved_results coerced via as.list)
  # simulate_impact_scores expects an input-like list with $ access
  mock_input <- row_list
  simulate_impact_scores(mock_input, confidence_levels)
}

# ── Re-index draw_store after a row deletion ─────────────────────────────────
# When row i is deleted, rows i+1..n shift down by one. The draw_store keys
# (row_1, row_2, ...) need to be re-indexed to match.
reindex_draw_store <- function(draw_store, deleted_row, old_nrow) {
  if (is.null(draw_store)) return(invisible(NULL))

  # Remove the deleted row's draws
  draw_store[[paste0("row_", deleted_row)]] <- NULL

  # Shift all keys above the deleted row down by one
  if (deleted_row < old_nrow) {
    for (i in (deleted_row + 1):old_nrow) {
      old_key <- paste0("row_", i)
      new_key <- paste0("row_", i - 1)
      draw_store[[new_key]] <- draw_store[[old_key]]
      draw_store[[old_key]] <- NULL
    }
  }
  invisible(NULL)
}

# ── Repopulate draw_store from saved_results after draft import ──────────────
# Loops through all rows, re-runs simulate_impact_scores for each, and stashes
# the draw vectors. Deterministic because simulate_impact_scores calls set.seed.
repopulate_draw_store <- function(df, draw_store, confidence_levels) {
  if (is.null(df) || nrow(df) == 0) return(invisible(NULL))

  for (i in seq_len(nrow(df))) {
    row_list <- as.list(df[i, , drop = FALSE])
    sim <- tryCatch(
      resimulate_row(row_list, confidence_levels),
      error = function(e) NULL
    )
    if (!is.null(sim)) {
      draw_store[[paste0("row_", i)]] <- sim
    }
  }
  invisible(NULL)
}


# build_saved_table_df <- function(df, idx, fmt_pm, npbv_colorize) {
#
#   if (nrow(df) == 0) {
#     return(list(
#       df = df,
#       labels = colnames(df)
#     ))
#   }
#
#   # --- Add Delete buttons
#   delete_btns <- sprintf(
#     '<button class="btn btn-sm btn-danger delete-btn" data-row="%s">Delete</button>',
#     seq_len(nrow(df))
#   )
#   df <- cbind(Delete = delete_btns, df)
#
#   # --- Add Edit buttons
#   edit_btns <- sprintf(
#     '<button class="btn btn-sm btn-danger edit-btn" data-row="%s">Edit</button>',
#     seq_len(nrow(df))
#   )
#   df <- cbind(Edit = edit_btns, df)
#
#   # --- Collapse mean + CI into single text fields
#   df <- df %>%
#     dplyr::mutate(
#       Impact = fmt_pm(Impact_Mean, Impact_CI),
#       Offset = fmt_pm(Offset_Mean, Offset_CI),
#       NPBV   = npbv_colorize(NPBV_Mean, NPBV_CI, fmt_pm(NPBV_Mean, NPBV_CI))
#     ) %>%
#     dplyr::select(
#       Delete, Edit,
#       biodiversity_type, biodiversity_component, biodiversity_attribute, measurement_unit,
#       Impact, Offset, NPBV
#     )
#
#   # --- Clean column headers
#   df <- clean_names(df)
#
#   # --- Labels
#   col_labels <- c(
#     "", "",                    # Delete, Edit
#     "Biodiversity Type",
#     "Component",
#     "Attribute",
#     "Measurement Unit",
#     "Impact", "Offset", "NPBV"
#   )
#
#   list(
#     df = df,
#     labels = col_labels,
#     idx = idx
#   )
# }


