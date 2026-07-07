new_report_modal <- function() {
  modalDialog(
    title = "Start a new report?",
    HTML("<p>This will discard all current inputs and saved results.</p>"),
    easyClose = FALSE,
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_new", "Continue", class = "btn btn-danger")
    )
  )
}

restore_autosave_modal <- function(timestamp) {
  modalDialog(
    title = "Resume previous session?",
    HTML(paste0(
      "<p>We found an autosaved draft from your last session (", timestamp, ").</p>",
      "<p>Would you like to restore it?</p>"
    )),
    easyClose = FALSE,
    footer = tagList(
      actionButton("discard_autosave", "Discard", class = "btn btn-secondary"),
      actionButton("restore_autosave", "Restore", class = "btn btn-dark-green")
    )
  )
}

idle_modal <- function() {
  modalDialog(
    title = "Still there?",
    HTML("<p>You've been inactive for a while. This session may disconnect soon if it stays idle &mdash; click below to keep working.</p>"),
    easyClose = TRUE,
    footer = tagList(
      actionButton("stay_active", "I'm still here", class = "btn btn-dark-green")
    )
  )
}

# * Reusable console printer for saved_results -------------------------------
print_saved_results <- function(df, title = "SAVED RESULTS TABLE") {
  if (is.null(df)) {
    cat(sprintf("\n\n******* %s *******\n<NULL>\n*************** END ***********************\n\n", title))
    return(invisible(NULL))
  }
  if (!is.data.frame(df) || nrow(df) == 0) {
    cat(sprintf("\n\n******* %s (0 rows) *******\n", title))
    if (requireNamespace("tibble", quietly = TRUE)) {
      print(tibble::as_tibble(df), n = Inf, width = Inf)
    } else {
      print(df)
    }
    cat("*************** END ***********************\n\n")
    return(invisible(df))
  }
  
  df_print <- df
  short_map <- c(
    "Impact_Mean" = "ImpM", "Impact_P5" = "ImpP5", "Impact_P95" = "ImpP95",
    "Offset_Mean" = "OffM", "Offset_P5" = "OffP5", "Offset_P95" = "OffP95",
    "NPBV_Mean" = "NPBVM", "NPBV_P5" = "NPBVP5", "NPBV_P95" = "NPBVP95",
    # biodiversity
    "biodiversity_type" = "bioType",
    "biodiversity_component" = "bComp",
    "biodiversity_attribute" = "bAttr",
    "measurement_unit" = "Munit",
    "benchmark_value" = "bench",
    "distribution" = "dist",
    "num_simulations" = "sims",
    # impact
    "impact_area_unit" = "impU", 
    "impact_area" = "impA",
    "impact_area_data_type" = "impTyp",
    "impact_area_empirical_details" = "impED",
    "impact_area_modelled_details" = "impMD",
    "impact_area_expert_details" = "impXD",
    "impact_area_proxy_details" = "impPD",
    "mx_prior_impact_mean" = "ImpPriorM",
    "mx_prior_impact_sd" = "ImpPriorSD",
    "prior_impact_sd_data_type" = "ImpSDT",
    "prior_impact_sd_empirical_details" = "ImpSDEmp",
    "prior_impact_sd_modelled_details" = "ImpSDMod",
    "prior_impact_sd_expert_details" = "ImpSDExp",
    "prior_impact_sd_proxy_details" = "ImpSDPrx",
    "mx_post_impact_mean" = "ImpPostM",
    "mx_post_impact_sd" = "ImpPostSD",
    # offset
    "offset_area" = "OffA",
    "offset_area_unit" = "offU",
    "mx_prior_offset_mean" = "OffPriorM",
    "mx_prior_offset_sd" = "OffPriorSD",
    "prior_offset_sd_data_type" = "OffSDT",
    "prior_offset_sd_empirical_details" = "OffSDEmp",
    "prior_offset_sd_modelled_details" = "OffSDMod",
    "prior_offset_sd_expert_details" = "OffSDExp",
    "prior_offset_sd_proxy_details" = "OffSDPrx",
    "mx_post_offset_mean" = "OffPostM",
    "mx_post_offset_sd" = "OffPostSD",
    
    
    "post_offset_sd_data_type" = "OffSDT_post",
    "post_offset_sd_empirical_details" = "OffSDEmp_post",
    "post_offset_sd_modelled_details" = "OffSDMod_post",
    "post_offset_sd_expert_details" = "OffSDExp_post",
    "post_offset_sd_proxy_details" = "OffSDPrx_post",
    
    
    "selected_confidence" = "Conf",
    "offset_confidence_justify" = "ConfWhy",
    "time_till_end" = "Yr",
    "offset_time_till_end_justify" = "YrWhy",
    "discount_rate" = "Dis",
    "offset_discount_rate_justify" = "DisWhy"
  )
  cn <- colnames(df_print)
  idx <- match(cn, names(short_map), nomatch = 0)
  cn[idx > 0] <- unname(short_map[cn[idx > 0]])
  colnames(df_print) <- cn
  first <- c("bioType","bComp","bAttr","Munit","ImpM","ImpCI","OffM","OffCI","NPBVM","NPBVCI")
  show_cols <- c(intersect(first, colnames(df_print)),
                 setdiff(colnames(df_print), first))
  df_print <- df_print[, show_cols, drop = FALSE]
  
  cat(sprintf("\n\n******* %s *******\n", title))
  if (requireNamespace("tibble", quietly = TRUE)) {
    print(tibble::as_tibble(df_print), n = Inf, width = Inf)
  } else {
    print(df_print)
  }
  cat("*************** END ***********************\n\n")
  invisible(df_print)
}

# Build the JSON-ready payload (inputs + saved_results) used by both the
# "Save Draft" download and the browser-side autosave.
build_draft_payload <- function(input, draft_fields, saved_results) {
  vals <- lapply(draft_fields, function(id) input[[id]])
  names(vals) <- draft_fields
  list(
    inputs = vals,
    saved_results = saved_results()
  )
}

import_draft_file <- function(file_input, session, results,
                              saved_results, summary_results,
                              compute_summary,
                              draw_store = NULL, confidence_levels = NULL) {

  req(file_input$datapath)
  txt <- readLines(file_input$datapath, warn = FALSE, encoding = "UTF-8")
  dat <- tryCatch(jsonlite::fromJSON(paste(txt, collapse = "\n")), error = function(e) NULL)
  req(!is.null(dat))

  restore_draft_data(dat, session, results, saved_results, summary_results,
                     compute_summary, draw_store, confidence_levels)
}

# Shared restore logic for both "Load Draft" (from file) and autosave
# restore (from browser localStorage), once `dat` has been parsed into a list.
restore_draft_data <- function(dat, session, results,
                               saved_results, summary_results,
                               compute_summary,
                               draw_store = NULL, confidence_levels = NULL) {

  getOr <- function(x, nm, default = NULL) if (!is.null(x[[nm]])) x[[nm]] else default
  ins <- if (is.null(dat$inputs)) list() else dat$inputs  # nil guard
  
  # --- Project Details ---
  updateTextInput(session, "project_name", value = getOr(ins, "project_name", ""))
  updateTextInput(session, "prepared_by",  value = getOr(ins, "prepared_by", ""))
  if (!is.null(ins$date)) suppressWarnings(updateDateInput(session, "date", value = as.Date(ins$date)))
  
  updateTextAreaInput(session, "proposal_overview",   value = getOr(ins, "proposal_overview", ""))
  updateTextAreaInput(session, "ecological_context",  value = getOr(ins, "ecological_context", ""))
  updateTextAreaInput(session, "biodiversity_impacts",  value = getOr(ins, "biodiversity_impacts", ""))
  updateTextAreaInput(session, "offset_package",      value = getOr(ins, "offset_package", ""))
  
  # --- Biodiversity / Impact / Offset (all Project Calculations fields) ---
  restore_inputs_from_list(session, ins)

  # --- Restore table from draft ---
  if (!is.null(dat$saved_results)) {
    # dat$saved_results may already be a data.frame (file-based import, via
    # jsonlite::fromJSON of a row-records array) or a columnar list
    # ({"col": [val], ...} - what Shiny produces serializing a 1-row
    # data.frame via sendCustomMessage, received here as autosave_check).
    # Normalize the columnar case into a data.frame column-by-column.
    df <- tryCatch({
      sr <- dat$saved_results
      if (is.data.frame(sr)) {
        sr
      } else if (is.list(sr) && length(sr) > 0) {
        sr <- lapply(sr, function(col) {
          if (is.list(col)) {
            col <- lapply(col, function(v) if (is.null(v) || length(v) == 0) NA else v)
            col <- unlist(col)
          }
          col
        })
        as.data.frame(sr, stringsAsFactors = FALSE)
      } else {
        NULL
      }
    }, error = function(e) NULL)
    if (!is.null(df) && !is.data.frame(df)) df <- NULL
    if (!is.null(df)) {
      saved_results(df)                                 # update backend table
      # Repopulate draw_store by re-simulating each saved row
      if (!is.null(draw_store) && !is.null(confidence_levels)) {
        # Clear any existing draw_store entries
        for (nm in names(draw_store)) draw_store[[nm]] <- NULL
        repopulate_draw_store(df, draw_store, confidence_levels)
      }
      summary_results(compute_summary(saved_results(), draw_store)) # recompute summary
    } else {
      saved_results(data.frame(stringsAsFactors = FALSE))
    }
  } else {
    saved_results(data.frame(stringsAsFactors = FALSE))
    summary_results(compute_summary(saved_results(), draw_store))
  }
  
  # Clear any old simulation vectors
  results$impact <- results$offset <- results$npbv <- NULL
  disable("save_and_reset")
  
  showNotification("Draft imported.", type = "message", duration = 3)
}

# helper function to avoid duplicating code
render_report <- function(input, saved_results, summary_results, file,
                          long_report_flag, draw_store = NULL) {
  # Use a temp directory for rendering
  temp_dir <- tempdir()
  input_rmd <- file.path(temp_dir, "report.Rmd")
  preamble_file <- file.path(temp_dir, "preamble.tex")  # target location
  
  # Copy the Rmd and tex to the temp directory
  file.copy("report.Rmd", input_rmd, overwrite = TRUE)
  file.copy("preamble.tex", preamble_file, overwrite = TRUE)
  
  # Copy all .otf font files from www
  font_files <- list.files("www", pattern = "texgyreheros.*\\.otf$", full.names = TRUE)
  file.copy(font_files, temp_dir, overwrite = TRUE)
  
  # Change working directory to temp (important for rendering)
  old_wd <- setwd(temp_dir)
  on.exit(setwd(old_wd), add = TRUE)
  # browser()
  
  # df_clean <- saved_results()
  # df_clean$`measurement_unit` <- gsub("%", "\\\\%", df_clean$`measurement_unit`, fixed = TRUE)
  
  
  # Render the PDF in the temp directory
  rmarkdown::render(
    input = "report.Rmd",
    output_format = "pdf_document",
    output_file = "report.pdf",  # just a filename
    params = list(
      name = input$prepared_by,
      project_name = input$project_name,
      project_date = format(input$date, "%d-%m-%Y"),
      proposal_overview = input$proposal_overview,
      ecological_context = input$ecological_context,
      biodiversity_impacts = input$biodiversity_impacts,
      offset_package = input$offset_package,
      saved_results_df = saved_results(),
      summary_results_df = compute_summary(saved_results(), draw_store),
      long_report = long_report_flag
    ),
    envir = new.env(parent = globalenv())
  )
  
  # Copy the output file to the Shiny download target
  file.copy(file.path(temp_dir, "report.pdf"), file)
}

edit_saved_row <- function(row, session, numeric_ids, saved_results, 
                           edit_row_idx, editing) {
  # Clear all red outlines first
  session$sendCustomMessage("clearAllInvalidInputs", numeric_ids)
  
  df <- saved_results()
  if (is.na(row) || row < 1 || row > nrow(df)) return(NULL)
  
  # 1) mark edit state + highlight
  edit_row_idx(row)
  editing(TRUE)
  
  # 2) repopulate inputs from that row
  row_list <- as.list(df[row, , drop = FALSE])
  restore_inputs_from_list(session, row_list)
  
  # 3) scroll to top
  session$sendCustomMessage("scrollToTop", "now")
  
  # 4) tweak button labels
  updateActionButton(session, "calculate",      label = "Re-Run Simulations")
  updateActionButton(session, "save_and_reset", label = "Update Entry In Report")
  
  # 5) enable save button
  enable("save_and_reset")
}


# ══════════════════════════════════════════════════════════════════════════════
# Excel Upload Functions (multi-sheet with join)
# ══════════════════════════════════════════════════════════════════════════════

# ── Column label-to-ID mappings for each sheet ───────────────────────────────

xlsx_biodiversity_columns <- function() {
  c(
    "biodiversity_type"      = "Biodiversity Type",
    "biodiversity_component" = "Biodiversity Component",
    "biodiversity_attribute" = "Biodiversity Attribute",
    "measurement_unit"       = "Measurement Unit",
    "benchmark_value"        = "Benchmark Value",
    "distribution"           = "Statistical Distribution"
  )
}

xlsx_impact_columns <- function() {
  c(
    "biodiversity_type"        = "Biodiversity Type",
    "biodiversity_component"   = "Biodiversity Component",
    "biodiversity_attribute"   = "Biodiversity Attribute",
    "impact_area"              = "Impact Area",
    "impact_area_unit"         = "Impact Area Unit",
    "prior_impact_data_type"   = "Prior Impact – Attribute Measure Data Type",
    "mx_prior_impact_mean"     = "Mean Attribute Measure Prior to Impact",
    "mx_prior_impact_sd"       = "NORMAL: SD Prior to Impact",
    "impact_sample_size_prior" = "POISSON: Sample Size Prior to Impact",
    "mx_prior_impact_var"      = "NEG BINOMIAL: Variance Prior to Impact",
    "post_impact_data_type"    = "Post Impact – Attribute Measure Data Type",
    "mx_post_impact_mean"      = "Mean Attribute Measure Post Impact",
    "mx_post_impact_sd"        = "NORMAL: SD Post Impact",
    "impact_sample_size_post"  = "POISSON: Sample Size Post Impact",
    "mx_post_impact_var"       = "NEG BINOMIAL: Variance Post Impact"
  )
}

xlsx_offset_columns <- function() {
  c(
    "biodiversity_type"        = "Biodiversity Type",
    "biodiversity_component"   = "Biodiversity Component",
    "biodiversity_attribute"   = "Biodiversity Attribute",
    "offset_area"              = "Offset Area",
    "offset_area_unit"         = "Offset Area Unit",
    "offset_actions_text"      = "Proposed Offset Actions",
    "prior_offset_data_type"   = "Prior Offset – Attribute Measure Data Type",
    "mx_prior_offset_mean"     = "Mean Attribute Measure Prior to Offset",
    "mx_prior_offset_sd"       = "NORMAL: SD Prior to Offset",
    "offset_sample_size_prior" = "POISSON: Sample Size Prior to Offset",
    "mx_prior_offset_var"      = "NEG BINOMIAL: Variance Prior to Offset",
    "post_offset_data_type"    = "Post Offset – Attribute Measure Data Type",
    "mx_post_offset_mean"      = "Mean Attribute Measure Post Offset",
    "mx_post_offset_sd"        = "NORMAL: SD Post Offset",
    "offset_sample_size_post"  = "POISSON: Sample Size Post Offset",
    "mx_post_offset_var"       = "NEG BINOMIAL: Variance Post Offset",
    "selected_confidence"      = "Confidence in Proposed Offset Action",
    "time_till_end"            = "Time till End (Years)",
    "discount_rate"            = "Discount Rate (%)"
  )
}

xlsx_doc_attr_columns <- function() {
  c(
    "biodiversity_type"                = "Biodiversity Type",
    "biodiversity_component"           = "Biodiversity Component",
    "biodiversity_attribute"           = "Biodiversity Attribute",
    "prior_impact_empirical_details"   = "EMPIRICAL Attribute Measure Prior to Impact – Provide sample size & duration",
    "prior_impact_modelled_details"    = "MODELLED Attribute Measure Prior to Impact – Describe the model",
    "prior_impact_expert_affiliations" = "EXPERT ELICITED Attribute Measure Prior to Impact – List expert(s) and affiliations",
    "prior_impact_proxy_details"       = "PROXY VALUE Attribute Measure Prior to Impact – Give reference and/or describe",
    "post_impact_empirical_details"    = "EMPIRICAL Attribute Measure Post Impact – Provide sample size & duration",
    "post_impact_modelled_details"     = "MODELLED Attribute Measure Post Impact – Describe the model",
    "post_impact_expert_affiliations"  = "EXPERT ELICITED Attribute Measure Post Impact – List expert(s) and affiliations",
    "post_impact_proxy_details"        = "PROXY VALUE Attribute Measure Post Impact – Give reference and/or describe",
    "prior_offset_empirical_details"   = "EMPIRICAL Attribute Measure Prior to Offset – Provide sample size & duration",
    "prior_offset_modelled_details"    = "MODELLED Attribute Measure Prior to Offset – Describe the model",
    "prior_offset_expert_affiliations" = "EXPERT ELICITED Attribute Measure Prior to Offset – List expert(s) and affiliations",
    "prior_offset_proxy_details"       = "PROXY VALUE Attribute Measure Prior to Offset – Give reference and/or describe",
    "post_offset_empirical_details"    = "EMPIRICAL Attribute Measure Post Offset – Provide sample size & duration",
    "post_offset_modelled_details"     = "MODELLED Attribute Measure Post Offset – Describe the model",
    "post_offset_expert_affiliations"  = "EXPERT ELICITED Attribute Measure Post Offset – List expert(s) and affiliations",
    "post_offset_proxy_details"        = "PROXY VALUE Attribute Measure Post Offset – Give reference and/or describe"
  )
}

xlsx_doc_other_columns <- function() {
  c(
    "biodiversity_type"            = "Biodiversity Type",
    "biodiversity_component"       = "Biodiversity Component",
    "biodiversity_attribute"       = "Biodiversity Attribute",
    "benchmark_justify"            = "Justify Benchmark Value",
    "offset_confidence_justify"    = "Justify Confidence in Proposed Offset Action",
    "offset_time_till_end_justify" = "Justify Time till End",
    "offset_discount_rate_justify" = "Justify Discount Rate"
  )
}

# ── Helper: rename columns from human labels to internal IDs ─────────────────
rename_to_ids <- function(df, col_map) {
  label_to_id <- setNames(names(col_map), unname(col_map))
  incoming <- colnames(df)
  for (j in seq_along(incoming)) {
    # read.xlsx(sep.names = " ") leaves XML entities (e.g. "&amp;") undecoded
    nm <- gsub("&amp;", "&", incoming[j], fixed = TRUE)
    if (nm %in% names(label_to_id)) {
      incoming[j] <- label_to_id[[nm]]
    }
  }
  colnames(df) <- incoming
  df
}

# Join key columns used across all sheets
join_keys <- c("biodiversity_type", "biodiversity_component", "biodiversity_attribute")

# ── Validate an uploaded multi-sheet Excel file ──────────────────────────────
# Returns a list with:
#   $valid      — logical
#   $errors     — data.frame(Row, Sheet, Field, Message)
#   $project    — named list of project detail values
#   $attributes — joined data.frame with internal column names, ready for simulation
validate_xlsx_upload <- function(filepath) {
  require(openxlsx)

  # Check the file is a readable .xlsx before doing anything else.
  # This catches files saved in the wrong format (e.g. .xls, .csv) even if
  # they have been renamed to .xlsx, which openxlsx cannot read.
  sheet_names <- tryCatch(getSheetNames(filepath), error = function(e) NULL)
  if (is.null(sheet_names)) {
    return(list(
      valid = FALSE,
      errors = data.frame(
        Row = NA, Sheet = "File", Field = "Format",
        Message = paste(
          "The file could not be opened as an Excel (.xlsx) file.",
          "This usually happens when the file has been saved in a different format.",
          "Please re-save it from Excel using 'Save As' \u2192 'Excel Workbook (.xlsx)' and try again."
        ),
        stringsAsFactors = FALSE
      ),
      project = list(),
      attributes = data.frame()
    ))
  }

  errors <- data.frame(Row = integer(), Sheet = character(), Field = character(),
                        Message = character(), stringsAsFactors = FALSE)

  add_err <- function(row, sheet, field, msg) {
    errors <<- rbind(errors, data.frame(Row = row, Sheet = sheet, Field = field,
                                         Message = msg, stringsAsFactors = FALSE))
  }

  # --- Read Project Details ---
  proj_sheet <- tryCatch(read.xlsx(filepath, sheet = "Project Details", colNames = TRUE, sep.names = " "),
                          error = function(e) NULL)
  if (is.null(proj_sheet)) {
    add_err(NA, "Project Details", "Sheet", "Missing 'Project Details' sheet")
  }

  project <- list()
  if (!is.null(proj_sheet) && nrow(proj_sheet) >= 1) {
    proj_ids    <- c("project_name", "prepared_by", "date",
                      "proposal_overview", "ecological_context",
                      "biodiversity_impacts", "offset_package")
    proj_labels <- c("Project Name", "Prepared By", "Date (dd/mm/yyyy)",
                      "Summary of Proposal Overview", "Ecological Context and Impact Summary",
                      "Biodiversity Impacts Addressed Outside of the Model",
                      "Summary Description of Proposed Offset Package")
    for (i in seq_along(proj_labels)) {
      match_row <- which(proj_sheet[[1]] == proj_labels[i])
      if (length(match_row) == 1 && ncol(proj_sheet) >= 2) {
        project[[proj_ids[i]]] <- as.character(proj_sheet[match_row, 2])
      } else {
        project[[proj_ids[i]]] <- ""
      }
    }
  }

  # --- Read Biodiversity ---
  bio_df <- tryCatch(read.xlsx(filepath, sheet = "Biodiversity", colNames = TRUE, sep.names = " "),
                      error = function(e) NULL)
  if (is.null(bio_df)) {
    add_err(NA, "Biodiversity", "Sheet", "Missing 'Biodiversity' sheet")
    return(list(valid = FALSE, errors = errors, project = project, attributes = data.frame()))
  }
  if (nrow(bio_df) == 0) {
    add_err(NA, "Biodiversity", "Sheet", "No data rows")
    return(list(valid = FALSE, errors = errors, project = project, attributes = data.frame()))
  }
  bio_df <- rename_to_ids(bio_df, xlsx_biodiversity_columns())

  # --- Read Impact Site ---
  impact_df <- tryCatch(read.xlsx(filepath, sheet = "Impact Site", colNames = TRUE, sep.names = " "),
                         error = function(e) NULL)
  if (is.null(impact_df)) {
    add_err(NA, "Impact Site", "Sheet", "Missing 'Impact Site' sheet")
    return(list(valid = FALSE, errors = errors, project = project, attributes = data.frame()))
  }
  if (nrow(impact_df) == 0) {
    add_err(NA, "Impact Site", "Sheet", "No data rows")
    return(list(valid = FALSE, errors = errors, project = project, attributes = data.frame()))
  }
  impact_df <- rename_to_ids(impact_df, xlsx_impact_columns())

  # --- Join Biodiversity fields (distribution, measurement_unit, benchmark_value) onto Impact rows ---
  bio_data <- bio_df[, setdiff(colnames(bio_df), join_keys), drop = FALSE]
  bio_keys <- paste(bio_df$biodiversity_type, bio_df$biodiversity_component,
                     bio_df$biodiversity_attribute, sep = "|||")
  impact_keys_pre <- paste(impact_df$biodiversity_type, impact_df$biodiversity_component,
                            impact_df$biodiversity_attribute, sep = "|||")
  bio_order <- match(impact_keys_pre, bio_keys)
  impact_df <- cbind(impact_df, bio_data[bio_order, , drop = FALSE])

  # --- Read Offset Site ---
  offset_df <- tryCatch(read.xlsx(filepath, sheet = "Offset Site", colNames = TRUE, sep.names = " "),
                         error = function(e) NULL)
  if (is.null(offset_df)) {
    add_err(NA, "Offset Site", "Sheet", "Missing 'Offset Site' sheet")
    return(list(valid = FALSE, errors = errors, project = project, attributes = data.frame()))
  }
  if (nrow(offset_df) == 0) {
    add_err(NA, "Offset Site", "Sheet", "No data rows")
    return(list(valid = FALSE, errors = errors, project = project, attributes = data.frame()))
  }
  offset_df <- rename_to_ids(offset_df, xlsx_offset_columns())

  # --- Read Documentation sheets (optional) ---
  doc_attr_df <- tryCatch(read.xlsx(filepath, sheet = "Documentation -AttributeMeasure", colNames = TRUE, sep.names = " "),
                           error = function(e) NULL)
  if (!is.null(doc_attr_df) && nrow(doc_attr_df) > 0) {
    doc_attr_df <- rename_to_ids(doc_attr_df, xlsx_doc_attr_columns())
  } else {
    doc_attr_df <- NULL
  }

  doc_other_df <- tryCatch(read.xlsx(filepath, sheet = "Documentation – Other ", colNames = TRUE, sep.names = " "),
                            error = function(e) NULL)
  if (!is.null(doc_other_df) && nrow(doc_other_df) > 0) {
    doc_other_df <- rename_to_ids(doc_other_df, xlsx_doc_other_columns())
  } else {
    doc_other_df <- NULL
  }

  # --- Validate Impact rows ---
  impact_required <- c("biodiversity_type", "biodiversity_component",
                        "biodiversity_attribute", "measurement_unit",
                        "benchmark_value", "distribution",
                        "impact_area", "impact_area_unit")

  for (i in seq_len(nrow(impact_df))) {
    row <- impact_df[i, , drop = FALSE]

    # Required fields
    for (fld in impact_required) {
      val <- row[[fld]]
      if (is.null(val) || is.na(val) || trimws(as.character(val)) == "") {
        add_err(i, "Impact", fld, paste0("Required field '", fld, "' is missing"))
      }
    }

    # Numeric checks
    for (fld in c("benchmark_value", "impact_area")) {
      val <- row[[fld]]
      if (!is.null(val) && !is.na(val) && is.na(suppressWarnings(as.numeric(val)))) {
        add_err(i, "Impact", fld, paste0("'", fld, "' must be a number"))
      }
    }

    # Distribution validation
    dist <- as.character(row$distribution)
    if (!is.na(dist) && !(dist %in% c("Normal", "Poisson", "Negative Binomial"))) {
      add_err(i, "Impact", "distribution", paste0("Invalid distribution: '", dist, "'"))
    }

    # Reject Expert elicited
    for (dt_fld in c("prior_impact_data_type", "post_impact_data_type")) {
      dt_val <- as.character(row[[dt_fld]])
      if (!is.na(dt_val) && dt_val == "Expert elicited") {
        add_err(i, "Impact", dt_fld,
                "Expert elicited data type is not supported via spreadsheet. Use the app GUI.")
      }
    }

    # Measurement point validation (prior + post impact)
    point_configs <- list(
      list(dt = "prior_impact_data_type", mean = "mx_prior_impact_mean",
           sd = "mx_prior_impact_sd", n = "impact_sample_size_prior",
           var = "mx_prior_impact_var"),
      list(dt = "post_impact_data_type", mean = "mx_post_impact_mean",
           sd = "mx_post_impact_sd", n = "impact_sample_size_post",
           var = "mx_post_impact_var")
    )

    for (pc in point_configs) {
      data_type <- as.character(row[[pc$dt]])
      if (!is.na(data_type) && data_type == "Expert elicited") next

      # Mean required
      mean_val <- row[[pc$mean]]
      if (is.null(mean_val) || is.na(mean_val) ||
          is.na(suppressWarnings(as.numeric(mean_val)))) {
        add_err(i, "Impact", pc$mean, paste0("'", pc$mean, "' is required"))
      }

      # Distribution-specific secondary
      if (!is.na(dist)) {
        if (dist == "Normal" || dist == "") {
          sv <- row[[pc$sd]]
          if (is.null(sv) || is.na(sv) || is.na(suppressWarnings(as.numeric(sv)))) {
            add_err(i, "Impact", pc$sd, paste0("'", pc$sd, "' required for Normal"))
          }
        } else if (dist == "Poisson") {
          nv <- row[[pc$n]]
          if (is.null(nv) || is.na(nv) || is.na(suppressWarnings(as.numeric(nv)))) {
            add_err(i, "Impact", pc$n, paste0("'", pc$n, "' required for Poisson"))
          }
        } else if (dist == "Negative Binomial") {
          vv <- row[[pc$var]]
          if (is.null(vv) || is.na(vv) || is.na(suppressWarnings(as.numeric(vv)))) {
            add_err(i, "Impact", pc$var, paste0("'", pc$var, "' required for Neg Binom"))
          }
        }
      }
    }
  }

  # --- Validate Offset rows ---
  offset_required <- c("biodiversity_type", "biodiversity_component",
                         "biodiversity_attribute",
                         "offset_area", "offset_area_unit",
                         "selected_confidence", "time_till_end", "discount_rate")

  for (i in seq_len(nrow(offset_df))) {
    row <- offset_df[i, , drop = FALSE]

    for (fld in offset_required) {
      val <- row[[fld]]
      if (is.null(val) || is.na(val) || trimws(as.character(val)) == "") {
        add_err(i, "Offset", fld, paste0("Required field '", fld, "' is missing"))
      }
    }

    for (fld in c("offset_area", "time_till_end", "discount_rate")) {
      val <- row[[fld]]
      if (!is.null(val) && !is.na(val) && is.na(suppressWarnings(as.numeric(val)))) {
        add_err(i, "Offset", fld, paste0("'", fld, "' must be a number"))
      }
    }

    # Reject Expert elicited
    for (dt_fld in c("prior_offset_data_type", "post_offset_data_type")) {
      dt_val <- as.character(row[[dt_fld]])
      if (!is.na(dt_val) && dt_val == "Expert elicited") {
        add_err(i, "Offset", dt_fld,
                "Expert elicited data type is not supported via spreadsheet. Use the app GUI.")
      }
    }

    # Need the distribution from the matching impact row to validate secondary params
    imp_key <- paste(row$biodiversity_type, row$biodiversity_component,
                      row$biodiversity_attribute, sep = "|||")
    imp_match <- which(paste(impact_df$biodiversity_type, impact_df$biodiversity_component,
                              impact_df$biodiversity_attribute, sep = "|||") == imp_key)
    dist <- if (length(imp_match) == 1) as.character(impact_df$distribution[imp_match]) else NA_character_

    point_configs <- list(
      list(dt = "prior_offset_data_type", mean = "mx_prior_offset_mean",
           sd = "mx_prior_offset_sd", n = "offset_sample_size_prior",
           var = "mx_prior_offset_var"),
      list(dt = "post_offset_data_type", mean = "mx_post_offset_mean",
           sd = "mx_post_offset_sd", n = "offset_sample_size_post",
           var = "mx_post_offset_var")
    )

    for (pc in point_configs) {
      data_type <- as.character(row[[pc$dt]])
      if (!is.na(data_type) && data_type == "Expert elicited") next

      mean_val <- row[[pc$mean]]
      if (is.null(mean_val) || is.na(mean_val) ||
          is.na(suppressWarnings(as.numeric(mean_val)))) {
        add_err(i, "Offset", pc$mean, paste0("'", pc$mean, "' is required"))
      }

      if (!is.na(dist)) {
        if (dist == "Normal" || dist == "") {
          sv <- row[[pc$sd]]
          if (is.null(sv) || is.na(sv) || is.na(suppressWarnings(as.numeric(sv)))) {
            add_err(i, "Offset", pc$sd, paste0("'", pc$sd, "' required for Normal"))
          }
        } else if (dist == "Poisson") {
          nv <- row[[pc$n]]
          if (is.null(nv) || is.na(nv) || is.na(suppressWarnings(as.numeric(nv)))) {
            add_err(i, "Offset", pc$n, paste0("'", pc$n, "' required for Poisson"))
          }
        } else if (dist == "Negative Binomial") {
          vv <- row[[pc$var]]
          if (is.null(vv) || is.na(vv) || is.na(suppressWarnings(as.numeric(vv)))) {
            add_err(i, "Offset", pc$var, paste0("'", pc$var, "' required for Neg Binom"))
          }
        }
      }
    }
  }

  # --- Cross-sheet join validation ---
  impact_keys <- paste(impact_df$biodiversity_type, impact_df$biodiversity_component,
                        impact_df$biodiversity_attribute, sep = "|||")
  offset_keys <- paste(offset_df$biodiversity_type, offset_df$biodiversity_component,
                        offset_df$biodiversity_attribute, sep = "|||")

  # Impact rows without matching offset
  unmatched_impact <- which(!(impact_keys %in% offset_keys))
  for (idx in unmatched_impact) {
    add_err(idx, "Impact", "join_key",
            paste0("No matching Offset row for: ",
                   impact_df$biodiversity_type[idx], " / ",
                   impact_df$biodiversity_component[idx], " / ",
                   impact_df$biodiversity_attribute[idx]))
  }

  # Offset rows without matching impact
  unmatched_offset <- which(!(offset_keys %in% impact_keys))
  for (idx in unmatched_offset) {
    add_err(idx, "Offset", "join_key",
            paste0("No matching Impact row for: ",
                   offset_df$biodiversity_type[idx], " / ",
                   offset_df$biodiversity_component[idx], " / ",
                   offset_df$biodiversity_attribute[idx]))
  }

  # --- Join sheets into a single attributes data.frame ---
  # Remove join key columns from offset to avoid duplication
  offset_data <- offset_df[, setdiff(colnames(offset_df), join_keys), drop = FALSE]
  # Bind by position (rows are matched by key order)
  # First reorder offset to match impact key order
  offset_order <- match(impact_keys, offset_keys)

  if (any(is.na(offset_order))) {
    # Can't join — unmatched rows exist; return errors
    return(list(valid = FALSE, errors = errors, project = project,
                attributes = data.frame()))
  }

  offset_data <- offset_data[offset_order, , drop = FALSE]
  joined <- cbind(impact_df, offset_data)

  # Merge in documentation if present
  if (!is.null(doc_attr_df) && nrow(doc_attr_df) > 0) {
    doc_data <- doc_attr_df[, setdiff(colnames(doc_attr_df), join_keys), drop = FALSE]
    doc_keys <- paste(doc_attr_df$biodiversity_type, doc_attr_df$biodiversity_component,
                       doc_attr_df$biodiversity_attribute, sep = "|||")
    doc_order <- match(impact_keys, doc_keys)
    # Only merge rows that matched; fill NA for unmatched
    if (any(!is.na(doc_order))) {
      doc_aligned <- doc_data[doc_order, , drop = FALSE]
      joined <- cbind(joined, doc_aligned)
    }
  }

  if (!is.null(doc_other_df) && nrow(doc_other_df) > 0) {
    doc_data <- doc_other_df[, setdiff(colnames(doc_other_df), join_keys), drop = FALSE]
    doc_keys <- paste(doc_other_df$biodiversity_type, doc_other_df$biodiversity_component,
                       doc_other_df$biodiversity_attribute, sep = "|||")
    doc_order <- match(impact_keys, doc_keys)
    if (any(!is.na(doc_order))) {
      doc_aligned <- doc_data[doc_order, , drop = FALSE]
      joined <- cbind(joined, doc_aligned)
    }
  }

  # --- Coerce numeric columns ---
  numeric_fields <- c("benchmark_value", "impact_area", "offset_area",
                       "mx_prior_impact_mean", "mx_prior_impact_sd",
                       "impact_sample_size_prior", "mx_prior_impact_var",
                       "mx_post_impact_mean", "mx_post_impact_sd",
                       "impact_sample_size_post", "mx_post_impact_var",
                       "mx_prior_offset_mean", "mx_prior_offset_sd",
                       "offset_sample_size_prior", "mx_prior_offset_var",
                       "mx_post_offset_mean", "mx_post_offset_sd",
                       "offset_sample_size_post", "mx_post_offset_var",
                       "time_till_end", "discount_rate")
  for (fld in numeric_fields) {
    if (fld %in% colnames(joined)) {
      joined[[fld]] <- suppressWarnings(as.numeric(joined[[fld]]))
    }
  }

  list(
    valid      = nrow(errors) == 0,
    errors     = errors,
    project    = project,
    attributes = joined
  )
}


# ── Process a validated upload: simulate all rows, populate saved_results ────
process_xlsx_upload <- function(validation_result, session, results,
                                 saved_results, summary_results,
                                 draw_store, confidence_levels) {

  proj <- validation_result$project
  attrs <- validation_result$attributes

  # --- Restore Project Details into the UI ---
  updateTextInput(session, "project_name", value = proj$project_name %||% "")
  updateTextInput(session, "prepared_by",  value = proj$prepared_by %||% "")
  if (!is.null(proj$date) && !is.na(proj$date) && nchar(proj$date) > 0) {
    suppressWarnings(updateDateInput(session, "date",
                                     value = as.Date(proj$date, format = "%d/%m/%Y")))
  }
  updateTextAreaInput(session, "proposal_overview",    value = proj$proposal_overview %||% "")
  updateTextAreaInput(session, "ecological_context",   value = proj$ecological_context %||% "")
  updateTextAreaInput(session, "biodiversity_impacts", value = proj$biodiversity_impacts %||% "")
  updateTextAreaInput(session, "offset_package",       value = proj$offset_package %||% "")

  # --- Clear existing data ---
  saved_results(data.frame(stringsAsFactors = FALSE))
  if (!is.null(draw_store)) {
    for (nm in names(draw_store)) draw_store[[nm]] <- NULL
  }

  # --- Simulate each row and build saved_results ---
  all_rows <- vector("list", nrow(attrs))

  for (i in seq_len(nrow(attrs))) {
    row_list <- as.list(attrs[i, , drop = FALSE])

    # Run simulation
    sim <- tryCatch(
      resimulate_row(row_list, confidence_levels),
      error = function(e) {
        showNotification(paste0("Simulation failed for row ", i, ": ", e$message),
                         type = "warning", duration = 5)
        NULL
      }
    )

    if (is.null(sim)) next

    # Compute summary stats
    stats <- list(
      Impact_Mean = round(mean(sim$impact), 2),
      Impact_P5   = round(quantile(sim$impact, 0.05), 2),
      Impact_P95  = round(quantile(sim$impact, 0.95), 2),
      Offset_Mean = round(mean(sim$offset), 2),
      Offset_P5   = round(quantile(sim$offset, 0.05), 2),
      Offset_P95  = round(quantile(sim$offset, 0.95), 2),
      NPBV_Mean   = round(mean(sim$npbv), 2),
      NPBV_P5     = round(quantile(sim$npbv, 0.05), 2),
      NPBV_P95    = round(quantile(sim$npbv, 0.95), 2)
    )

    all_rows[[i]] <- as.data.frame(
      c(row_list, stats),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    # Stash draws
    if (!is.null(draw_store)) {
      draw_store[[paste0("row_", i)]] <- sim
    }
  }

  # Combine all rows
  all_rows <- Filter(Negate(is.null), all_rows)
  if (length(all_rows) > 0) {
    df <- dplyr::bind_rows(all_rows)
    saved_results(df)
    summary_results(compute_summary(saved_results(), draw_store))
  }

  # Clear current simulation vectors and reset form
  results$impact <- results$offset <- results$npbv <- NULL
  reset_project_calc_inputs(session)
  disable("save_and_reset")

  showNotification(
    paste0("Successfully imported ", length(all_rows), " attribute(s) from Excel."),
    type = "message", duration = 5
  )
}