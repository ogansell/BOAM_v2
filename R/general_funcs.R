show_startup_modal <- function() {
  showModal(modalDialog(
    title = "About",
    HTML("
      <h4>FIRST CUT SUGGESTED ALTERNATIVE TEXT [Fleur]</h4>

      <p><strong>ABOUT</strong></p>

      <p>This net gain model evaluates offset proposals for ecological equivalence and net gain across biodiversity type, amount, and time, based on user inputs, and can be used to simulate numerous biodiversity offset scenarios to compare predicted outcomes.</p>

      <p>The model uses Net Present Biodiversity Value (NPBV) to indicate predicted outcomes from offset proposals, coupled with confidence levels to describe certainty associated with the model outputs.</p>

      <p>The NPBV does not quantify predicted net gain outcomes. Outputs from this model should be further interpreted to meaningful ecological 'on-the-ground' measures to support quantified (e.g., 10%) net gain claims.</p>

      <ul>
        <li>Enter ecological data for impact and offset sites</li>
        <li>Enter proposed offset measures and predicted outcome values</li>
        <li>Run simulations with confidence levels</li>
        <li>Save and export model output summaries</li>
      </ul>

      <p><strong>The system will time out after 60 minutes of inactivity.</strong></p>
      <p><strong>Use the Save Draft feature regularly to ensure work is not lost.</strong></p>

      <br>

      <p>For assistance and questions please contact:<br>
      Jane Doe<br>
      <a href='mailto:jane.doe@example.com'>jane.doe@example.com</a><br>
      Phone: +64 21 123 4567</p>
    "),
    easyClose = TRUE,
    footer = modalButton("Continue")
  ))
}

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

import_draft_file <- function(file_input, session, results,
                              saved_results, summary_results,
                              compute_summary,
                              draw_store = NULL, confidence_levels = NULL) {
  
  req(file_input$datapath)
  txt <- readLines(file_input$datapath, warn = FALSE, encoding = "UTF-8")
  dat <- tryCatch(jsonlite::fromJSON(paste(txt, collapse = "\n")), error = function(e) NULL)
  req(!is.null(dat))
  
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
  
  # --- Biodiversity ---
  updateTextInput(session, "biodiversity_type",        value = getOr(ins, "biodiversity_type", ""))
  updateTextInput(session, "biodiversity_component",   value = getOr(ins, "biodiversity_component", ""))
  updateTextInput(session, "biodiversity_attribute",   value = getOr(ins, "biodiversity_attribute", ""))
  updateTextInput(session, "measurement_unit",         value = getOr(ins, "measurement_unit", ""))
  updateNumericInput(session, "benchmark_value",       value = getOr(ins, "benchmark_value", 30))
  updateSelectInput(session,  "distribution",          selected = getOr(ins, "distribution", "Normal"))
  updateNumericInput(session, "num_simulations",       value = getOr(ins, "num_simulations", 100))
  
  # --- Impact ---
  updateSelectInput(session,  "impact_area_unit",        selected = getOr(ins, "impact_area_unit", "m"))
  updateNumericInput(session, "impact_area",             value = getOr(ins, "impact_area", 0.35))
  updateSelectInput(session,  "impact_area_data_type",   selected = getOr(ins, "impact_area_data_type", "Empirical"))
  updateTextAreaInput(session, "impact_area_empirical_details", value = getOr(ins, "impact_area_empirical_details", ""))
  updateTextAreaInput(session, "impact_area_modelled_details",  value = getOr(ins, "impact_area_modelled_details", ""))
  updateTextAreaInput(session, "impact_area_expert_details",    value = getOr(ins, "impact_area_expert_details", ""))
  updateTextAreaInput(session, "impact_area_proxy_details",     value = getOr(ins, "impact_area_proxy_details", ""))
  
  updateNumericInput(session, "mx_prior_impact_mean",    value = getOr(ins, "mx_prior_impact_mean", 30))
  updateNumericInput(session, "mx_prior_impact_sd",      value = getOr(ins, "mx_prior_impact_sd", 5))
  updateSelectInput(session,  "prior_impact_sd_data_type", selected = getOr(ins, "prior_impact_sd_data_type", "Empirical"))
  updateTextAreaInput(session, "prior_impact_sd_empirical_details", value = getOr(ins, "prior_impact_sd_empirical_details", ""))
  updateTextAreaInput(session, "prior_impact_sd_modelled_details",  value = getOr(ins, "prior_impact_sd_modelled_details", ""))
  updateTextAreaInput(session, "prior_impact_sd_expert_details",    value = getOr(ins, "prior_impact_sd_expert_details", ""))
  updateTextAreaInput(session, "prior_impact_sd_proxy_details",     value = getOr(ins, "prior_impact_sd_proxy_details", ""))
  
  updateNumericInput(session, "mx_post_impact_mean",     value = getOr(ins, "mx_post_impact_mean", 0))
  updateNumericInput(session, "mx_post_impact_sd",       value = getOr(ins, "mx_post_impact_sd", 5))
  
  updateSelectInput(session,  "post_impact_sd_data_type", selected = getOr(ins, "post_impact_sd_data_type", "Empirical"))
  updateTextAreaInput(session, "post_impact_sd_empirical_details", value = getOr(ins, "post_impact_sd_empirical_details", ""))
  updateTextAreaInput(session, "post_impact_sd_modelled_details",  value = getOr(ins, "post_impact_sd_modelled_details", ""))
  updateTextAreaInput(session, "post_impact_sd_expert_details",    value = getOr(ins, "post_impact_sd_expert_details", ""))
  updateTextAreaInput(session, "post_impact_sd_proxy_details",     value = getOr(ins, "post_impact_sd_proxy_details", ""))
  
  # --- Offset ---
  updateSelectInput(session,  "offset_area_unit",       selected = getOr(ins, "offset_area_unit", "m"))
  updateNumericInput(session, "offset_area",             value = getOr(ins, "offset_area", 1))
  updateNumericInput(session, "mx_prior_offset_mean",    value = getOr(ins, "mx_prior_offset_mean", 0))
  updateNumericInput(session, "mx_prior_offset_sd",      value = getOr(ins, "mx_prior_offset_sd", 5))
  updateSelectInput(session,  "prior_offset_sd_data_type", selected = getOr(ins, "prior_offset_sd_data_type", "Empirical"))
  updateTextAreaInput(session, "prior_offset_sd_empirical_details", value = getOr(ins, "prior_offset_sd_empirical_details", ""))
  updateTextAreaInput(session, "prior_offset_sd_modelled_details",  value = getOr(ins, "prior_offset_sd_modelled_details", ""))
  updateTextAreaInput(session, "prior_offset_sd_expert_details",    value = getOr(ins, "prior_offset_sd_expert_details", ""))
  updateTextAreaInput(session, "prior_offset_sd_proxy_details",     value = getOr(ins, "prior_offset_sd_proxy_details", ""))
  
  updateNumericInput(session, "mx_post_offset_mean",     value = getOr(ins, "mx_post_offset_mean", 30))
  updateNumericInput(session, "mx_post_offset_sd",       value = getOr(ins, "mx_post_offset_sd", 5))
  
  updateSelectInput(session,  "selected_confidence",     selected = getOr(ins, "selected_confidence", "Confident 75-90%"))
  updateTextAreaInput(session, "offset_confidence_justify", value = getOr(ins, "offset_confidence_justify", ""))
  
  updateNumericInput(session, "time_till_end",           value = getOr(ins, "time_till_end", 1))
  updateTextAreaInput(session, "offset_time_till_end_justify", value = getOr(ins, "offset_time_till_end_justify", ""))
  
  updateNumericInput(session, "discount_rate",           value = getOr(ins, "discount_rate", 3))
  updateTextAreaInput(session, "offset_discount_rate_justify", value = getOr(ins, "offset_discount_rate_justify", ""))
  
  # --- Restore table from draft ---
  if (!is.null(dat$saved_results)) {
    df <- tryCatch(as.data.frame(dat$saved_results, stringsAsFactors = FALSE), error = function(e) NULL)
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
# Excel Upload / Template Functions
# ══════════════════════════════════════════════════════════════════════════════

# ── Human-readable column labels for the Attributes sheet ────────────────────
# Maps internal field IDs to user-friendly headers. Order here determines
# column order in the template.
xlsx_attribute_columns <- function() {
  c(
    # Biodiversity identity
    "biodiversity_type"          = "Biodiversity Type",
    "biodiversity_component"     = "Biodiversity Component",
    "biodiversity_attribute"     = "Biodiversity Attribute",
    "measurement_unit"           = "Measurement Unit",
    "benchmark_value"            = "Benchmark Value",
    "distribution"               = "Statistical Distribution",

    # Impact Area
    "impact_area"                = "Impact Area",
    "impact_area_unit"           = "Impact Area Unit",
    "impact_area_data_type"      = "Impact Area Data Type",
    "impact_area_empirical_details"  = "Impact Area Empirical Details",
    "impact_area_modelled_details"   = "Impact Area Modelled Details",
    "impact_area_expert_details"     = "Impact Area Expert Details",
    "impact_area_proxy_details"      = "Impact Area Proxy Details",

    # Prior Impact
    "prior_impact_data_type"     = "Prior Impact Data Type",
    "mx_prior_impact_mean"       = "Prior Impact Mean",
    "mx_prior_impact_sd"         = "Prior Impact SD",
    "impact_sample_size_prior"   = "Prior Impact Sample Size (Poisson)",
    "mx_prior_impact_var"        = "Prior Impact Variance (Neg Binom)",
    "prior_impact_empirical_details"     = "Prior Impact Empirical Details",
    "prior_impact_modelled_details"      = "Prior Impact Modelled Details",
    "prior_impact_expert_affiliations"   = "Prior Impact Expert Affiliations",
    "prior_impact_proxy_details"         = "Prior Impact Proxy Details",
    "prior_impact_p_low"         = "Prior Impact SHELF Lower Bound",
    "prior_impact_p50"           = "Prior Impact SHELF Median",
    "prior_impact_p_high"        = "Prior Impact SHELF Upper Bound",
    "prior_impact_ci_level"      = "Prior Impact SHELF CI Level",

    # Post Impact
    "post_impact_data_type"      = "Post Impact Data Type",
    "mx_post_impact_mean"        = "Post Impact Mean",
    "mx_post_impact_sd"          = "Post Impact SD",
    "impact_sample_size_post"    = "Post Impact Sample Size (Poisson)",
    "mx_post_impact_var"         = "Post Impact Variance (Neg Binom)",
    "post_impact_empirical_details"      = "Post Impact Empirical Details",
    "post_impact_modelled_details"       = "Post Impact Modelled Details",
    "post_impact_expert_affiliations"    = "Post Impact Expert Affiliations",
    "post_impact_proxy_details"          = "Post Impact Proxy Details",
    "post_impact_p_low"          = "Post Impact SHELF Lower Bound",
    "post_impact_p50"            = "Post Impact SHELF Median",
    "post_impact_p_high"         = "Post Impact SHELF Upper Bound",
    "post_impact_ci_level"       = "Post Impact SHELF CI Level",

    # Offset Area
    "offset_area"                = "Offset Area",
    "offset_area_unit"           = "Offset Area Unit",

    # Prior Offset
    "prior_offset_data_type"     = "Prior Offset Data Type",
    "mx_prior_offset_mean"       = "Prior Offset Mean",
    "mx_prior_offset_sd"         = "Prior Offset SD",
    "offset_sample_size_prior"   = "Prior Offset Sample Size (Poisson)",
    "mx_prior_offset_var"        = "Prior Offset Variance (Neg Binom)",
    "prior_offset_empirical_details"     = "Prior Offset Empirical Details",
    "prior_offset_modelled_details"      = "Prior Offset Modelled Details",
    "prior_offset_expert_affiliations"   = "Prior Offset Expert Affiliations",
    "prior_offset_proxy_details"         = "Prior Offset Proxy Details",
    "prior_offset_p_low"         = "Prior Offset SHELF Lower Bound",
    "prior_offset_p50"           = "Prior Offset SHELF Median",
    "prior_offset_p_high"        = "Prior Offset SHELF Upper Bound",
    "prior_offset_ci_level"      = "Prior Offset SHELF CI Level",

    # Post Offset
    "post_offset_data_type"      = "Post Offset Data Type",
    "mx_post_offset_mean"        = "Post Offset Mean",
    "mx_post_offset_sd"          = "Post Offset SD",
    "offset_sample_size_post"    = "Post Offset Sample Size (Poisson)",
    "mx_post_offset_var"         = "Post Offset Variance (Neg Binom)",
    "post_offset_empirical_details"      = "Post Offset Empirical Details",
    "post_offset_modelled_details"       = "Post Offset Modelled Details",
    "post_offset_expert_affiliations"    = "Post Offset Expert Affiliations",
    "post_offset_proxy_details"          = "Post Offset Proxy Details",
    "post_offset_p_low"          = "Post Offset SHELF Lower Bound",
    "post_offset_p50"            = "Post Offset SHELF Median",
    "post_offset_p_high"         = "Post Offset SHELF Upper Bound",
    "post_offset_ci_level"       = "Post Offset SHELF CI Level",

    # Offset confidence / time / discount
    "selected_confidence"        = "Confidence in Offset Action",
    "offset_confidence_justify"  = "Justify Confidence",
    "time_till_end"              = "Time Till End (Years)",
    "offset_time_till_end_justify" = "Justify Time Till End",
    "discount_rate"              = "Discount Rate (%)",
    "offset_discount_rate_justify" = "Justify Discount Rate"
  )
}

# ── Generate the Excel template workbook ─────────────────────────────────────
generate_xlsx_template <- function(file) {
  require(openxlsx)

  wb <- createWorkbook()

  # --- Sheet 1: Project Details ---
  addWorksheet(wb, "Project Details")
  proj_headers <- c("Project Name", "Prepared By", "Date (dd-mm-yyyy)",
                     "Proposal Overview", "Ecological Context",
                     "Biodiversity Impacts", "Offset Package")
  proj_ids     <- c("project_name", "prepared_by", "date",
                     "proposal_overview", "ecological_context",
                     "biodiversity_impacts", "offset_package")

  writeData(wb, "Project Details", data.frame(Field = proj_headers, Value = ""),
            startRow = 1, startCol = 1)

  # Style the header row
  header_style <- createStyle(textDecoration = "bold", fgFill = "#D5E8D4", border = "Bottom")
  addStyle(wb, "Project Details", header_style, rows = 1, cols = 1:2)
  setColWidths(wb, "Project Details", cols = 1, widths = 30)
  setColWidths(wb, "Project Details", cols = 2, widths = 80)

  # --- Sheet 2: Attributes ---
  addWorksheet(wb, "Attributes")
  attr_cols <- xlsx_attribute_columns()
  attr_labels <- unname(attr_cols)

  # Write header row with human-readable labels
  header_df <- as.data.frame(matrix(nrow = 0, ncol = length(attr_labels)))
  colnames(header_df) <- attr_labels
  writeData(wb, "Attributes", header_df, startRow = 1)

  # Style header
  attr_header_style <- createStyle(textDecoration = "bold", fgFill = "#D5E8D4",
                                    border = "Bottom", wrapText = TRUE)
  addStyle(wb, "Attributes", attr_header_style,
           rows = 1, cols = seq_along(attr_labels), gridExpand = TRUE)
  setColWidths(wb, "Attributes", cols = seq_along(attr_labels), widths = 22)

  # Add data validation for dropdown fields
  # Distribution
  dist_col <- which(attr_labels == "Statistical Distribution")
  dataValidation(wb, "Attributes", col = dist_col, rows = 2:100,
                 type = "list", value = '"Normal,Poisson,Negative Binomial"')

  # Data type columns
  data_type_labels <- c("Impact Area Data Type", "Prior Impact Data Type",
                         "Post Impact Data Type", "Prior Offset Data Type",
                         "Post Offset Data Type")
  for (lbl in data_type_labels) {
    dt_col <- which(attr_labels == lbl)
    if (length(dt_col) == 1) {
      dataValidation(wb, "Attributes", col = dt_col, rows = 2:100,
                     type = "list",
                     value = '"Empirical,Modelled,Expert elicited,Proxy value"')
    }
  }

  # Confidence
  conf_col <- which(attr_labels == "Confidence in Offset Action")
  dataValidation(wb, "Attributes", col = conf_col, rows = 2:100,
                 type = "list",
                 value = '"Low confidence >50% <75%,Confident 75-90%,Very confident >90%"')

  # Area unit columns
  area_unit_labels <- c("Impact Area Unit", "Offset Area Unit")
  for (lbl in area_unit_labels) {
    au_col <- which(attr_labels == lbl)
    if (length(au_col) == 1) {
      dataValidation(wb, "Attributes", col = au_col, rows = 2:100,
                     type = "list", value = '"km,m,ha,m\u00B2"')
    }
  }

  # SHELF CI level columns
  ci_labels <- c("Prior Impact SHELF CI Level", "Post Impact SHELF CI Level",
                  "Prior Offset SHELF CI Level", "Post Offset SHELF CI Level")
  for (lbl in ci_labels) {
    ci_col <- which(attr_labels == lbl)
    if (length(ci_col) == 1) {
      dataValidation(wb, "Attributes", col = ci_col, rows = 2:100,
                     type = "list", value = '"0.90,0.80,0.50"')
    }
  }

  # --- Sheet 3: Instructions ---
  addWorksheet(wb, "Instructions")
  instructions <- data.frame(
    Instruction = c(
      "1. Fill in the 'Project Details' sheet with project-level information.",
      "2. Fill in the 'Attributes' sheet with one row per biodiversity attribute.",
      "3. Each row must have: Biodiversity Type, Component, Attribute, Measurement Unit, Benchmark Value, Distribution.",
      "4. Each row must have impact and offset measurement data appropriate to the chosen distribution.",
      "5. For Normal distribution: provide Mean and SD for each measurement point.",
      "6. For Poisson distribution: provide Mean and Sample Size for each measurement point.",
      "7. For Negative Binomial distribution: provide Mean and Variance for each measurement point.",
      "8. For Expert elicited data type: provide SHELF Lower Bound, Median, Upper Bound, and CI Level.",
      "9. Dropdown validation is provided for Distribution, Data Type, Confidence, Area Unit, and SHELF CI Level.",
      "10. Leave distribution-specific fields blank if not applicable (e.g. leave Poisson Sample Size blank if using Normal).",
      "11. Upload the completed file using the 'Upload Data' button on the Project Calculations tab."
    ),
    stringsAsFactors = FALSE
  )
  writeData(wb, "Instructions", instructions)
  setColWidths(wb, "Instructions", cols = 1, widths = 100)

  saveWorkbook(wb, file, overwrite = TRUE)
}


# ── Validate an uploaded Excel file ──────────────────────────────────────────
# Returns a list with:
#   $valid    — logical, TRUE if all rows pass
#   $errors   — data.frame with columns: Row, Field, Message
#   $project  — named list of project detail values
#   $attributes — data.frame with internal column names, ready for simulation
validate_xlsx_upload <- function(filepath) {
  require(openxlsx)

  errors <- data.frame(Row = integer(), Field = character(),
                        Message = character(), stringsAsFactors = FALSE)

  # --- Read Project Details ---
  proj_sheet <- tryCatch(read.xlsx(filepath, sheet = "Project Details",
                                    colNames = TRUE),
                          error = function(e) NULL)
  if (is.null(proj_sheet)) {
    errors <- rbind(errors, data.frame(Row = NA, Field = "Sheet",
                                        Message = "Missing 'Project Details' sheet"))
  }

  project <- list()
  if (!is.null(proj_sheet) && nrow(proj_sheet) >= 1) {
    proj_ids <- c("project_name", "prepared_by", "date",
                   "proposal_overview", "ecological_context",
                   "biodiversity_impacts", "offset_package")
    proj_labels <- c("Project Name", "Prepared By", "Date (dd-mm-yyyy)",
                      "Proposal Overview", "Ecological Context",
                      "Biodiversity Impacts", "Offset Package")

    for (i in seq_along(proj_labels)) {
      match_row <- which(proj_sheet[[1]] == proj_labels[i])
      if (length(match_row) == 1 && ncol(proj_sheet) >= 2) {
        project[[proj_ids[i]]] <- as.character(proj_sheet[match_row, 2])
      } else {
        project[[proj_ids[i]]] <- ""
      }
    }
  }

  # --- Read Attributes ---
  attr_sheet <- tryCatch(read.xlsx(filepath, sheet = "Attributes",
                                    colNames = TRUE),
                          error = function(e) NULL)
  if (is.null(attr_sheet)) {
    errors <- rbind(errors, data.frame(Row = NA, Field = "Sheet",
                                        Message = "Missing 'Attributes' sheet"))
    return(list(valid = FALSE, errors = errors, project = project,
                attributes = data.frame()))
  }

  if (nrow(attr_sheet) == 0) {
    errors <- rbind(errors, data.frame(Row = NA, Field = "Sheet",
                                        Message = "'Attributes' sheet has no data rows"))
    return(list(valid = FALSE, errors = errors, project = project,
                attributes = data.frame()))
  }

  # Map human-readable column names back to internal IDs
  attr_col_map <- xlsx_attribute_columns()
  label_to_id <- setNames(names(attr_col_map), unname(attr_col_map))

  # Rename columns from labels to IDs
  incoming_names <- colnames(attr_sheet)
  mapped_names <- character(length(incoming_names))
  for (j in seq_along(incoming_names)) {
    if (incoming_names[j] %in% names(label_to_id)) {
      mapped_names[j] <- label_to_id[[incoming_names[j]]]
    } else {
      mapped_names[j] <- incoming_names[j]  # keep as-is if unrecognised
    }
  }
  colnames(attr_sheet) <- mapped_names

  # --- Validate each row ---
  required_always <- c("biodiversity_type", "biodiversity_component",
                        "biodiversity_attribute", "measurement_unit",
                        "benchmark_value", "distribution",
                        "impact_area", "offset_area",
                        "selected_confidence", "time_till_end", "discount_rate")

  for (i in seq_len(nrow(attr_sheet))) {
    row <- attr_sheet[i, , drop = FALSE]

    # Check always-required fields
    for (fld in required_always) {
      val <- row[[fld]]
      if (is.null(val) || is.na(val) || trimws(as.character(val)) == "") {
        errors <- rbind(errors, data.frame(
          Row = i, Field = fld,
          Message = paste0("Required field '", fld, "' is missing")))
      }
    }

    # Check numeric fields are numeric
    numeric_always <- c("benchmark_value", "impact_area", "offset_area",
                         "time_till_end", "discount_rate")
    for (fld in numeric_always) {
      val <- row[[fld]]
      if (!is.null(val) && !is.na(val) && is.na(suppressWarnings(as.numeric(val)))) {
        errors <- rbind(errors, data.frame(
          Row = i, Field = fld,
          Message = paste0("'", fld, "' must be a number")))
      }
    }

    # Distribution validation
    dist <- as.character(row$distribution)
    if (!is.na(dist) && !(dist %in% c("Normal", "Poisson", "Negative Binomial"))) {
      errors <- rbind(errors, data.frame(
        Row = i, Field = "distribution",
        Message = paste0("Invalid distribution: '", dist, "'")))
    }

    # Validate measurement points based on distribution and data type
    point_configs <- list(
      list(prefix = "prior_impact", dt_field = "prior_impact_data_type",
           mean_field = "mx_prior_impact_mean", sd_field = "mx_prior_impact_sd",
           n_field = "impact_sample_size_prior", var_field = "mx_prior_impact_var",
           shelf_fields = c("prior_impact_p_low", "prior_impact_p50", "prior_impact_p_high")),
      list(prefix = "post_impact", dt_field = "post_impact_data_type",
           mean_field = "mx_post_impact_mean", sd_field = "mx_post_impact_sd",
           n_field = "impact_sample_size_post", var_field = "mx_post_impact_var",
           shelf_fields = c("post_impact_p_low", "post_impact_p50", "post_impact_p_high")),
      list(prefix = "prior_offset", dt_field = "prior_offset_data_type",
           mean_field = "mx_prior_offset_mean", sd_field = "mx_prior_offset_sd",
           n_field = "offset_sample_size_prior", var_field = "mx_prior_offset_var",
           shelf_fields = c("prior_offset_p_low", "prior_offset_p50", "prior_offset_p_high")),
      list(prefix = "post_offset", dt_field = "post_offset_data_type",
           mean_field = "mx_post_offset_mean", sd_field = "mx_post_offset_sd",
           n_field = "offset_sample_size_post", var_field = "mx_post_offset_var",
           shelf_fields = c("post_offset_p_low", "post_offset_p50", "post_offset_p_high"))
    )

    for (pc in point_configs) {
      data_type <- as.character(row[[pc$dt_field]])

      if (!is.na(data_type) && data_type == "Expert elicited") {
        # SHELF fields required
        for (sf in pc$shelf_fields) {
          val <- row[[sf]]
          if (is.null(val) || is.na(val) || is.na(suppressWarnings(as.numeric(val)))) {
            errors <- rbind(errors, data.frame(
              Row = i, Field = sf,
              Message = paste0("SHELF field '", sf, "' required for Expert elicited data")))
          }
        }
      } else if (!is.na(dist)) {
        # Mean always required for non-expert
        mean_val <- row[[pc$mean_field]]
        if (is.null(mean_val) || is.na(mean_val) ||
            is.na(suppressWarnings(as.numeric(mean_val)))) {
          errors <- rbind(errors, data.frame(
            Row = i, Field = pc$mean_field,
            Message = paste0("'", pc$mean_field, "' is required")))
        }

        # Distribution-specific secondary param
        if (dist == "Normal" || dist == "") {
          sd_val <- row[[pc$sd_field]]
          if (is.null(sd_val) || is.na(sd_val) ||
              is.na(suppressWarnings(as.numeric(sd_val)))) {
            errors <- rbind(errors, data.frame(
              Row = i, Field = pc$sd_field,
              Message = paste0("'", pc$sd_field, "' required for Normal distribution")))
          }
        } else if (dist == "Poisson") {
          n_val <- row[[pc$n_field]]
          if (is.null(n_val) || is.na(n_val) ||
              is.na(suppressWarnings(as.numeric(n_val)))) {
            errors <- rbind(errors, data.frame(
              Row = i, Field = pc$n_field,
              Message = paste0("'", pc$n_field, "' required for Poisson distribution")))
          }
        } else if (dist == "Negative Binomial") {
          var_val <- row[[pc$var_field]]
          if (is.null(var_val) || is.na(var_val) ||
              is.na(suppressWarnings(as.numeric(var_val)))) {
            errors <- rbind(errors, data.frame(
              Row = i, Field = pc$var_field,
              Message = paste0("'", pc$var_field, "' required for Negative Binomial")))
          }
        }
      }
    }

    # Cross-validate: benchmark_value, distribution, measurement_unit should be
    # consistent within a biodiversity_component (warn, not error)
  }

  # Coerce numeric columns
  numeric_fields <- c("benchmark_value", "impact_area", "offset_area",
                       "mx_prior_impact_mean", "mx_prior_impact_sd",
                       "impact_sample_size_prior", "mx_prior_impact_var",
                       "prior_impact_p_low", "prior_impact_p50", "prior_impact_p_high",
                       "mx_post_impact_mean", "mx_post_impact_sd",
                       "impact_sample_size_post", "mx_post_impact_var",
                       "post_impact_p_low", "post_impact_p50", "post_impact_p_high",
                       "mx_prior_offset_mean", "mx_prior_offset_sd",
                       "offset_sample_size_prior", "mx_prior_offset_var",
                       "prior_offset_p_low", "prior_offset_p50", "prior_offset_p_high",
                       "mx_post_offset_mean", "mx_post_offset_sd",
                       "offset_sample_size_post", "mx_post_offset_var",
                       "post_offset_p_low", "post_offset_p50", "post_offset_p_high",
                       "time_till_end", "discount_rate")
  for (fld in numeric_fields) {
    if (fld %in% colnames(attr_sheet)) {
      attr_sheet[[fld]] <- suppressWarnings(as.numeric(attr_sheet[[fld]]))
    }
  }

  list(
    valid      = nrow(errors) == 0,
    errors     = errors,
    project    = project,
    attributes = attr_sheet
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
  if (!is.null(proj$date) && nchar(proj$date) > 0) {
    suppressWarnings(updateDateInput(session, "date", value = as.Date(proj$date, format = "%d-%m-%Y")))
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