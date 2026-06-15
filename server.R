
# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # start buttons disabld
  disable("save_and_reset")

  # Keepalive: receive periodic ping from client to reset shinyapps.io idle timer
  observeEvent(input$keepalive_ping, {
    # no-op: exists only to receive the keepalive ping and reset the idle timer
  }, ignoreInit = TRUE)

  
  # * define reactives ----------------------------------------------------------
  
  # results reactive
  results <- reactiveValues(impact = NULL, offset = NULL, npbv = NULL)
  
  # saved results reactive
  saved_results <- reactiveVal(data.frame(stringsAsFactors = FALSE))
  
  # draw store: holds MC draw vectors keyed by row index (ephemeral, not persisted)
  draw_store <- reactiveValues()
  
  # summary results reactive 
  summary_results <- reactiveVal()
    
  # message store for the summary UI (used by the summaries below)
  sim_status <- reactiveVal(NULL)
  editing <- reactiveVal(FALSE)        # TRUE while editing a row
  edit_row_idx <- reactiveVal(NULL)    # 1-based row index being edited
  
  # * SHELF live-derived displays -----------------------------------------------
  # Helper: render derived mean+SD from SHELF inputs in real time
  render_shelf_display <- function(p_low_id, p50_id, p_high_id, ci_id) {
    renderUI({
      p_low  <- input[[p_low_id]]
      p50    <- input[[p50_id]]
      p_high <- input[[p_high_id]]
      ci     <- input[[ci_id]]
      if (any(is.null(c(p_low, p50, p_high, ci))) ||
          any(is.na(c(p_low, p50, p_high)))) {
        return(tags$em(style="color:#999;font-size:0.82em;", "Enter all three percentiles to see derived values."))
      }
      res <- tryCatch(shelf_to_mean_sd(p_low, p50, p_high, ci), error = function(e) NULL)
      if (is.null(res)) return(NULL)
      tags$div(
        style = "background:#fffde7;border:1px solid #f9a825;border-radius:3px;padding:6px 10px;font-size:0.83em;",
        HTML(sprintf(
          "<strong>Derived:</strong> &nbsp; Mean = <strong>%.3f</strong> &nbsp;|&nbsp; SD = <strong>%.3f</strong> &nbsp;
           <span style='color:#777;'>(CI level: %s%%)</span>",
          res$mean, res$sd, round(as.numeric(ci) * 100)
        ))
      )
    })
  }

  output$prior_impact_shelf_derived  <- render_shelf_display("prior_impact_p_low",  "prior_impact_p50",  "prior_impact_p_high",  "prior_impact_ci_level")
  output$post_impact_shelf_derived   <- render_shelf_display("post_impact_p_low",   "post_impact_p50",   "post_impact_p_high",   "post_impact_ci_level")
  output$prior_offset_shelf_derived  <- render_shelf_display("prior_offset_p_low",  "prior_offset_p50",  "prior_offset_p_high",  "prior_offset_ci_level")
  output$post_offset_shelf_derived   <- render_shelf_display("post_offset_p_low",   "post_offset_p50",   "post_offset_p_high",   "post_offset_ci_level")

  # New report confirm modal
  observeEvent(input$new_report_btn, {                                       
    showModal(new_report_modal())
  })                                                                          
  
  # Perform full browser reload (new session)
  observeEvent(input$confirm_new, {
    removeModal()
    session$sendCustomMessage("clearAutosave", list())
    shinyjs::runjs("location.reload(true);")
  })

  # * autosave (browser localStorage) --------------------------------------

  autosave_data <- reactiveVal(NULL)

  # On page load, the client checks localStorage for an autosaved draft
  # and reports it here so we can offer to restore it.
  observeEvent(input$autosave_check, {
    found <- input$autosave_check
    if (!is.null(found) && !is.null(found$data)) {
      autosave_data(found$data)
      showModal(restore_autosave_modal(found$ts))
    }
  }, once = TRUE)

  observeEvent(input$restore_autosave, {
    removeModal()
    dat <- autosave_data()
    if (!is.null(dat)) {
      restore_draft_data(dat, session, results, saved_results, summary_results,
                         compute_summary, draw_store, confidence_levels)
    }
    autosave_data(NULL)
  })

  observeEvent(input$discard_autosave, {
    removeModal()
    session$sendCustomMessage("clearAutosave", list())
    autosave_data(NULL)
  })

  # Idle reminder: show a "still there?" modal after IDLE_MODAL_MIN of inactivity
  observeEvent(input$idle_warn, {
    showModal(idle_modal())
  })

  observeEvent(input$dismiss_idle, {
    removeModal()
  })

  observeEvent(input$stay_active, {
    removeModal()
  })

  #hide the saved results table if there are no rows
  output$saved_rows <- reactive({
    nrow(saved_results())
  })
  outputOptions(output, "saved_rows", suspendWhenHidden = FALSE)
  
  
  observeEvent(input$go_to_inputs, {
    updateTabsetPanel(session, "main_tabs", selected = "Project Calculations")
    session$sendCustomMessage("scrollToTop", "now")
  })
  
  observeEvent(input$go_to_export, {
    updateTabsetPanel(session, "main_tabs", selected = "Export Report")
    session$sendCustomMessage("scrollToTop", "now")
  })
  
  # * calculate button ----------------------------------------------------------

  observeEvent(input$calculate, {
    validate_and_run_simulation(input, results, numeric_ids, session, confidence_levels)
  })
  
  # * save and reset button ----------------------------------------------------------
  
  observeEvent(input$save_and_reset, {
    save_and_reset_logic(
      input, results, project_calc_fields,
      saved_results, editing, edit_row_idx,
      summary_results, session, draw_store
    )

    # Autosave the draft to browser localStorage after each completed attribute
    payload <- build_draft_payload(input, draft_fields, saved_results)
    session$sendCustomMessage("autosaveDraft", payload)
  })
  
  # * model output summary ----------------------------------------------------------
  ## _impact summary ----------------------------------------------------------
  
  output$impact_summary <- renderUI({
    msg <- sim_status(); if (!is.null(msg)) return(span(style="color:red;", msg))
    if (is.null(results$impact)) return(span(style="color:red;","Run simulations to see results."))
    span(style="color:blue;",
         sprintf("Impact: %s",
                 fmt_pi(mean(results$impact), quantile(results$impact, 0.05), quantile(results$impact, 0.95))))
  })
  
  ## _ offset summary ----------------------------------------------------------
  
  output$offset_summary <- renderUI({
    if (is.null(results$offset)) return(span(style="color:red;"," "))
    span(style="color:blue;",
         sprintf("Offset: %s",
                 fmt_pi(mean(results$offset), quantile(results$offset, 0.05), quantile(results$offset, 0.95))))
  })
  
  ## _ npbv summary ----------------------------------------------------------
  
  output$npbv_summary <- renderUI({
    if (is.null(results$npbv)) return(span(style="color:red;"," "))
    span(style="color:blue;",
         sprintf("NPBV: %s",
                 fmt_pi(mean(results$npbv), quantile(results$npbv, 0.05), quantile(results$npbv, 0.95))))
  })
  

## _sync units --------------------------------------------------------------

  observeEvent(input$impact_area_unit, {
    req(input$impact_area_unit)  # ignore NULL
    
    updateSelectInput(
      session,
      "offset_area_unit",
      selected = input$impact_area_unit
    )
  })
  
  # * render full table ----------------------------------------------------------

  output$saved_table <- renderDT({
    
    df_prepared <- prepare_saved_results_df(saved_results())
    
    make_saved_results_datatable(
      df = df_prepared,
      edit_row_idx = edit_row_idx()
    )
    
  }, server = FALSE)
  
  # * delete row ----------------------------------------------------------
  
  observeEvent(input$delete_row, {
    df <- saved_results()
    row <- as.numeric(input$delete_row)
    if (!is.na(row) && row <= nrow(df)) {
      old_nrow <- nrow(df)
      df <- df[-row, ]
      saved_results(df)

      # Reindex draw_store to match shifted row positions
      reindex_draw_store(draw_store, row, old_nrow)

      # If we just deleted the row being edited, exit edit mode and reset UI
      if (isTRUE(editing()) && !is.null(edit_row_idx()) && row == edit_row_idx()) {
        editing(FALSE)
        edit_row_idx(NULL)
        updateActionButton(session, "calculate",      label = "Run Simulations")
        updateActionButton(session, "save_and_reset", label = "Save to Report & Reset")
        # cancel button hides automatically because output$cancel_edit_ui depends on editing()
      }
      
      summary_results(compute_summary(saved_results(), draw_store))
      print_saved_results(saved_results(), title = "SAVED RESULTS TABLE AFTER DELETE")
    }
  })
  
  # * edit row ----------------------------------------------------------
  
  observeEvent(input$edit_row, {
    edit_saved_row(
      row = as.integer(input$edit_row),
      session = session,
      numeric_ids = numeric_ids,
      saved_results = saved_results,
      edit_row_idx = edit_row_idx,
      editing = editing
    )
  })

  output$cancel_edit_ui <- renderUI({
    if (!editing()) return(NULL)
    actionButton("cancel_edit", "Cancel Current Edit", class = "btn btn-secondary")
  })
  
  
  observeEvent(input$cancel_edit, {
    editing(FALSE)
    edit_row_idx(NULL)
    updateActionButton(session, "calculate",      label = "Run Simulations")
    updateActionButton(session, "save_and_reset", label = "Save to Report & Reset")
  })
  
  # * render summary table ----------------------------------------------------------
  
  output$summary_table <- renderDT({
    df <- saved_results()
    summary_df <- compute_summary_df(df, fmt_pi, npbv_colorize, draw_store)
    summary_results(summary_df)  # store summary reactively if needed
    make_summary_datatable(summary_df)
  }, server = FALSE)
  
  # * download pdf ----------------------------------------------------------
  
  # Short report
  output$download_report_pdf <- downloadHandler(
    filename = function() {
      paste0("report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      render_report(input, saved_results, summary_results, file,
                    long_report_flag = FALSE, draw_store = draw_store)
    }
  )
  
  # Long report
  output$download_long_report_pdf <- downloadHandler(
    filename = function() {
      paste0("long_report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      render_report(input, saved_results, summary_results, file,
                    long_report_flag = TRUE, draw_store = draw_store)
    }
  )
  
  # ---- SAVE: download JSON with inputs + saved_results ----
  output$draft_export <- downloadHandler(
    filename = function() {
      paste0("biodiversity_draft_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".json")
    },
    content = function(file) {
      payload <- build_draft_payload(input, draft_fields, saved_results)
      writeLines(jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE, na = "null"), file, useBytes = TRUE)
    }
  )
  
  # ---- LOAD: read JSON and push values back into the UI + table ----
  
  observeEvent(input$draft_import_file, {
    import_draft_file(input$draft_import_file, session, results,
                      saved_results, summary_results, compute_summary,
                      draw_store, confidence_levels)
  })
  
  # ---- EXCEL: download template (static file from www/) ----
  output$xlsx_template_download <- downloadHandler(
    filename = function() {
      "Net_Gain_Data_Template_v2.xlsx"
    },
    content = function(file) {
      file.copy("www/Net_Gain_Data_Template_v2.xlsx", file)
    }
  )

  # ---- EXCEL: upload and validate ----
  # Reactive to hold validation result for UI display
  xlsx_validation <- reactiveVal(NULL)

  observeEvent(input$xlsx_upload_file, {
    req(input$xlsx_upload_file$datapath)

    # Validate
    result <- tryCatch(
      validate_xlsx_upload(input$xlsx_upload_file$datapath),
      error = function(e) {
        list(valid = FALSE,
             errors = data.frame(Row = NA, Sheet = "File", Field = "File",
                                  Message = paste("Error reading file:", e$message),
                                  stringsAsFactors = FALSE),
             project = list(),
             attributes = data.frame())
      }
    )

    if (result$valid) {
      # Process the upload — replaces existing data
      process_xlsx_upload(result, session, results,
                          saved_results, summary_results,
                          draw_store, confidence_levels)
      xlsx_validation(NULL)  # clear any previous validation errors
    } else {
      # Store validation result so the UI can display errors
      xlsx_validation(result)
      showNotification("Validation errors found — see details below the upload button.",
                       type = "error", duration = 8)
    }
  })

  # ---- EXCEL: validation results UI ----
  output$xlsx_validation_ui <- renderUI({
    vr <- xlsx_validation()
    if (is.null(vr)) return(NULL)

    err <- vr$errors
    if (nrow(err) == 0) return(NULL)

    # Build an error summary table
    err_display <- err
    err_display$Row <- ifelse(is.na(err_display$Row), "-", as.character(err_display$Row))

    tags$div(
      style = "margin-top: 12px; padding: 10px; background: #fff3f3; border: 1px solid #e57373; border-radius: 4px;",
      tags$p(style = "font-weight: 600; color: #c62828; margin-bottom: 6px;",
             icon("exclamation-triangle"),
             sprintf(" %d validation error(s) found:", nrow(err))),
      tags$div(
        style = "max-height: 300px; overflow-y: auto;",
        tags$table(
          class = "table table-sm table-bordered",
          style = "font-size: 0.82em; margin-bottom: 0;",
          tags$thead(
            tags$tr(
              tags$th("Row"), tags$th("Sheet"), tags$th("Field"), tags$th("Message")
            )
          ),
          tags$tbody(
            lapply(seq_len(nrow(err_display)), function(j) {
              tags$tr(
                tags$td(err_display$Row[j]),
                tags$td(err_display$Sheet[j]),
                tags$td(err_display$Field[j]),
                tags$td(err_display$Message[j])
              )
            })
          )
        )
      ),
      tags$p(style = "font-size: 0.82em; color: #777; margin-top: 6px;",
             "Fix the errors in your spreadsheet and re-upload.")
    )
  })

  
}