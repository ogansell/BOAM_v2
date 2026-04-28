# Load Required Packages --------------------------------------------------
library(shiny)
library(dplyr)
library(bslib)
library(rmarkdown)
library(lorem)
library(DT)
library(shinyjs)
library(kableExtra)

addResourcePath("custom", "www")
# === Idle reminder config (minutes) ============================================
IDLE_MODAL_MIN <- 2
# ==============================================================================


# Confidence Levels -------------------------------------------------------
confidence_levels <- data.frame(
  levels = c("Low confidence >50% <75%", "Confident 75-90%", "Very confident >90%"),
  scores = c(0.62, 0.825, 0.955)
)

# Custom Theme ------------------------------------------------------------
custom_theme <- bs_theme(
  bg = "#F5F4F2",
  fg = "#384246",
  primary = "#FFC51D",
  secondary = "#194036",
  # base_font = font_google("Lato", local = TRUE),
  btn_bg = "#E0DCCE"
)

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  theme = custom_theme,
  useShinyjs(),
  tags$head(
    
    tags$link(href = "https://fonts.googleapis.com/css2?family=Lato&display=swap", rel = "stylesheet"),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$style(HTML("body { font-family: 'Lato', sans-serif; }")),
            
    
  # Scripts ----------------------------------------------------------------------   
    tags$script(HTML("
  $(function() {
    const tooltips = {
   project_name: 'Hover over text inserted here - 1',
      prepared_by: 'Hover over text inserted here - 2',
      date: 'Hover over text inserted here - 3',
      proposal_overview: 'Copy and Paste your Proposal Overview here',
      ecological_context: 'Copy and Paste your Ecological Context here',
      offset_package: 'Copy and Paste your Offset here',
      biodiversity_type: 'Hover over text inserted here - 7',
      biodiversity_component: 'Hover over text inserted here - 8',
      biodiversity_attribute: 'Hover over text inserted here - 9',
      measurement_unit: 'Hover over text inserted here - 10',
      num_simulations: 'Hover over text inserted here - 11',
      selected_confidence: 'Hover over text inserted here - 12',
      impact_area: 'Hover over text inserted here - 13',
      offset_area: 'Hover over text inserted here - 14',
      mx_prior_impact_mean: 'Hover over text inserted here - 15',
      mx_prior_offset_mean: 'Hover over text inserted here - 16',
      mx_prior_impact_sd: 'Hover over text inserted here - 17',
      mx_prior_offset_sd: 'Hover over text inserted here - 18',
      mx_post_impact_mean: 'Hover over text inserted here - 19',
      mx_post_offset_mean: 'Hover over text inserted here - 20',
      mx_post_impact_sd: 'Hover over text inserted here - 21',
      mx_post_offset_sd: 'Hover over text inserted here - 22',
      benchmark_value: 'Hover over text inserted here - 23',
      time_till_end: 'Hover over text inserted here - 24',
      discount_rate: 'Hover over text inserted here - 25',
      distribution: 'Hover over text inserted here - 26'
      
        };

        Object.entries(tooltips).forEach(([id, text]) => {
          $('#' + id).popover({
            trigger: 'hover',
            placement: 'right',
            content: text,
            container: 'body'
          });
        });
    $('#header_bar').on('click', function() {
      Shiny.setInputValue('show_header_modal', Math.random());
    });
  });
   Shiny.addCustomMessageHandler('scrollToBottom', function(message) {
    setTimeout(function() {
      window.scrollTo({ top: document.body.scrollHeight, behavior: 'smooth' });
    }, 300); // delay to ensure DOM update
  });
  
  Shiny.addCustomMessageHandler('scrollToTop', function(message) {
  setTimeout(function() {
    window.scrollTo({ top: 0, behavior: 'smooth' });
  }, 300); // delay to ensure DOM has updated
});


  document.addEventListener('DOMContentLoaded', function(){
    var btn = document.getElementById('draft_import_btn');
    var file = document.querySelector('input[type=file][id$=\"draft_import_file\"]');
    if(btn && file){ btn.addEventListener('click', function(){ file.click(); }); }
  });
  
  
                     "))
  ),
  


tags$script(HTML(sprintf("
    // sandbox-style config (seconds). We set both to the same so warn == show.
    window.APP_CFG = { IDLE_LIMIT_SEC: %d, IDLE_WARN_AT_SEC: %d };
  ", IDLE_MODAL_MIN*60, IDLE_MODAL_MIN*60)))
  ,
  
  tags$script(HTML("
  (function(){
    var LIMIT   = (window.APP_CFG && window.APP_CFG.IDLE_LIMIT_SEC)   || 60;
    var WARN_AT = (window.APP_CFG && window.APP_CFG.IDLE_WARN_AT_SEC) || LIMIT;
    var remaining = LIMIT, warned = false;

    function activityReset(e){
      if (e && e.isTrusted === false) return;      // ignore synthetic events
      // ignore clicks on Save Draft so you can still see the modal if you want
      if (e && e.type === 'click') {
        var t = e.target;
        if (t && t.closest && t.closest('#draft_export')) return;
      }
       remaining = LIMIT;
      // only dismiss the idle modal if we had shown it
      if (warned && window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue('dismiss_idle', { ts: Date.now() }, {priority:'event'});
      }
      warned = false;
    }

    document.addEventListener('DOMContentLoaded', function(){
      // countdown tick
      setInterval(function(){
        remaining = Math.max(0, remaining - 1);

        // show warning once we cross WARN_AT (== LIMIT in our config)
       if (remaining === 0 && !warned) {
  warned = true;
  if (window.Shiny && Shiny.setInputValue) {
    Shiny.setInputValue('idle_warn', { ts: Date.now() }, {priority:'event'});
  }
}
        // optional: when it hits 0, keep sending the warn once per minute
        if (remaining === 0) {
          // keep the timer at 0 until there is activity
          remaining = 0;
        }
      }, 1000);

      // real user activity that clears the warning and resets the timer
      ['keydown','click','touchstart','pointerdown'].forEach(function(ev){
        window.addEventListener(ev, activityReset, {passive:true});
      });
      document.addEventListener('shiny:inputchanged', activityReset);
    });
  })();
"))
,


# hidden file input (kept outside the header, stays invisible)
tags$div(style = "display:none;",
         fileInput("draft_import_file", "Choose JSON", accept = ".json")
),
  
div(
  id = "sticky_header",
  div(class = "top-stripe"),
  div(
    id = "header_bar",
    class = "title-bar",
    div(id = "header_text", "Biodiversity Offsetting", style = "flex: 1"),
    div(
      style = "display:flex; gap:20px; align-items:center; margin-right:8px;",
      downloadButton("draft_export", "Save Draft", class = "btn btn-dark-green"),
      actionButton("draft_import_btn", "Load Draft", class = "btn-dark-green", icon = icon("upload")),
      actionButton("new_report_btn", "New", class = "btn btn-dark-green", icon = icon("file"))   
      ),
    img(id = "header_logo", src = "Logo.png", height = "70px", width = "70px", style = "margin-left: auto;")
  )
)
,
  
  div(class = "container-fluid",
      tabsetPanel(id = "main_tabs",
                  
                  # * Project Details UI ------------------------------------------------------

                  tabPanel("Project Details",
                           div(class = "form-section",
                               fluidRow(
                                 column(12, textInput("project_name", label = "Project Name", placeholder = "Enter project name", width = "100%")),
                                 column(8, textInput("prepared_by", "Prepared By", placeholder = "Enter your name", width = "100%")),
                                 column(4, dateInput("date", "Date", Sys.Date(), format = "dd-mm-yyyy", width = "100%"))
                               )
                           ),
                           div(class = "form-section",
                               textAreaInput("proposal_overview", "Proposal Overview", value = lorem::ipsum(3), rows = 6, width = "100%"),
                               tags$br(),
                               textAreaInput("ecological_context", "Ecological Context", value = lorem::ipsum(3), rows = 6, width = "100%"),
                               tags$br(),
                               textAreaInput("offset_package", "Proposed Offset Package", value = lorem::ipsum(3), rows = 6, width = "100%")
                           ),
                           div(style = "text-align: center;",
                               actionButton("go_to_inputs", "Proceed to Project Calculations >", class = "btn btn-dark-green", style = "margin-bottom: 20px;")
                           )
                  ),
                  
                  # * Project Calculations UI ------------------------------------------------------
                  
                  tabPanel("Project Calculations",
                           
                           # __Biodiversity ----------------------------------------------------------
                           div(class = "colored-box red-box form-section-box",
                               div(class = "box-header", "Biodiversity"),
                               div(class = "compact-grid",
                                   div(class = "input-block", tags$label("Biodiversity Type", `for` = "biodiversity_type"), textInput("biodiversity_type", label = NULL, value = "Dune slack wetland")),
                                   div(class = "input-block", tags$label("Biodiversity Component", `for` = "biodiversity_component"), textInput("biodiversity_component", label = NULL, value = "Open water")),
                                   div(class = "input-block", tags$label("Biodiversity Attribute", `for` = "biodiversity_attribute"), textInput("biodiversity_attribute", label = NULL, value = "Water depth")),
                                   div(class = "input-block", tags$label("Measurement Unit", `for` = "measurement_unit"), textInput("measurement_unit", label = NULL, value = "Depth (cm)")),
                                   div(class = "input-block", tags$label("Benchmark Value"), numericInput("benchmark_value", label = NULL, value = 30, width = "100%")),
                                   div(class = "input-block", tags$label("Statistical Distribution"), selectInput("distribution", label = NULL, choices = c("Normal", "Poisson", "Binomial"), width = "100%")),
                                   div(class = "input-block", tags$label("Number of Simulations", `for` = "num_simulations"), numericInput("num_simulations", label = NULL, value = 100, min = 10, step = 10))
                               )
                           ),
                           
                           # __Impact + Offset side-by-side -----------------------------------------
                           div(class = "two-col-panels",
                               # Impact --------------------------------------------------------------
                               div(class = "colored-box blue-box form-section-box",
                                   div(class = "box-header", "Impact Site"),
                                   div(class = "compact-grid",
                                       
                                       # Impact Area (always start on a new row)
                                       div(class = "input-block row-start",  # Impact Area
                                           tags$label("Impact Area"),
                                           numericInput("impact_area", label = NULL, value = 0.35, width = "100%")
                                       ),
                               
                               # Impact Area Data Type + conditionals (unchanged)
                               div(class = "input-block",
                                   tags$label("Impact Area Data Type"),
                                   selectInput("impact_area_data_type", label = NULL,
                                               choices = c("Empirical", "Modelled", "Expert elicited", "Proxy value"),
                                               width = "100%")),
                               conditionalPanel(
                                 condition = "input.impact_area_data_type == 'Empirical'",
                                 div(class = "input-block",
                                     textAreaInput(
                                       "impact_area_empirical_details",
                                       "Provide sample size & duration:",
                                       "",
                                       placeholder = "250 words",
                                       width = "100%",
                                       rows = 3
                                     )
                                 )),
                               
                               
                               conditionalPanel(
                                 condition = "input.impact_area_data_type == 'Modelled'",
                                 textAreaInput("impact_area_modelled_details", "Describe the model:", "",
                                               placeholder = "250 words", width = "100%")
                               ),
                               conditionalPanel(
                                 condition = "input.impact_area_data_type == 'Expert elicited'",
                                 textAreaInput("impact_area_expert_details", "List expert(s) and affiliations:",
                                               placeholder = "250 words", "", width = "100%")
                               ),
                               conditionalPanel(
                                 condition = "input.impact_area_data_type == 'Proxy value'",
                                 textAreaInput("impact_area_proxy_details", "Give reference and/or describe:",
                                               placeholder = "250 words", "", width = "100%", rows = 6)
                               ),
                               
                               # Move Impact Area Unit to be just before Mean Prior Impact
                               div(class = "input-block row-start",
                                   tags$label("Impact Area Unit"),
                                   selectInput("impact_area_unit", label = NULL, choices = c("m","ha","m²"), width = "100%")
                               ),
                               
                               # Mean Prior Impact
                               div(class = "input-block",
                                   tags$label("Mean Prior Impact"),
                                   numericInput("mx_prior_impact_mean", label = NULL, value = 30, width = "100%")),
                               
                               # SD Prior Impact (always start on a new row)
                               div(class = "input-block row-start",  # SD Prior Impact
                                   tags$label("SD Prior Impact"),
                                   numericInput("mx_prior_impact_sd", label = NULL, value = 5, width = "100%")
                               ),
                               
                               # SD Prior Impact Data Type + conditionals (unchanged)
                               div(class = "input-block",
                                   tags$label("SD Prior Impact Data Type"),
                                   selectInput("prior_impact_sd_data_type", label = NULL,
                                               choices = c("Empirical", "Modelled", "Expert elicited", "Proxy value"),
                                               width = "100%")),
                               conditionalPanel(
                                 condition = "input.prior_impact_sd_data_type == 'Empirical'",
                                 div(class = "input-block",
                                     textAreaInput(
                                       "prior_impact_sd_empirical_details",
                                       "Provide sample size & duration:",
                                       "",
                                       placeholder = "250 words",
                                       width = "100%",
                                       rows = 3
                                     )
                                 )
                               )
                               ,
                               conditionalPanel(
                                 condition = "input.prior_impact_sd_data_type == 'Modelled'",
                                 textAreaInput("prior_impact_sd_modelled_details", "Describe the model:", "",
                                               placeholder = "250 words", width = "100%")
                               ),
                               conditionalPanel(
                                 condition = "input.prior_impact_sd_data_type == 'Expert elicited'",
                                 textAreaInput("prior_impact_sd_expert_details", "List expert(s) and affiliations:",
                                               placeholder = "250 words", "", width = "100%")
                               ),
                               conditionalPanel(
                                 condition = "input.prior_impact_sd_data_type == 'Proxy value'",
                                 textAreaInput("prior_impact_sd_proxy_details", "Give reference and/or describe:",
                                               placeholder = "250 words", "", width = "100%", rows = 6)
                               ),
                               
                               # Post Impact
                               div(class = "input-block row-start",
                                   tags$label("Mean Post Impact"),
                                   numericInput("mx_post_impact_mean", label = NULL, value = 0, width = "100%")
                               ),
                               div(class = "input-block", tags$label("SD Post Impact"),
                                   numericInput("mx_post_impact_sd", label = NULL, value = 5, width = "100%"))
                           )
                  ),
                               
                               # Offset --------------------------------------------------------------
                               div(class = "colored-box purple-box form-section-box",
                                   div(class = "box-header", "Offset Site"),
                                   div(class = "compact-grid offset-grid",
                                       div(class = "input-block", tags$label("Offset Area Unit"), selectInput("offset_area_unit", label = NULL, choices = c("m", "ha", "m²"), width = "100%")),
                                       div(class = "input-block", tags$label("Offset Area"), numericInput("offset_area", label = NULL, value = 1, width = "100%")),
                                       div(class = "input-block", tags$label("Mean Prior Offset"), numericInput("mx_prior_offset_mean", label = NULL, value = 0, width = "100%")),
                                       div(class = "input-block", tags$label("SD Prior Offset"), numericInput("mx_prior_offset_sd", label = NULL, value = 5, width = "100%")),
                                       div(class = "input-block", tags$label("SD Prior Offset Data Type"), selectInput("prior_offset_sd_data_type", label = NULL, choices = c("Empirical", "Modelled", "Expert elicited", "Proxy value"), width = "100%")),
                                       conditionalPanel(
                                         condition = "input.prior_offset_sd_data_type == 'Empirical'",
                                         div(class = "input-block",
                                             textAreaInput(
                                               "prior_offset_sd_empirical_details",
                                               "Provide sample size & duration:",
                                               "",
                                               placeholder = "250 words",
                                               width = "100%",
                                               rows = 3
                                             )
                                         )
                                       ),
                                       conditionalPanel(
                                         condition = "input.prior_offset_sd_data_type == 'Modelled'",
                                         textAreaInput("prior_offset_sd_modelled_details", "Describe the model:", "", placeholder = "250 words", width = "100%")
                                       ),
                                       conditionalPanel(
                                         condition = "input.prior_offset_sd_data_type == 'Expert elicited'",
                                         textAreaInput("prior_offset_sd_expert_details", "List expert(s) and affiliations:", placeholder = "250 words", "", width = "100%")
                                       ),
                                       conditionalPanel(
                                         condition = "input.prior_offset_sd_data_type == 'Proxy value'",
                                         textAreaInput("prior_offset_sd_proxy_details", "Give reference and/or describe:", placeholder = "250 words", "", width = "100%", rows = 6)
                                       ),
                                       div(class = "input-block", tags$label("Mean Post Offset"), numericInput("mx_post_offset_mean", label = NULL, value = 30, width = "100%")),
                                       div(class = "input-block", tags$label("SD Post Offset"), numericInput("mx_post_offset_sd", label = NULL, value = 5, width = "100%")),
                                       
                                       # Pairs that should share a row in 3-col mode 
                                         div(class = "input-block conf-level",
                                             tags$label("Confidence Level", `for` = "selected_confidence"),
                                             selectInput("selected_confidence", label = NULL, choices = confidence_levels$levels, selected = "Confident 75-90%")
                                         ),
                                       div(class = "input-block conf-justify",
                                           textAreaInput("offset_confidence_justify", "Justify confidence level:", placeholder = "250 words", "", width = "100%", rows = 3)
                                       ),
                                       
                                       div(class = "input-block end-time",
                                           tags$label("Time till End (Years)"),
                                           numericInput("time_till_end", label = NULL, value = 1, width = "100%")
                                       ),
                                       div(class = "input-block end-time-justify",
                                           textAreaInput("offset_time_till_end_justify", "Justify time till end:", placeholder = "250 words", "", width = "100%", rows = 3)
                                       ),
                                       
                                       div(class = "input-block discount-rate",
                                           tags$label("Discount Rate (%)"),
                                           numericInput("discount_rate", label = NULL, value = 3, width = "100%", step = 0.1)
                                       ),
                                       div(class = "input-block discount-justify",
                                           textAreaInput("offset_discount_rate_justify", "Justify discount rate:", placeholder = "250 words", "", width = "100%", rows = 3)
                                       )
                                   ))
                               ),
                           
                           # __Action buttons + compact summaries -----------------------------------
                           div(style = "text-align: center;",
                               actionButton("calculate", "Run Simulations", class = "btn btn-dark-green")
                           ),
                           tags$br(),
                           div(class = "summaries-row",
                               uiOutput("impact_summary"),
                               uiOutput("offset_summary"),
                               uiOutput("npbv_summary")
                           ),
                           tags$br(),
                           div(style = "text-align: center;",
                               div(style = "text-align: center;", uiOutput("cancel_edit_ui")),
                               actionButton("save_and_reset", "Save to Report & Reset", class = "btn btn-dark-green", style = "margin-left: 10px;", disabled = "disabled")
                           ),
                           tags$br(), tags$br(),
                           conditionalPanel(
                             condition = "output.saved_rows > 0",
                             div(class = "section-header", "Saved Results"),
                             DTOutput("saved_table"),
                             tags$br(),
                             # div(style = "text-align: center;",
                             #     actionButton("go_to_export", "Proceed to Export Report >", class = "btn btn-dark-green", style = "margin-bottom: 20px;")
                             # )
                           ),
                
                conditionalPanel(
                  condition = "output.saved_rows > 0",
                  div(class = "section-header", "Summary Table"),
                  DTOutput("summary_table"),
                  tags$br(),
                  div(style = "text-align: center;",
                      actionButton("go_to_export", "Proceed to Export Report >", class = "btn btn-dark-green", style = "margin-bottom: 20px;")
                  )
                )
                
                  ),
                  
                  # * Export Report UI ------------------------------------------------------
                  
                  
                tabPanel("Export Report",
                         div(class = "form-section",
                             tags$br(), tags$br(),
                             downloadButton("download_report_pdf", "Download Summary Report as pdf", class = "btn btn-dark-green"),
                             tags$br(), tags$br(), tags$br(),
                             downloadButton("download_long_report_pdf", "Download Full Report as pdf", class = "btn btn-dark-green"),
                             tags$br(), tags$br(), tags$br()
                         )
                )
                
      )
  )
)



# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  
    # Reset ALL inputs on the "Project Calculations" tab to their initial defaults
      reset_project_calc_inputs <- function() {
          # --- Biodiversity ---
            updateTextInput(   session, "biodiversity_type",      value   = "Dune slack wetland")
          updateTextInput(   session, "biodiversity_component", value   = "Open water")
          updateTextInput(   session, "biodiversity_attribute", value   = "Water depth")
          updateTextInput(   session, "measurement_unit",       value   = "Depth (cm)")
          updateNumericInput(session, "benchmark_value",        value   = 30)
          updateSelectInput( session, "distribution",           selected= "Normal")
          updateNumericInput(session, "num_simulations",        value   = 100)
      
            # --- Impact ---
            updateSelectInput( session, "impact_area_unit",       selected= "m")
        updateNumericInput(session, "impact_area",            value   = 0.35)
          updateSelectInput( session, "impact_area_data_type",  selected= "Empirical")
          updateTextAreaInput(session, "impact_area_empirical_details", value = "")
          updateTextAreaInput(session, "impact_area_modelled_details",  value = "")
          updateTextAreaInput(session, "impact_area_expert_details",    value = "")
          updateTextAreaInput(session, "impact_area_proxy_details",     value = "")
      
            updateNumericInput(session, "mx_prior_impact_mean",   value   = 30)
          updateNumericInput(session, "mx_prior_impact_sd",     value   = 5)
          updateSelectInput( session, "prior_impact_sd_data_type", selected = "Empirical")
          updateTextAreaInput(session, "prior_impact_sd_empirical_details", value = "")
         updateTextAreaInput(session, "prior_impact_sd_modelled_details",  value = "")
          updateTextAreaInput(session, "prior_impact_sd_expert_details",    value = "")
          updateTextAreaInput(session, "prior_impact_sd_proxy_details",     value = "")
      
            updateNumericInput(session, "mx_post_impact_mean",    value   = 0)
          updateNumericInput(session, "mx_post_impact_sd",      value   = 5)
      
            # --- Offset ---
          updateSelectInput( session, "offset_area_unit", selected = "m")
            updateNumericInput(session, "offset_area",            value   = 1)
          updateNumericInput(session, "mx_prior_offset_mean",   value   = 0)
          updateNumericInput(session, "mx_prior_offset_sd",     value   = 5)
         updateSelectInput( session, "prior_offset_sd_data_type", selected = "Empirical")
          updateTextAreaInput(session, "prior_offset_sd_empirical_details", value = "")
          updateTextAreaInput(session, "prior_offset_sd_modelled_details",  value = "")
         updateTextAreaInput(session, "prior_offset_sd_expert_details",    value = "")
          updateTextAreaInput(session, "prior_offset_sd_proxy_details",     value = "")
      
            updateNumericInput(session, "mx_post_offset_mean",    value   = 30)
          updateNumericInput(session, "mx_post_offset_sd",      value   = 5)
      
            updateSelectInput( session, "selected_confidence",    selected= "Confident 75-90%")
          updateTextAreaInput(session, "offset_confidence_justify",   value = "")
      
            updateNumericInput(session, "time_till_end",          value   = 1)
          updateTextAreaInput(session, "offset_time_till_end_justify", value = "")
      
          updateNumericInput(session, "discount_rate",          value   = 3)
          updateTextAreaInput(session, "offset_discount_rate_justify", value = "")
        }
    
    
      # New report confirm modal ----------------------------------------------  
      observeEvent(input$new_report_btn, {                                       
        showModal(modalDialog(                                                   
          title = "Start a new report?",                                         
          HTML("<p>This will discard all current inputs and saved results.</p>"),           
          easyClose = FALSE,                                                     
          footer = tagList(                                                      
            modalButton("Cancel"),                                               
            actionButton("confirm_new", "Continue", class = "btn btn-danger")    
          )                                                                      
        ))                                                                       
      })                                                                          
      
      # Perform full browser reload (new session) ------------------------------  
      observeEvent(input$confirm_new, {                                           
        removeModal()                                                           
        shinyjs::runjs("location.reload(true);")                                  
      })                                                                          
      
  
  
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
              "Impact_Mean" = "ImpM", "Impact_CI" = "ImpCI",
              "Offset_Mean" = "OffM", "Offset_CI" = "OffCI",
              "NPBV_Mean" = "NPBVM", "NPBV_CI" = "NPBVCI",
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
  
  
  
  # * remove underscores in table column names ----------------------------------------------------------
  clean_names <- function(df) {
    if (is.null(df)) return(df)
    names(df) <- gsub("_", " ", names(df))
    df
  }
  
  # * Collapse Mean + CI into single item---------------------------------------------------------
  fmt_pm <- function(m, ci) {
    ifelse(is.na(m) | is.na(ci), "", sprintf("%.2f \u00B1%.2f", m, ci))  # \u00B1 = ±
  }
  
  # Wrap text red if lower CI <= 0
  npbv_colorize <- function(mean, ci, text) {
    ifelse(mean - ci <= 0,
           sprintf('<span class="npbv-alert">%s</span>', text),  # or style="color:#C62828;font-weight:700;"
           text)
  }
  
  # * Load Draft---------------------------------------------------------
  compute_summary <- function(df) {
    if (is.null(df) || !nrow(df)) {
      return(data.frame(
        Component   = character(),
        Impact_Mean = numeric(), Impact_CI = numeric(),
        Offset_Mean = numeric(), Offset_CI = numeric(),
        NPBV_Mean   = numeric(), NPBV_CI   = numeric(),
        stringsAsFactors = FALSE
      ))
    }
    
    combine_means <- function(means, cis, conf_level = 0.95) {
      z  <- qnorm(1 - (1 - conf_level)/2)
      se <- cis / (2 * z)
      if (all(se == 0)) return(data.frame(mean = mean(means), ci = 0))
      w   <- 1 / (se^2)
      wmu <- sum(w * means) / sum(w)
      wse <- sqrt(1 / sum(w))
      data.frame(mean = wmu, ci = 2 * z * wse)
    }
    
    df %>%
      dplyr::group_by(biodiversity_component) %>%
      dplyr::summarise(
        Impact = list(combine_means(Impact_Mean, Impact_CI)),
        Offset = list(combine_means(Offset_Mean, Offset_CI)),
        NPBV   = list(combine_means(NPBV_Mean,  NPBV_CI)),
        .groups = "drop"
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        Impact_Mean = round(Impact$mean, 2),
        Impact_CI   = round(Impact$ci,   2),
        Offset_Mean = round(Offset$mean, 2),
        Offset_CI   = round(Offset$ci,   2),
        NPBV_Mean   = round(NPBV$mean,   2),
        NPBV_CI     = round(NPBV$ci,     2)
      ) %>%
      dplyr::select(biodiversity_component, Impact_Mean, Impact_CI,
                    Offset_Mean, Offset_CI, NPBV_Mean, NPBV_CI) %>%
      dplyr::rename(Component = biodiversity_component) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~ replace(., is.nan(.), 0))) %>%
      as.data.frame()
  }
  
  
  
  disable("save_and_reset")
  
  results <- reactiveValues(impact = NULL, offset = NULL, npbv = NULL)
  
  # * saved results reactive ----------------------------------------------------------
  
  saved_results <- reactiveVal(data.frame(stringsAsFactors = FALSE))
  
  # * summary results reactive ----------------------------------------------------------
  
  summary_results <- reactiveVal()
  # message store for the summary UI (used by the summaries below)
  sim_status <- reactiveVal(NULL)
  editing <- reactiveVal(FALSE)        # TRUE while editing a row
  edit_row_idx <- reactiveVal(NULL)    # 1-based row index being edited
  
  # * ping server ----------------------------------------------------------
  
  # # Total pings (8 hours, every 60 seconds -> 480 pings)
  # total_pings <- 480
  # ping_count <- reactiveVal(0)  # Track number of pings
  # last_ping_time <- reactiveVal(Sys.time())  # Time of the last ping
  # 
  # # Function to log pings to the console
  # log_ping <- function() {
  #   ping_count(ping_count() + 1)
  #   print(paste("Ping", ping_count(), "of", total_pings))  # Log each ping
  # }
  # 
  # # Check the time every second, and if a minute has passed, ping the server
  # observe({
  #   invalidateLater(1000, session)  # Check every second
  #   
  #   # Calculate time elapsed since the last ping
  #   time_elapsed <- as.numeric(difftime(Sys.time(), last_ping_time(), units = "secs"))
  #   
  #   # If a minute has passed and we have not exceeded the total pings
  #   if (time_elapsed >= 60 && ping_count() < total_pings) {
  #     log_ping()  # Log the ping
  #     last_ping_time(Sys.time())  # Update the last ping time
  #   }
  #   
  #   # Stop after 480 pings (8 hours)
  #   if (ping_count() >= total_pings) {
  #     print("Finished pinging. Server connection will now close.")
  #     return(NULL)  # Stop further pinging
  #   }
  # })
  
  # * 'about' modal popup ----------------------------------------------------------
  
  # observeEvent(input$show_header_modal, {
    showModal(modalDialog(
      title = "About",
      HTML("
      <p>This tool helps you simulate biodiversity offset scenarios based on user inputs.</p>
      <ul>
        <li>Enter ecological data and site characteristics</li>
        <li>Run simulations with confidence levels</li>
        <li>Save and export offset summaries</li>
      </ul>
      <br>
      <p><strong>The system will time out after 60 minutes of Inactivity.</strong></p>
            <p><strong>Use the Save Draft feature regularly to ensure work is not lost</strong></p>
      <br>
      <p>For assistance and questions please contact: <br>Jane Doe<br>
  <a href='mailto:jane.doe@example.com'>jane.doe@example.com</a><br>
  Phone: +64 21 123 4567</p>
    "),
      easyClose = TRUE,
      footer = modalButton("Continue")
    ))
  # })
  
  
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
    conf_score <- confidence_levels$scores[confidence_levels$levels == input$selected_confidence]
    discount_factor <- (1 + input$discount_rate / 100)^input$time_till_end
    sims <- replicate(input$num_simulations, {
      prior_impact <- rnorm(1, input$mx_prior_impact_mean, input$mx_prior_impact_sd)
      post_impact  <- rnorm(1, input$mx_post_impact_mean, input$mx_post_impact_sd)
      prior_offset <- rnorm(1, input$mx_prior_offset_mean, input$mx_prior_offset_sd)
      post_offset  <- rnorm(1, input$mx_post_offset_mean, input$mx_post_offset_sd)
      impact_value <- ((post_impact - prior_impact) / input$benchmark_value) * input$impact_area
      offset_value <- ((post_offset - prior_offset) / input$benchmark_value) * conf_score / discount_factor * input$offset_area
      npbv <- impact_value + offset_value
      c(impact_value, offset_value, npbv)
    })
    results$impact <- sims[1, ]
    results$offset <- sims[2, ]
    results$npbv   <- sims[3, ]
    
    enable("save_and_reset")
  })
  
  # * save and reset button ----------------------------------------------------------
  
  observeEvent(input$save_and_reset, {
     # Safety: require simulation results before saving
        if (is.null(results$impact) || is.null(results$offset) || is.null(results$npbv)) {
            showNotification("Please run or re-run simulations before saving.", type = "error")
            return()
          }
    
    ci <- function(x) 1.96 * sd(x) / sqrt(length(x))
    
    stats <- list(
      Impact_Mean = round(mean(results$impact), 2),
      Impact_CI = round(ci(results$impact), 2),
      Offset_Mean = round(mean(results$offset), 2),
      Offset_CI   = round(ci(results$offset),   2),
      NPBV_Mean   = round(mean(results$npbv),   2),
      NPBV_CI     = round(ci(results$npbv),     2)
    )
    
    # keep collecting ALL Project Calculations inputs
    vals <- lapply(project_calc_fields, function(id) input[[id]])
    names(vals) <- project_calc_fields
    
    # write ONLY raw inputs + stats (no legacy_* fields)
    new_row <- as.data.frame(
      c(vals, stats),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    
    # --- SAFE write path: overwrite when editing, else append ---
      df <- saved_results()
      if (is.null(df)) df <- data.frame(stringsAsFactors = FALSE)
      if (isTRUE(editing()) && !is.null(edit_row_idx()) && nrow(df) > 0) {
                # add to df any columns that exist in new_row but not in df — with typed NAs
                  for (nm in setdiff(names(new_row), names(df))) {
                      cls <- class(new_row[[nm]])
                      if ("character" %in% cls) {
                          df[[nm]] <- rep(NA_character_, nrow(df))
                        } else if (any(c("numeric", "double") %in% cls)) {
                            df[[nm]] <- rep(NA_real_, nrow(df))
                          } else if ("integer" %in% cls) {
                              df[[nm]] <- rep(NA_integer_, nrow(df))
                            } else {
                                df[[nm]] <- rep(NA, nrow(df))
                              }
                    }
                # add to new_row any columns that exist in df but not in new_row — with typed NAs
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
                # reorder and replace the row being edited
                  new_row <- new_row[, names(df), drop = FALSE]
                  i <- edit_row_idx()
                  if (!is.na(i) && i >= 1 && i <= nrow(df)) {
                      df[i, ] <- new_row[1, ]
                    } else {
                        showNotification("Edit index out of range; appending new row.", type = "warning")
                       df <- dplyr::bind_rows(df, new_row)
                      }
                  # exit edit mode & restore labels
                    editing(FALSE); edit_row_idx(NULL)
                  updateActionButton(session, "calculate",      label = "Run Simulations")
                  updateActionButton(session, "save_and_reset", label = "Save to Report & Reset")
                } else {
                    # simple append on first save — no pre-alignment needed (avoids logical/character clash)
                      df <- dplyr::bind_rows(df, new_row)
                    }
      saved_results(df)
    
        # Recompute summary & reset state
        summary_results(compute_summary(saved_results()))
        results$impact <- results$offset <- results$npbv <- NULL
              reset_project_calc_inputs()
      disable("save_and_reset")
      session$sendCustomMessage("scrollToBottom", "now")
    
      print_saved_results(saved_results(), title = "SAVED RESULTS TABLE AFTER SAVE")
  })
  
  # Show the modal when JS sends the warning
  observeEvent(input$idle_warn, {
    showModal(modalDialog(
      title = "System Inactivity detected",
      HTML("       <p>Click <strong>Save Draft</strong> regularly to avoid losing any work.</p>"),
      easyClose = FALSE,    # cannot dismiss by clicking backdrop
      footer = NULL         # no Close button; any activity will dismiss
    ))
  }, ignoreInit = TRUE)
  
  # Hide the modal on any real activity
  observeEvent(input$dismiss_idle, {
    removeModal()
  }, ignoreInit = TRUE)
  
  
  
  # * model output summary ----------------------------------------------------------
   ## _impact summary ----------------------------------------------------------
  
  output$impact_summary <- renderUI({
    msg <- sim_status(); if (!is.null(msg)) return(span(style="color:red;", msg))
    if (is.null(results$impact)) return(span(style="color:red;","Run simulations to see results."))
    span(style="color:blue;",
         sprintf("Impact: %s",
                 fmt_pm(mean(results$impact), 1.96 * sd(results$impact)/sqrt(length(results$impact)))))
  })
  
  ## _ offset summary ----------------------------------------------------------
  
  output$offset_summary <- renderUI({
    if (is.null(results$offset)) return(span(style="color:red;"," "))
    span(style="color:blue;",
         sprintf("Offset: %s",
                 fmt_pm(mean(results$offset), 1.96 * sd(results$offset)/sqrt(length(results$offset)))))
  })
  
  ## _ npbv summary ----------------------------------------------------------
  
  output$npbv_summary <- renderUI({
    if (is.null(results$npbv)) return(span(style="color:red;"," "))
    span(style="color:blue;",
         sprintf("NPBV: %s",
                 fmt_pm(mean(results$npbv), 1.96 * sd(results$npbv)/sqrt(length(results$npbv)))))
  })
  
  # * render full table ----------------------------------------------------------
  
  # output$saved_table <- renderDT({
  #   browser()
  #   df <- saved_results()
  #   if (nrow(df) == 0) return(datatable(df))
  #   delete_btns <- sprintf('<button class="btn btn-sm btn-danger delete-btn" data-row="%s">Delete</button>', seq_len(nrow(df)))
  #   df <- cbind(Delete = delete_btns, df)
  #   # clean column names
  #   df <- clean_names(df)
  #   datatable(df, escape = FALSE, selection = 'none', options = list(dom = 't', paging = FALSE),
  #             callback = JS("
  #               table.on('click', '.delete-btn', function() {
  #                 Shiny.setInputValue('delete_row', this.getAttribute('data-row'), {priority: 'event'});
  #               });
  #             "))
  # }, server = FALSE)
  
  
  
  output$saved_table <- renderDT({
    
    df <- saved_results()
    if (nrow(df) == 0) return(datatable(df))
    
    # Add Delete buttons
    delete_btns <- sprintf(
      '<button class="btn btn-sm btn-danger delete-btn" data-row="%s">Delete</button>',
      seq_len(nrow(df))
    )
    df <- cbind(Delete = delete_btns, df)
    
    edit_btns <- sprintf(
      '<button class="btn btn-sm btn-danger edit-btn" data-row="%s">Edit</button>',
      seq_len(nrow(df))
    )
    df <- cbind(Edit = edit_btns, df)
    
    # Collapse Mean + CI into single item------------------
    df <- df %>%
      dplyr::mutate(
        Impact = fmt_pm(Impact_Mean, Impact_CI),
        Offset = fmt_pm(Offset_Mean, Offset_CI),
        NPBV   = npbv_colorize(NPBV_Mean, NPBV_CI, fmt_pm(NPBV_Mean, NPBV_CI)) 
      ) %>%
      dplyr::select(
        Delete, Edit,
        biodiversity_type, biodiversity_component, biodiversity_attribute, measurement_unit,
        Impact, Offset, NPBV
      )
    
    # Clean the column headers of underscores
    df <- clean_names(df)
# <<<<<<< HEAD
# 
# =======
    
        # Header labels (make the first two headers blank)
          col_labels <- c(
              "", "",                    # Delete, Edit
              "Biodiversity Type",
              "Component",
              "Attribute",
             "Measurement Unit",
              "Impact", "Offset", "NPBV"
            )
        
      
    # this makes DT repaint when edit_row_idx changes
    idx <- edit_row_idx()
    
# >>>>>>> Edit-Functionality
    # --- Render DT table ---
    datatable(
      df,
      colnames = col_labels,
      escape = FALSE,
      selection = 'none',
      options = list(
        dom = 't',
        paging = FALSE,
        columnDefs = list(
                    list(targets = c(1,2), orderable = FALSE, searchable = FALSE) # actions
                  ),
        # rowCallback runs every draw, so the row stays highlighted and
         #          the Edit button reads EDITING for the active row 
                  rowCallback = JS(
                      sprintf(
                          "function(row, data, displayNum, displayIndex, dataIndex){
               var editIdx = %s;              // 1-based index from Shiny
               var thisIdx = dataIndex + 1;   // DataTables data index is 0-based
               var $row = $(row);
               if (editIdx !== null && thisIdx === editIdx){
                 $row.addClass('editing-row');
                 $row.find('.edit-btn').text('EDITING');
               } else {
                 $row.removeClass('editing-row');
                 $row.find('.edit-btn').text('Edit');
              }
             }",
                          ifelse(is.null(idx), "null", idx)
                        )
                    )
      ),
      callback = JS("
      table.on('click', '.delete-btn', function() {
        Shiny.setInputValue('delete_row', this.getAttribute('data-row'), {priority: 'event'});
      });
      table.on('click', '.edit-btn', function() {
        // reset all rows/buttons first
        table.$('tr').removeClass('editing-row');
        table.$('.edit-btn').text('Edit');
        // mark this row now
        var $tr = $(this).closest('tr');
        $tr.addClass('editing-row');
        $(this).text('EDITING');
        // compute 1-based row index consistent with server
        var rowIdx = table.row($tr).index() + 1;
        Shiny.setInputValue('edit_row', rowIdx, {priority: 'event'});
      });
    ")
    )
  }, server = FALSE)
  
  # * delete row ----------------------------------------------------------
  
  observeEvent(input$delete_row, {
    df <- saved_results()
    row <- as.numeric(input$delete_row)
    if (!is.na(row) && row <= nrow(df)) {
      df <- df[-row, ]
      saved_results(df)
      # If we just deleted the row being edited, exit edit mode and reset UI
          if (isTRUE(editing()) && !is.null(edit_row_idx()) && row == edit_row_idx()) {
              editing(FALSE)
              edit_row_idx(NULL)
              updateActionButton(session, "calculate",      label = "Run Simulations")
              updateActionButton(session, "save_and_reset", label = "Save to Report & Reset")
              # cancel button hides automatically because output$cancel_edit_ui depends on editing()
              }
      
      summary_results(compute_summary(saved_results()))
          print_saved_results(saved_results(), title = "SAVED RESULTS TABLE AFTER DELETE")
    }
  })
  
  # * edit row ----------------------------------------------------------
  
  observeEvent(input$edit_row, {
# <<<<<<< HEAD
#     browser()
# =======
    row <- as.integer(input$edit_row)
    df  <- saved_results()
    req(!is.na(row), row >= 1, row <= nrow(df))
    
    # 1) mark edit state + highlight
    edit_row_idx(row)
    editing(TRUE)
# >>>>>>> Edit-Functionality
    
    # 2) repopulate inputs from that row (use only fields you actually store)
    row_list <- as.list(df[row, , drop = FALSE])
    restore_inputs_from_list(row_list)
    
    # 3) move to top
    session$sendCustomMessage("scrollToTop", "now")
    
    # 4) tweak button labels
    updateActionButton(session, "calculate",     label = "Re-Run Simulations")
    updateActionButton(session, "save_and_reset",label = "Update Entry In Report")
    
    # 5) enable save button (in case it was disabled)
    enable("save_and_reset")
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
  
  
  restore_inputs_from_list <- function(ins) {
    getOr <- function(x, nm, default = NULL) if (!is.null(x[[nm]])) x[[nm]] else default
    
    # --- Biodiversity ---
    updateTextInput(   session, "biodiversity_type",      value   = getOr(ins, "biodiversity_type", ""))
    updateTextInput(   session, "biodiversity_component", value   = getOr(ins, "biodiversity_component", ""))
    updateTextInput(   session, "biodiversity_attribute", value   = getOr(ins, "biodiversity_attribute", ""))
    updateTextInput(   session, "measurement_unit",       value   = getOr(ins, "measurement_unit", ""))
    updateNumericInput(session, "benchmark_value",        value   = getOr(ins, "benchmark_value", 30))
    updateSelectInput( session, "distribution",           selected= getOr(ins, "distribution", "Normal"))
    updateNumericInput(session, "num_simulations",        value   = getOr(ins, "num_simulations", 100))
    
    # --- Impact ---
    updateSelectInput( session, "impact_area_unit",       selected= getOr(ins, "impact_area_unit", "m"))
    updateNumericInput(session, "impact_area",            value   = getOr(ins, "impact_area", 0.35))
    updateSelectInput( session, "impact_area_data_type",  selected= getOr(ins, "impact_area_data_type", "Empirical"))
    updateTextAreaInput(session, "impact_area_empirical_details", value = getOr(ins, "impact_area_empirical_details", ""))
    updateTextAreaInput(session, "impact_area_modelled_details",  value = getOr(ins, "impact_area_modelled_details", ""))
    updateTextAreaInput(session, "impact_area_expert_details",    value = getOr(ins, "impact_area_expert_details", ""))
    updateTextAreaInput(session, "impact_area_proxy_details",     value = getOr(ins, "impact_area_proxy_details", ""))
    
    updateNumericInput(session, "mx_prior_impact_mean",   value   = getOr(ins, "mx_prior_impact_mean", 30))
    updateNumericInput(session, "mx_prior_impact_sd",     value   = getOr(ins, "mx_prior_impact_sd", 5))
    updateSelectInput( session, "prior_impact_sd_data_type", selected = getOr(ins, "prior_impact_sd_data_type", "Empirical"))
    updateTextAreaInput(session, "prior_impact_sd_empirical_details", value = getOr(ins, "prior_impact_sd_empirical_details", ""))
    updateTextAreaInput(session, "prior_impact_sd_modelled_details",  value = getOr(ins, "prior_impact_sd_modelled_details", ""))
    updateTextAreaInput(session, "prior_impact_sd_expert_details",    value = getOr(ins, "prior_impact_sd_expert_details", ""))
    updateTextAreaInput(session, "prior_impact_sd_proxy_details",     value = getOr(ins, "prior_impact_sd_proxy_details", ""))
    
    updateNumericInput(session, "mx_post_impact_mean",    value   = getOr(ins, "mx_post_impact_mean", 0))
    updateNumericInput(session, "mx_post_impact_sd",      value   = getOr(ins, "mx_post_impact_sd", 5))
    
    # --- Offset ---
    updateSelectInput( session, "offset_area_unit",       selected = getOr(ins, "offset_area_unit", "m"))
    updateNumericInput(session, "offset_area",            value   = getOr(ins, "offset_area", 1))
    updateNumericInput(session, "mx_prior_offset_mean",   value   = getOr(ins, "mx_prior_offset_mean", 0))
    updateNumericInput(session, "mx_prior_offset_sd",     value   = getOr(ins, "mx_prior_offset_sd", 5))
    updateSelectInput( session, "prior_offset_sd_data_type", selected = getOr(ins, "prior_offset_sd_data_type", "Empirical"))
    updateTextAreaInput(session, "prior_offset_sd_empirical_details", value = getOr(ins, "prior_offset_sd_empirical_details", ""))
    updateTextAreaInput(session, "prior_offset_sd_modelled_details",  value = getOr(ins, "prior_offset_sd_modelled_details", ""))
    updateTextAreaInput(session, "prior_offset_sd_expert_details",    value = getOr(ins, "prior_offset_sd_expert_details", ""))
    updateTextAreaInput(session, "prior_offset_sd_proxy_details",     value = getOr(ins, "prior_offset_sd_proxy_details", ""))
    
    updateNumericInput(session, "mx_post_offset_mean",    value   = getOr(ins, "mx_post_offset_mean", 30))
    updateNumericInput(session, "mx_post_offset_sd",      value   = getOr(ins, "mx_post_offset_sd", 5))
    
    updateSelectInput( session, "selected_confidence",    selected= getOr(ins, "selected_confidence", "Confident 75-90%"))
    updateTextAreaInput(session, "offset_confidence_justify",   value = getOr(ins, "offset_confidence_justify", ""))
    
    updateNumericInput(session, "time_till_end",          value   = getOr(ins, "time_till_end", 1))
    updateTextAreaInput(session, "offset_time_till_end_justify", value = getOr(ins, "offset_time_till_end_justify", ""))
    
    updateNumericInput(session, "discount_rate",          value   = getOr(ins, "discount_rate", 3))
    updateTextAreaInput(session, "offset_discount_rate_justify", value = getOr(ins, "offset_discount_rate_justify", ""))
  }
  
  
  # * render summary table ----------------------------------------------------------

  output$summary_table <- renderDT({
    df <- saved_results()

    if (nrow(df) == 0) return(datatable(df))
    
    # --- Helper function to combine means & CI widths ---
    combine_means <- function(means, cis, conf_level = 0.95) {
      z <- qnorm(1 - (1 - conf_level)/2)
      
      # Convert CI to SE; handle all-zero CI
      se <- cis / (2 * z)
      if (all(se == 0)) {
        return(data.frame(mean = mean(means), ci = 0))
      }
      
      weights <- 1 / (se^2)
      wmean <- sum(weights * means) / sum(weights)
      wse <- sqrt(1 / sum(weights))
      
      data.frame(mean = wmean, ci = 2 * z * wse)
    }
    
    # --- Group by Component and compute weighted means & CI widths ---
    summary_df <- df %>%
      group_by(biodiversity_component) %>%
      summarise(
        Impact = list(combine_means(Impact_Mean, Impact_CI)),
        Offset = list(combine_means(Offset_Mean, Offset_CI)),
        NPBV   = list(combine_means(NPBV_Mean, NPBV_CI)),
        .groups = "drop"
      ) %>%
      rowwise() %>%
      mutate(
        Impact_Mean = round(Impact$mean, 2),
        Impact_CI   = round(Impact$ci, 2),
        Offset_Mean = round(Offset$mean, 2),
        Offset_CI   = round(Offset$ci, 2),
        NPBV_Mean   = round(NPBV$mean, 2),
        NPBV_CI     = round(NPBV$ci, 2)
      ) %>%
      select(biodiversity_component, Impact_Mean, Impact_CI, Offset_Mean, Offset_CI, NPBV_Mean, NPBV_CI) %>%
      rename(Component = biodiversity_component) %>%
      ungroup()
    
    summary_df <- summary_df %>%
      mutate(across(where(is.numeric), ~ replace(., is.nan(.), 0)))

    summary_results(summary_df)
    
    
    # Handing the Collapsed Mean + CI into single item------------------
    summary_df <- summary_df %>%
      dplyr::mutate(
        Impact = fmt_pm(Impact_Mean, Impact_CI),
        Offset = fmt_pm(Offset_Mean, Offset_CI),
        NPBV   = npbv_colorize(NPBV_Mean, NPBV_CI, fmt_pm(NPBV_Mean, NPBV_CI))
      ) %>%
      dplyr::select(Component, Impact, Offset, NPBV)
    
    # --- Replace underscores with spaces ---
    summary_df <- clean_names(summary_df)
    
    # --- Render table ---
    datatable(summary_df, escape = FALSE, selection = 'none', options = list(dom = 't', paging = FALSE))
  }, server = FALSE)

# * download pdf ----------------------------------------------------------

  # helper function to avoid duplicating code
  render_report <- function(file, long_report_flag) {
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
        offset_package = input$offset_package,
        saved_results_df = clean_names(saved_results()),
        summary_results_df = clean_names(summary_results()),
        long_report = long_report_flag
      ),
      envir = new.env(parent = globalenv())
    )
    
    # Copy the output file to the Shiny download target
    file.copy(file.path(temp_dir, "report.pdf"), file)
  }
  
  # Short report
  output$download_report_pdf <- downloadHandler(
    filename = function() {
      paste0("report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      render_report(file, long_report_flag = FALSE)
    }
  )
  
  # Long report
  output$download_long_report_pdf <- downloadHandler(
    filename = function() {
      paste0("long_report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      render_report(file, long_report_flag = TRUE)
    }
  )
  
	
  # ---- Draft: fields to capture/restore ----
  draft_fields <- c(
    # Project Details
    "project_name","prepared_by","date",
    "proposal_overview","ecological_context","offset_package",
    # Project Calculations - Biodiversity
    "biodiversity_type","biodiversity_component","biodiversity_attribute",
    "measurement_unit","benchmark_value","distribution","num_simulations",
    # Impact
    "impact_area_unit","impact_area","impact_area_data_type",
    "impact_area_empirical_details","impact_area_modelled_details","impact_area_expert_details","impact_area_proxy_details",
    "mx_prior_impact_mean",
    "mx_prior_impact_sd", "prior_impact_sd_data_type",
    "prior_impact_sd_empirical_details", "prior_impact_sd_modelled_details", "prior_impact_sd_expert_details", "prior_impact_sd_proxy_details",
    "mx_post_impact_mean","mx_post_impact_sd",
    # Offset
    "offset_area_unit","offset_area","mx_prior_offset_mean","mx_prior_offset_sd",
    "prior_offset_sd_data_type", "prior_offset_sd_empirical_details", "prior_offset_sd_modelled_details", "prior_offset_sd_expert_details", "prior_offset_sd_proxy_details",
    "mx_post_offset_mean","mx_post_offset_sd",
    "selected_confidence", "offset_confidence_justify",
    "time_till_end", "offset_time_till_end_justify",
    "discount_rate", "offset_discount_rate_justify"

  )

  # Everything in Project Calculations = draft_fields minus Project Details fields
  project_calc_fields <- setdiff(
    draft_fields,
    c("project_name","prepared_by","date",
      "proposal_overview","ecological_context","offset_package")
  )
  
  # ---- SAVE: download JSON with inputs + saved_results ----
  output$draft_export <- downloadHandler(
    filename = function() {
      paste0("biodiversity_draft_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".json")
    },
    content = function(file) {
      vals <- lapply(draft_fields, function(id) input[[id]])
      names(vals) <- draft_fields
      payload <- list(
        inputs = vals,
        saved_results = saved_results()  # <- your variable-length table
      )
      writeLines(jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE, na = "null"), file, useBytes = TRUE)
    }
  )
  
  # ---- LOAD: read JSON and push values back into the UI + table ----
  observeEvent(input$draft_import_file, {
    req(input$draft_import_file$datapath)
    txt <- readLines(input$draft_import_file$datapath, warn = FALSE, encoding = "UTF-8")
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
    updateTextAreaInput(session, "offset_package",      value = getOr(ins, "offset_package", ""))
    
    # --- Biodiversity ---
    updateTextInput(session, "biodiversity_type",        value = getOr(ins, "biodiversity_type", ""))
    updateTextInput(session, "biodiversity_component",   value = getOr(ins, "biodiversity_component", ""))
    updateTextInput(session, "biodiversity_attribute",   value = getOr(ins, "biodiversity_attribute", ""))
    updateTextInput(session, "measurement_unit",         value = getOr(ins, "measurement_unit", ""))
    updateNumericInput(session, "benchmark_value",       value = getOr(ins, "benchmark_value", 30))
    updateSelectInput(session,  "distribution",        selected = getOr(ins, "distribution", "Normal"))
    updateNumericInput(session, "num_simulations",     value = getOr(ins, "num_simulations", 100))
    
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
        saved_results(df)                                 # 1) update backend table
        summary_results(compute_summary(saved_results())) # 2) recompute summary now
      } else {
        saved_results(data.frame(
          Biodiversity_Type = character(), Component = character(),
          Attribute = character(), Measurement_Unit = character(),
          Impact_Mean = numeric(), Impact_CI = numeric(),
          Offset_Mean = numeric(), Offset_CI = numeric(),
          NPBV_Mean = numeric(), NPBV_CI = numeric(),
          stringsAsFactors = FALSE
        ))
        
        # good fallback:
        saved_results(data.frame(stringsAsFactors = FALSE))
      }
    } else {
      # No saved table in draft -> reset to empty & summary empty
      saved_results(data.frame(
        Biodiversity_Type = character(), Component = character(),
        Attribute = character(), Measurement_Unit = character(),
        Impact_Mean = numeric(), Impact_CI = numeric(),
        Offset_Mean = numeric(), Offset_CI = numeric(),
        NPBV_Mean = numeric(), NPBV_CI = numeric(),
        stringsAsFactors = FALSE
      ))
      summary_results(compute_summary(saved_results()))
    }
    
    # Clear any old simulation vectors from previous session
    results$impact <- results$offset <- results$npbv <- NULL
    disable("save_and_reset")
    
    showNotification("Draft imported.", type = "message", duration = 3)
  })
  
}

# Run App -----------------------------------------------------------------
shinyApp(ui = ui, server = server)