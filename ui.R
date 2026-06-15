
# UI ----------------------------------------------------------------------
ui <- fluidPage(
  theme = custom_theme,
  useShinyjs(),
  tags$head(

    tags$link(href = "https://fonts.googleapis.com/css2?family=Lato&display=swap", rel = "stylesheet"),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(src = "app-hover-behaviour.js"), #THIS CONTAINS HOVEROVER INFORMATION
    tags$style(HTML("body { font-family: 'Lato', sans-serif; }")),

    # Scripts ----------------------------------------------------------------------

    tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function(){
    var btn = document.getElementById('draft_import_btn');
    var file = document.querySelector('input[type=file][id$=\"draft_import_file\"]');
    if(btn && file){ btn.addEventListener('click', function(){ file.click(); }); }
    var xlsxBtn = document.getElementById('xlsx_upload_btn');
    var xlsxFile = document.querySelector('input[type=file][id$=\"xlsx_upload_file\"]');
    if(xlsxBtn && xlsxFile){ xlsxBtn.addEventListener('click', function(){ xlsxFile.click(); }); }
  });
                ")),
    tags$style(HTML("
    .invalid-input {
      border: 2px solid red !important;
      box-shadow: none !important;
    }
  ")),

    # Keepalive: send a periodic event to reset shinyapps.io's idle timer
    tags$script(HTML("
    setInterval(function() {
      Shiny.setInputValue('keepalive_ping', Date.now(), {priority: 'event'});
    }, 4 * 60 * 1000);
  "))

  ),

  tags$script(HTML("
  Shiny.addCustomMessageHandler('clearAllInvalidInputs', function(ids) {
    ids.forEach(function(id) {
      $('#' + id).removeClass('invalid-input');
    });
  });
")),

  tags$script(HTML("
  Shiny.addCustomMessageHandler('addInvalidClass', function(id) {
    $('#' + id).addClass('invalid-input');
  });
  Shiny.addCustomMessageHandler('removeInvalidClass', function(id) {
    $('#' + id).removeClass('invalid-input');
  });
")),

  # Autosave: persist a draft to browser localStorage after each completed
  # attribute, and offer to restore it if found on the next page load.
  tags$script(HTML("
  (function(){
    var AUTOSAVE_KEY = 'boam_autosave_draft';

    Shiny.addCustomMessageHandler('autosaveDraft', function(payload) {
      try {
        localStorage.setItem(AUTOSAVE_KEY, JSON.stringify({
          data: payload,
          ts: new Date().toLocaleString()
        }));
      } catch (e) {}
    });

    Shiny.addCustomMessageHandler('clearAutosave', function(msg) {
      try { localStorage.removeItem(AUTOSAVE_KEY); } catch (e) {}
    });

    function checkAutosave() {
      try {
        var raw = localStorage.getItem(AUTOSAVE_KEY);
        if (raw) {
          Shiny.setInputValue('autosave_check', JSON.parse(raw), {priority: 'event'});
        }
      } catch (e) {}
    }

    // On a local/fast connection 'shiny:connected' can fire before this
    // script runs and registers the listener, so poll for the socket
    // becoming ready as a fallback.
    $(document).on('shiny:connected', checkAutosave);
    (function pollConnected() {
      if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && Shiny.shinyapp.$socket.readyState === 1) {
        checkAutosave();
      } else {
        setTimeout(pollConnected, 50);
      }
    })();
  })();
")),

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
")),

  # hidden file input (kept outside the header, stays invisible)
  tags$div(style = "display:none;",
           fileInput("draft_import_file", "Choose JSON", accept = ".json"),
           fileInput("xlsx_upload_file", "Choose Excel", accept = ".xlsx")
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
      tabsetPanel(id = "main_tabs", selected = "About this model",

                  # * About UI ------------------------------------------------------
                  tabPanel(
                    "About this model",
                    div(
                      class = "form-section",

                      HTML("
                        <p>This net gain model evaluates offset proposals for ecological equivalence and net gain across biodiversity type, amount, and time, based on user inputs, and can be used to simulate numerous biodiversity offset scenarios to compare predicted outcomes.</p>

                        <p>The model uses Net Present Biodiversity Value (NPBV) to indicate predicted outcomes from offset proposals, coupled with prediction intervals to describe certainty associated with the model outputs.</p>

                        <p>The NPBV does not quantify predicted net gain outcomes. Outputs from this model should be further interpreted to meaningful ecological 'on-the-ground' measures to support quantified (e.g., 10%) net gain claims.</p>

                        <p><strong>The system will time out after 120 minutes of inactivity.</strong></p>
                        <p><strong>Use the Save Draft feature regularly to ensure work is not lost.</strong></p>

                        <p>For assistance and questions please contact:<br>
                        Jane Doe<br>
                        <a href='mailto:jane.doe@example.com'>jane.doe@example.com</a><br>
                        Phone: +64 21 123 4567</p>
                      "),

                      tags$img(
                        src = "custom/Screenshot 2025-10-14 at 6.40.52 PM.png",
                        style = "max-width: 100%; height: auto; margin-top: 15px;"
                      ),

                      tags$br(), tags$br(),

                      tags$h4("How uncertainty is calculated in this model"),

                      tags$p("The original BOAM model is deterministic and uses single point values to describe biodiversity values at the impact and offset sites—one number in,
                             one number out. This revised model makes uncertainty explicit by using a Monte Carlo simulation framework to propagate
                             uncertainty through all stages of the offset calculation."),

                      tags$p("For each biodiversity attribute, the model takes the user’s inputs describing central tendency and uncertainty
                      (for example, a mean and a measure of spread such as standard deviation, sample size,
                      or an expert-elicited range) and generates 10,000 plausible values from an appropriate
                      statistical distribution constrained by ecological bounds. This is done independently for all four measurement points:
                             prior to impact, post impact, prior to offset, and post offset."),

                      tags$p("These simulated values are then carried through the full Net Present Biodiversity Value (NPBV) calculation - including benchmark scaling,
                             area weighting, confidence adjustment, and time discounting - producing 10,000 plausible NPBV outcomes for each attribute.
                             In this way, uncertainty in the inputs is consistently propagated through the entire model,
                             rather than being applied only to the final result."),

                      tags$p("From the resulting distribution of 10,000 outcomes,
                             the model reports three summary statistics:"),
                      tags$ul(
                        tags$li(tags$strong("Mean"), " — the central estimate of the NPBV."),
                        tags$li(tags$strong("5th percentile (P5)"), " — a pessimistic but plausible scenario,
                                representing the lower boundary below which only 5% of simulated outcomes fall."),
                        tags$li(tags$strong("95th percentile (P95)"), " — an optimistic but plausible scenario,
                                representing the upper boundary above which only 5% of simulated outcomes fall.")
                      ),

                      tags$p("Together, the P5 and P95 bounds define a 90% prediction interval - the range within which the
                             true NPBV is expected to fall with 90% probability, given the input assumptions.
                             Unlike traditional confidence intervals, these bounds are not constrained to be symmetric,
                             honestly reflecting the true shape of the underlying uncertainty in the model inputs and transformations."),

                      tags$p("At the component level, the model aggregates the attribute-level simulation results using equal weighting,
                             consistent with the BOAM framework’s disaggregated currency and its emphasis on like-for-like exchanges,
                             transparency, and the avoidance of implicit value weighting. For each of the 10,000 simulation runs,
                             the attribute-level NPBV values within a component are averaged to produce a single component-level NPBV.
                             The component-level mean, P5, and P95 are then derived from the resulting distribution of 10,000 component scores."),

                      tags$p("A negative P5 at either the attribute or component level indicates that there is a meaningful probability (at least 5%)
                             that the proposed offset will fail to achieve no net loss for that element of biodiversity, given the stated assumptions.
                             These cases are highlighted in red throughout the model outputs to clearly signal residual risk,
                             even where the mean outcome may be neutral or positive. Similarly, a positive mean NPBV with a 90% prediction interval that
                             includes zero indicates that the proposed offset may only achieve no net loss, thereby failing to guarantee a net gain outcome."),

                      tags$h4("Choosing a distribution: which one fits your data?"),

                      tags$p("The model offers a few different ways of describing the uncertainty in a measurement,
                             depending on how that measurement was collected. Choosing the option that matches
                             how the data was gathered helps the model represent uncertainty realistically."),

                      tags$ul(
                        tags$li(
                          tags$strong("Normal — for measurements with a typical value and a spread either side. "),
                          "Use this when your value is a continuous measurement (e.g. percentage cover, an index score,
                           a density estimate) and you have a sense of both the average and how much it tends to vary —
                           for example, from repeat plots or survey rounds. You provide the average (mean) and a measure
                           of how spread out the values are (standard deviation)."
                        ),
                        tags$li(
                          tags$strong("Poisson — for counts of things. "),
                          "Use this when your data is a count — number of individuals, nests, burrows, sightings,
                           or similar — collected across a number of survey plots, transects, or visits. You provide
                           the average count and the number of plots/visits the count is based on. The model uses this
                           to work out how reliable that average count is likely to be: counts based on more plots are
                           treated as more certain than counts based on just a few."
                        ),
                        tags$li(
                          tags$strong("Negative Binomial — for counts that are more variable than usual. "),
                          "Some count data is 'patchy' — most plots have few or none, but occasionally a plot has a lot
                           more (e.g. species that occur in clumps or colonies). If your counts vary more than you'd
                           expect from a simple average (the variance is clearly bigger than the mean), this option
                           better reflects that extra variability. You provide the average count and the observed
                           variance across your sample."
                        ),
                        tags$li(
                          tags$strong("Expert elicited — for values based on professional judgement rather than direct measurement. "),
                          "Use this when there isn't enough direct survey data to calculate a mean and spread, and the
                           value instead relies on the judgement of an experienced ecologist or other expert. Rather
                           than a mean and standard deviation, you provide three values: a realistic low estimate,
                           a most likely (central) estimate, and a realistic high estimate, along with how confident
                           the expert is that the true value falls between the low and high estimates (e.g. 90%
                           confident). The model converts these into an equivalent average and spread, and always
                           treats expert-elicited values as following a typical bell-curve pattern of uncertainty."
                        )
                      ),

                      tags$p("If you're unsure which option applies, ask: ", tags$em("was this a direct count from survey
                             plots, a continuous measurement with known variability, or a best estimate from an expert
                             because direct data wasn't available?"), " That will usually point to the right choice.")
                    )
                  ),

                  # * How to use this model UI ------------------------------------------------------
                  tabPanel(
                    "How to use this model",
                    div(
                      class = "form-section",
                      HTML("
      <ol>
        <li>Enter Biodiversity Type</li>
        <li>Enter a Biodiversity Component</li>
        <li>Enter a Biodiversity Attribute</li>
        <li>Enter all remaining data associated with that Biodiversity Attribute</li>
        <li>Click on <strong>'Run Simulations'</strong></li>
        <li>Click on <strong>'Save to Report & Reset'</strong></li>
        <li>Repeat steps 3–6 for all additional attributes</li>
        <li>Repeat steps 2–7 for all additional Biodiversity Components</li>
        <li>Repeat steps 1–8 for additional Biodiversity Types</li>
      </ol>
    ")
                    )
                  ),

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
                               textAreaInput("proposal_overview", "Summary of Proposal Overview", placeholder = "Describe the proposed development activity and the ecological and biodiversity features to be impacted. [500 words]", rows = 6, width = "100%"),
                               tags$br(),
                               textAreaInput("ecological_context", "Ecological Context and Impact Summary", placeholder = "Summarise the context of the ecological and biodiversity features impacted by the proposed development. [500 words]", rows = 6, width = "100%"),
                               tags$br(),
                               textAreaInput("biodiversity_impacts", "Biodiversity Impacts Addressed Outside of the Model", placeholder = "Summarise any additional ecological values at the site that may be affected by the activity but are not included in this model. [500 words]", rows = 6, width = "100%"),
                               tags$br(),
                               textAreaInput("offset_package", "Summary Description of Proposed Offset Package", placeholder = "Describe the proposed offset package. [500 words]", rows = 6, width = "100%")
                           ),
                           div(style = "text-align: center;",
                               actionButton("go_to_inputs", "Proceed to Project Calculations >", class = "btn btn-dark-green", style = "margin-bottom: 20px;")
                           )
                  ),

                  # * Project Calculations UI ------------------------------------------------------

                  tabPanel("Project Calculations",

                           # __Required field legend ------------------------------------------------
                           tags$p(style = "font-size: 0.85em; color: #555; margin: 6px 0 0;",
                                  tags$span(" *", style = "color:#c62828; font-weight:bold;"),
                                  " indicates a required field"),

                           # __Excel Upload / Template ------------------------------------------------
                           div(
                             style = "margin: 10px 0; padding: 12px; background: #f0f7f4; border: 1px solid #c8e6c9; border-radius: 6px;",
                             tags$p(style = "font-weight: 600; margin-bottom: 8px;",
                                    icon("file-excel"), " Bulk Data Entry via Excel"),
                             tags$p(style = "font-size: 0.85em; color: #555; margin-bottom: 10px;",
                                    "Download the template, fill in your data, and upload to populate all attributes at once. This will replace any existing data."),
                             div(style = "display: flex; gap: 12px; align-items: center;",
                                 downloadButton("xlsx_template_download", "Download Template",
                                                class = "btn btn-dark-green", icon = icon("download")),
                                 actionButton("xlsx_upload_btn", "Upload Data",
                                              class = "btn btn-dark-green", icon = icon("upload"))
                             ),
                             # Validation results area (initially hidden)
                             uiOutput("xlsx_validation_ui")
                           ),

                           # __Biodiversity ----------------------------------------------------------
                           div(class = "colored-box red-box form-section-box",
                               div(class = "box-header", "Biodiversity"),
                               div(class = "compact-grid",
                                   div(class = "input-block", req_label("Biodiversity Type", `for` = "biodiversity_type"), textInput("biodiversity_type", label = NULL, value = "")),
                                   div(class = "input-block", req_label("Biodiversity Component", `for` = "biodiversity_component"), textInput("biodiversity_component", label = NULL, value = "")),
                                   div(class = "input-block", req_label("Biodiversity Attribute", `for` = "biodiversity_attribute"), textInput("biodiversity_attribute", label = NULL, value = "")),
                                   div(class = "input-block", req_label("Measurement Unit", `for` = "measurement_unit"), textInput("measurement_unit", label = NULL, value = "")),
                                   div(class = "input-block", req_label("Benchmark Value"), numericInput("benchmark_value", label = NULL, value = NULL, width = "100%")),
                                   div(class = "input-block", req_label("Statistical Distribution"), selectInput("distribution", label = NULL, choices = c("","Normal", "Poisson", "Negative Binomial"), selected = NULL, width = "100%")),
                                   # Distribution hint panel
                                   conditionalPanel(
                                     condition = "input.distribution == 'Poisson'",
                                     div(style = "grid-column: 1 / -1; background:#e8f4fd; border-left:4px solid #1a73e8; padding:8px 12px; border-radius:4px; font-size:0.875em;",
                                         HTML("<strong>Poisson distribution selected.</strong> Enter the expected count (mean &lambda;) and sample size (<em>n</em>) for each measurement period. Standard deviation is derived via Method of Moments: <strong>SD = &radic;&lambda; / &radic;n</strong>. All simulations use <em>rpois(&lambda;)</em> internally.")
                                     )
                                   ),
                                   conditionalPanel(
                                     condition = "input.distribution == 'Negative Binomial'",
                                     div(style = "grid-column: 1 / -1; background:#f3e8fd; border-left:4px solid #7b1fa2; padding:8px 12px; border-radius:4px; font-size:0.875em;",
                                         HTML("<strong>Negative Binomial distribution selected.</strong> Enter the expected mean (&mu;) and observed variance (s&sup2;) for each measurement period. The dispersion parameter k is derived automatically via Method of Moments: <strong>k = &mu;&sup2; / (s&sup2; &minus; &mu;)</strong>. Variance must be greater than the mean &mdash; if not, use a Poisson or Normal distribution instead.")
                                     )
                                   )
                                   # # Remove number of simulations as it's baked in
                                   # div(class = "input-block", tags$label("Number of Simulations", `for` = "num_simulations"), numericInput("num_simulations", label = NULL, value = NULL, min = 10, step = 10))
                               )
                           ),

                           div(class = "two-col-panels",
                               # __Impact -----------------------------------------
                               div(class = "colored-box blue-box form-section-box",
                                   div(class = "box-header", "Impact Site"),
                                   div(class = "compact-grid",

                                       # Impact Area (always start on a new row)
                                       div(class = "input-block row-start",  # Impact Area
                                           req_label("Impact Area"),
                                           numericInput("impact_area", label = NULL, value = "", width = "100%")
                                       ),
                                       # Impact Area Unit
                                       div(class = "input-block",
                                           req_label("Impact Area Measurement Unit"),
                                           selectInput("impact_area_unit", label = NULL, choices = c("","km","m","ha","m²"), width = "100%")
                                       ),

                                       # Impact Area Data Type + conditionals (always start on a new row)
                                       div(class = "input-block  row-start",
                                           req_label("Attribute Measure Data Type"),
                                           selectInput("impact_area_data_type", label = NULL,
                                                       choices = c("","Empirical", "Modelled", "Expert elicited", "Proxy value"),
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
                                       conditionalPanel(
                                         condition = "!(input.impact_area_data_type == 'Empirical' ||
                                                     input.impact_area_data_type == 'Modelled' ||
                                                     input.impact_area_data_type == 'Expert elicited' ||
                                                     input.impact_area_data_type == 'Proxy value')",
                                         # Greyed-out textarea (disabled)
                                         tags$div(
                                           style = "opacity: 0.5; pointer-events: none;",
                                           textAreaInput("impact_sd_disabled",
                                                         "Data Type Description",
                                                         "",
                                                         placeholder = "250 words",
                                                         width = "100%")
                                         )
                                       ),
                                       # ── Prior Impact measurement ───────────────────────────────────────────
                                       div(class = "input-block row-start",
                                           req_label("Prior Impact — Data Type"),
                                           selectInput("prior_impact_data_type", label = NULL,
                                                       choices = c("","Empirical","Modelled","Expert elicited","Proxy value"),
                                                       width = "100%")
                                       ),
                                       # Non-expert justification panels
                                       conditionalPanel(
                                         condition = "input.prior_impact_data_type == 'Empirical'",
                                         div(class="input-block", textAreaInput("prior_impact_empirical_details","Provide sample size & duration:","",placeholder="250 words",width="100%",rows=3))
                                       ),
                                       conditionalPanel(
                                         condition = "input.prior_impact_data_type == 'Modelled'",
                                         div(class="input-block", textAreaInput("prior_impact_modelled_details","Describe the model:","",placeholder="250 words",width="100%",rows=3))
                                       ),
                                       conditionalPanel(
                                         condition = "input.prior_impact_data_type == 'Proxy value'",
                                         div(class="input-block", textAreaInput("prior_impact_proxy_details","Give reference and/or describe:","",placeholder="250 words",width="100%",rows=3))
                                       ),
                                       # Direct numeric inputs (hidden when Expert elicited)
                                       conditionalPanel(
                                         condition = "input.prior_impact_data_type != 'Expert elicited'",
                                         div(class = "input-block row-start",
                                             req_label("Mean Attribute Measure Prior To Impact"),
                                             numericInput("mx_prior_impact_mean", label = NULL, value = "", width = "100%")
                                         ),
                                         conditionalPanel(
                                           condition = "input.distribution == 'Normal' || input.distribution == ''",
                                           div(class = "input-block",
                                               req_label("SD Prior Impact"),
                                               numericInput("mx_prior_impact_sd", label = NULL, value = "", width = "100%"))
                                         ),
                                         conditionalPanel(
                                           condition = "input.distribution == 'Poisson'",
                                           div(class = "input-block",
                                               req_label(HTML("Sample Size Prior to Impact <small><em>(n — MoM: SD=&radic;&lambda;/&radic;n)</em></small>")),
                                               numericInput("impact_sample_size_prior", label = NULL, value = "", min = 1, step = 1, width = "100%"))
                                         ),
                                         conditionalPanel(
                                           condition = "input.distribution == 'Negative Binomial'",
                                           div(class = "input-block",
                                               req_label(HTML("Variance Prior to Impact <small><em>(must be &gt; mean; k derived as &mu;&sup2;/(s&sup2;&minus;&mu;))</em></small>")),
                                               numericInput("mx_prior_impact_var", label = NULL, value = "", min = 0, step = 0.1, width = "100%"))
                                         )
                                       ),
                                       # SHELF expert elicitation panel — Prior Impact
                                       conditionalPanel(
                                         condition = "input.prior_impact_data_type == 'Expert elicited'",
                                         div(style="grid-column:1/-1;background:#fff8e1;border-left:4px solid #f9a825;padding:10px 14px 8px;border-radius:4px;margin:4px 0;",
                                           tags$p(style="font-weight:600;margin-bottom:4px;font-size:0.9em;",
                                                  HTML("&#128101; SHELF Elicitation &mdash; <em>Prior to Impact</em>")),
                                           tags$p(style="font-size:0.82em;color:#555;margin-bottom:8px;",
                                                  "Enter the expert plausible range as percentiles. Mean and SD are derived automatically."),
                                           fluidRow(
                                             column(4, numericInput("prior_impact_p_low",  HTML("Lower bound <small>(P5 or P25)</small>"),  value=NULL, width="100%")),
                                             column(4, numericInput("prior_impact_p50",    HTML("Median <small>(P50)</small>"),             value=NULL, width="100%")),
                                             column(4, numericInput("prior_impact_p_high", HTML("Upper bound <small>(P95 or P75)</small>"), value=NULL, width="100%"))
                                           ),
                                           fluidRow(
                                             column(6, selectInput("prior_impact_ci_level",
                                                                   HTML("Credible interval <small>(% spanned by low–high)</small>"),
                                                                   choices=c("90% (P5–P95)"="0.90","80% (P10–P90)"="0.80","50% (P25–P75)"="0.50"),
                                                                   selected="0.90", width="100%")),
                                             column(6, textAreaInput("prior_impact_expert_affiliations","Expert(s) and affiliations:",
                                                                     placeholder="List names & organisations", width="100%", rows=2))
                                           ),
                                           div(style="margin-top:4px;", uiOutput("prior_impact_shelf_derived"))
                                         )
                                       ),

                                       # ── Post Impact measurement ────────────────────────────────────────────
                                       div(class = "input-block row-start",
                                           req_label("Post Impact — Data Type"),
                                           selectInput("post_impact_data_type", label = NULL,
                                                       choices = c("","Empirical","Modelled","Expert elicited","Proxy value"),
                                                       width = "100%")
                                       ),
                                       conditionalPanel(
                                         condition = "input.post_impact_data_type == 'Empirical'",
                                         div(class="input-block", textAreaInput("post_impact_empirical_details","Provide sample size & duration:","",placeholder="250 words",width="100%",rows=3))
                                       ),
                                       conditionalPanel(
                                         condition = "input.post_impact_data_type == 'Modelled'",
                                         div(class="input-block", textAreaInput("post_impact_modelled_details","Describe the model:","",placeholder="250 words",width="100%",rows=3))
                                       ),
                                       conditionalPanel(
                                         condition = "input.post_impact_data_type == 'Proxy value'",
                                         div(class="input-block", textAreaInput("post_impact_proxy_details","Give reference and/or describe:","",placeholder="250 words",width="100%",rows=3))
                                       ),
                                       conditionalPanel(
                                         condition = "input.post_impact_data_type != 'Expert elicited'",
                                         div(class = "input-block row-start",
                                             req_label("Mean Attribute Measure Post Impact"),
                                             numericInput("mx_post_impact_mean", label = NULL, value = "", width = "100%")
                                         ),
                                         conditionalPanel(
                                           condition = "input.distribution == 'Normal' || input.distribution == ''",
                                           div(class = "input-block",
                                               req_label("SD Post Impact"),
                                               numericInput("mx_post_impact_sd", label = NULL, value = "", width = "100%"))
                                         ),
                                         conditionalPanel(
                                           condition = "input.distribution == 'Poisson'",
                                           div(class = "input-block",
                                               req_label(HTML("Sample Size Post Impact <small><em>(n)</em></small>")),
                                               numericInput("impact_sample_size_post", label = NULL, value = "", min = 1, step = 1, width = "100%"))
                                         ),
                                         conditionalPanel(
                                           condition = "input.distribution == 'Negative Binomial'",
                                           div(class = "input-block",
                                               req_label(HTML("Variance Post Impact <small><em>(must be &gt; mean; k derived as &mu;&sup2;/(s&sup2;&minus;&mu;))</em></small>")),
                                               numericInput("mx_post_impact_var", label = NULL, value = "", min = 0, step = 0.1, width = "100%"))
                                         )
                                       ),
                                       conditionalPanel(
                                         condition = "input.post_impact_data_type == 'Expert elicited'",
                                         div(style="grid-column:1/-1;background:#fff8e1;border-left:4px solid #f9a825;padding:10px 14px 8px;border-radius:4px;margin:4px 0;",
                                           tags$p(style="font-weight:600;margin-bottom:4px;font-size:0.9em;",
                                                  HTML("&#128101; SHELF Elicitation &mdash; <em>Post Impact</em>")),
                                           tags$p(style="font-size:0.82em;color:#555;margin-bottom:8px;",
                                                  "Enter the expert plausible range as percentiles. Mean and SD are derived automatically."),
                                           fluidRow(
                                             column(4, numericInput("post_impact_p_low",  HTML("Lower bound <small>(P5 or P25)</small>"),  value=NULL, width="100%")),
                                             column(4, numericInput("post_impact_p50",    HTML("Median <small>(P50)</small>"),             value=NULL, width="100%")),
                                             column(4, numericInput("post_impact_p_high", HTML("Upper bound <small>(P95 or P75)</small>"), value=NULL, width="100%"))
                                           ),
                                           fluidRow(
                                             column(6, selectInput("post_impact_ci_level",
                                                                   HTML("Credible interval <small>(% spanned by low–high)</small>"),
                                                                   choices=c("90% (P5–P95)"="0.90","80% (P10–P90)"="0.80","50% (P25–P75)"="0.50"),
                                                                   selected="0.90", width="100%")),
                                             column(6, textAreaInput("post_impact_expert_affiliations","Expert(s) and affiliations:",
                                                                     placeholder="List names & organisations", width="100%", rows=2))
                                           ),
                                           div(style="margin-top:4px;", uiOutput("post_impact_shelf_derived"))
                                         )
                                       )
                                   )
                               ),

                               # __Offset -----------------------------------------
                               div(class = "colored-box purple-box form-section-box",
                                   div(class = "box-header", "Offset Site"),
                                   div(class = "compact-grid offset-grid",
                                       div(class = "input-block", req_label("Offset Area"), numericInput("offset_area", label = NULL, value = "", width = "100%")),
                                       div(class = "input-block", req_label("Offset Area Unit"), selectInput("offset_area_unit", label = NULL, choices = c("","km","m", "ha", "m²"), width = "100%")),
                                       # ── Prior Offset measurement ───────────────────────────────────────────
                                       div(class = "input-block row-start",
                                           req_label("Prior Offset — Data Type"),
                                           selectInput("prior_offset_data_type", label = NULL,
                                                       choices = c("","Empirical","Modelled","Expert elicited","Proxy value"),
                                                       width = "100%")
                                       ),
                                       conditionalPanel(
                                         condition = "input.prior_offset_data_type == 'Empirical'",
                                         div(class="input-block", textAreaInput("prior_offset_empirical_details","Provide sample size & duration:","",placeholder="250 words",width="100%",rows=3))
                                       ),
                                       conditionalPanel(
                                         condition = "input.prior_offset_data_type == 'Modelled'",
                                         div(class="input-block", textAreaInput("prior_offset_modelled_details","Describe the model:","",placeholder="250 words",width="100%",rows=3))
                                       ),
                                       conditionalPanel(
                                         condition = "input.prior_offset_data_type == 'Proxy value'",
                                         div(class="input-block", textAreaInput("prior_offset_proxy_details","Give reference and/or describe:","",placeholder="250 words",width="100%",rows=3))
                                       ),
                                       conditionalPanel(
                                         condition = "input.prior_offset_data_type != 'Expert elicited'",
                                         div(class = "input-block row-start",
                                             req_label("Mean Attribute Measure Prior To Offset"),
                                             numericInput("mx_prior_offset_mean", label = NULL, value = "", width = "100%")
                                         ),
                                         conditionalPanel(
                                           condition = "input.distribution == 'Normal' || input.distribution == ''",
                                           div(class = "input-block",
                                               req_label("SD Prior Offset"),
                                               numericInput("mx_prior_offset_sd", label = NULL, value = "", width = "100%"))
                                         ),
                                         conditionalPanel(
                                           condition = "input.distribution == 'Poisson'",
                                           div(class = "input-block",
                                               req_label(HTML("Sample Size Prior to Offset <small><em>(n — MoM: SD=&radic;&lambda;/&radic;n)</em></small>")),
                                               numericInput("offset_sample_size_prior", label = NULL, value = "", min = 1, step = 1, width = "100%"))
                                         ),
                                         conditionalPanel(
                                           condition = "input.distribution == 'Negative Binomial'",
                                           div(class = "input-block",
                                               req_label(HTML("Variance Prior to Offset <small><em>(must be &gt; mean; k derived as &mu;&sup2;/(s&sup2;&minus;&mu;))</em></small>")),
                                               numericInput("mx_prior_offset_var", label = NULL, value = "", min = 0, step = 0.1, width = "100%"))
                                         )
                                       ),
                                       conditionalPanel(
                                         condition = "input.prior_offset_data_type == 'Expert elicited'",
                                         div(style="grid-column:1/-1;background:#fff8e1;border-left:4px solid #f9a825;padding:10px 14px 8px;border-radius:4px;margin:4px 0;",
                                           tags$p(style="font-weight:600;margin-bottom:4px;font-size:0.9em;",
                                                  HTML("&#128101; SHELF Elicitation &mdash; <em>Prior to Offset</em>")),
                                           tags$p(style="font-size:0.82em;color:#555;margin-bottom:8px;",
                                                  "Enter the expert plausible range as percentiles. Mean and SD are derived automatically."),
                                           fluidRow(
                                             column(4, numericInput("prior_offset_p_low",  HTML("Lower bound <small>(P5 or P25)</small>"),  value=NULL, width="100%")),
                                             column(4, numericInput("prior_offset_p50",    HTML("Median <small>(P50)</small>"),             value=NULL, width="100%")),
                                             column(4, numericInput("prior_offset_p_high", HTML("Upper bound <small>(P95 or P75)</small>"), value=NULL, width="100%"))
                                           ),
                                           fluidRow(
                                             column(6, selectInput("prior_offset_ci_level",
                                                                   HTML("Credible interval <small>(% spanned by low–high)</small>"),
                                                                   choices=c("90% (P5–P95)"="0.90","80% (P10–P90)"="0.80","50% (P25–P75)"="0.50"),
                                                                   selected="0.90", width="100%")),
                                             column(6, textAreaInput("prior_offset_expert_affiliations","Expert(s) and affiliations:",
                                                                     placeholder="List names & organisations", width="100%", rows=2))
                                           ),
                                           div(style="margin-top:4px;", uiOutput("prior_offset_shelf_derived"))
                                         )
                                       ),

                                       # ── Post Offset measurement ────────────────────────────────────────────
                                       div(class = "input-block row-start",
                                           req_label("Post Offset — Data Type"),
                                           selectInput("post_offset_data_type", label = NULL,
                                                       choices = c("","Empirical","Modelled","Expert elicited","Proxy value"),
                                                       width = "100%")
                                       ),
                                       conditionalPanel(
                                         condition = "input.post_offset_data_type == 'Empirical'",
                                         div(class="input-block", textAreaInput("post_offset_empirical_details","Provide sample size & duration:","",placeholder="250 words",width="100%",rows=3))
                                       ),
                                       conditionalPanel(
                                         condition = "input.post_offset_data_type == 'Modelled'",
                                         div(class="input-block", textAreaInput("post_offset_modelled_details","Describe the model:","",placeholder="250 words",width="100%",rows=3))
                                       ),
                                       conditionalPanel(
                                         condition = "input.post_offset_data_type == 'Proxy value'",
                                         div(class="input-block", textAreaInput("post_offset_proxy_details","Give reference and/or describe:","",placeholder="250 words",width="100%",rows=3))
                                       ),
                                       conditionalPanel(
                                         condition = "input.post_offset_data_type != 'Expert elicited'",
                                         div(class = "input-block row-start",
                                             req_label("Mean Attribute Measure Post Offset"),
                                             numericInput("mx_post_offset_mean", label = NULL, value = "", width = "100%")
                                         ),
                                         conditionalPanel(
                                           condition = "input.distribution == 'Normal' || input.distribution == ''",
                                           div(class = "input-block",
                                               req_label("SD Post Offset"),
                                               numericInput("mx_post_offset_sd", label = NULL, value = "", width = "100%"))
                                         ),
                                         conditionalPanel(
                                           condition = "input.distribution == 'Poisson'",
                                           div(class = "input-block",
                                               req_label(HTML("Sample Size Post Offset <small><em>(n)</em></small>")),
                                               numericInput("offset_sample_size_post", label = NULL, value = "", min = 1, step = 1, width = "100%"))
                                         ),
                                         conditionalPanel(
                                           condition = "input.distribution == 'Negative Binomial'",
                                           div(class = "input-block",
                                               req_label(HTML("Variance Post Offset <small><em>(must be &gt; mean; k derived as &mu;&sup2;/(s&sup2;&minus;&mu;))</em></small>")),
                                               numericInput("mx_post_offset_var", label = NULL, value = "", min = 0, step = 0.1, width = "100%"))
                                         )
                                       ),
                                       conditionalPanel(
                                         condition = "input.post_offset_data_type == 'Expert elicited'",
                                         div(style="grid-column:1/-1;background:#fff8e1;border-left:4px solid #f9a825;padding:10px 14px 8px;border-radius:4px;margin:4px 0;",
                                           tags$p(style="font-weight:600;margin-bottom:4px;font-size:0.9em;",
                                                  HTML("&#128101; SHELF Elicitation &mdash; <em>Post Offset</em>")),
                                           tags$p(style="font-size:0.82em;color:#555;margin-bottom:8px;",
                                                  "Enter the expert plausible range as percentiles. Mean and SD are derived automatically."),
                                           fluidRow(
                                             column(4, numericInput("post_offset_p_low",  HTML("Lower bound <small>(P5 or P25)</small>"),  value=NULL, width="100%")),
                                             column(4, numericInput("post_offset_p50",    HTML("Median <small>(P50)</small>"),             value=NULL, width="100%")),
                                             column(4, numericInput("post_offset_p_high", HTML("Upper bound <small>(P95 or P75)</small>"), value=NULL, width="100%"))
                                           ),
                                           fluidRow(
                                             column(6, selectInput("post_offset_ci_level",
                                                                   HTML("Credible interval <small>(% spanned by low–high)</small>"),
                                                                   choices=c("90% (P5–P95)"="0.90","80% (P10–P90)"="0.80","50% (P25–P75)"="0.50"),
                                                                   selected="0.90", width="100%")),
                                             column(6, textAreaInput("post_offset_expert_affiliations","Expert(s) and affiliations:",
                                                                     placeholder="List names & organisations", width="100%", rows=2))
                                           ),
                                           div(style="margin-top:4px;", uiOutput("post_offset_shelf_derived"))
                                         )
                                       ),
                                       # Pairs that should share a row in 3-col mode
                                       div(class = "input-block conf-level",
                                           req_label("Confidence in Proposed Offset Action", `for` = "selected_confidence"),
                                           selectInput("selected_confidence", label = NULL, choices = confidence_levels$levels, selected = "")
                                       ),
                                       div(class = "input-block conf-justify",
                                           textAreaInput("offset_confidence_justify", "Justify Confidence in Proposed Offset Action:", placeholder = "250 words", "", width = "100%", rows = 3)
                                       ),

                                       div(class = "input-block end-time",
                                           req_label("Time till End (Years)"),
                                           numericInput("time_till_end", label = NULL, value = "", width = "100%")
                                       ),
                                       div(class = "input-block end-time-justify",
                                           textAreaInput("offset_time_till_end_justify", "Justify time till end:", placeholder = "250 words", "", width = "100%", rows = 3)
                                       ),

                                       div(class = "input-block discount-rate",
                                           req_label("Discount Rate (%)"),
                                           numericInput("discount_rate", label = NULL, value = "", width = "100%", step = 0.1)
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
                  ),
      )
  )
)
