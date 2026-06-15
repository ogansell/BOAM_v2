
# Label helper: appends a red asterisk to mark a required field
req_label <- function(...) {
  tags$label(..., tags$span(" *", style = "color:#c62828; font-weight:bold;"))
}

# Reset ALL inputs on the "Project Calculations" tab to their initial defaults
reset_project_calc_inputs <- function(session) {
  # --- Biodiversity ---
  # updateTextInput(   session, "biodiversity_type",      value   = "")
  # updateTextInput(   session, "biodiversity_component", value   = "Open water")
  updateTextInput(   session, "biodiversity_attribute", value   = "")
  updateTextInput(   session, "measurement_unit",       value   = "")
  updateNumericInput(session, "benchmark_value",        value   = "")
  updateSelectInput( session, "distribution",           selected= "")
  #updateNumericInput(session, "num_simulations",        value   = "")
  
  # --- Impact ---
  updateSelectInput( session, "impact_area_unit",       selected= "")
  updateNumericInput(session, "impact_area",            value   = "")
  updateSelectInput( session, "impact_area_data_type",  selected= "")
  updateTextAreaInput(session, "impact_area_empirical_details", value = "")
  updateTextAreaInput(session, "impact_area_modelled_details",  value = "")
  updateTextAreaInput(session, "impact_area_expert_details",    value = "")
  updateTextAreaInput(session, "impact_area_proxy_details",     value = "")
  
  # Prior impact measurement point
  updateSelectInput( session, "prior_impact_data_type",       selected = "")
  updateTextAreaInput(session, "prior_impact_empirical_details", value = "")
  updateTextAreaInput(session, "prior_impact_modelled_details",  value = "")
  updateTextAreaInput(session, "prior_impact_proxy_details",     value = "")
  updateTextAreaInput(session, "prior_impact_expert_affiliations", value = "")
  updateNumericInput(session, "mx_prior_impact_mean",   value = "")
  updateNumericInput(session, "mx_prior_impact_sd",     value = "")
  updateNumericInput(session, "prior_impact_p_low",     value = "")
  updateNumericInput(session, "prior_impact_p50",       value = "")
  updateNumericInput(session, "prior_impact_p_high",    value = "")
  updateSelectInput( session, "prior_impact_ci_level",  selected = "0.90")
  updateNumericInput(session, "impact_sample_size_prior", value = "")
  updateNumericInput(session, "mx_prior_impact_var",        value = "")
  # Post impact measurement point
  updateSelectInput( session, "post_impact_data_type",        selected = "")
  updateTextAreaInput(session, "post_impact_empirical_details",  value = "")
  updateTextAreaInput(session, "post_impact_modelled_details",   value = "")
  updateTextAreaInput(session, "post_impact_proxy_details",      value = "")
  updateTextAreaInput(session, "post_impact_expert_affiliations", value = "")
  updateNumericInput(session, "mx_post_impact_mean",    value = "")
  updateNumericInput(session, "mx_post_impact_sd",      value = "")
  updateNumericInput(session, "post_impact_p_low",      value = "")
  updateNumericInput(session, "post_impact_p50",        value = "")
  updateNumericInput(session, "post_impact_p_high",     value = "")
  updateSelectInput( session, "post_impact_ci_level",   selected = "0.90")
  updateNumericInput(session, "impact_sample_size_post", value = "")
  updateNumericInput(session, "mx_post_impact_var",         value = "")
  
  # --- Offset ---
  updateSelectInput( session, "offset_area_unit", selected = "")
  updateNumericInput(session, "offset_area",            value   = "")
  updateNumericInput(session, "mx_prior_offset_mean",   value   = "")
  updateNumericInput(session, "mx_prior_offset_sd",     value   = "")
  updateSelectInput( session, "prior_offset_sd_data_type", selected = "")
  updateTextAreaInput(session, "prior_offset_sd_empirical_details", value = "")
  updateTextAreaInput(session, "prior_offset_sd_modelled_details",  value = "")
  updateTextAreaInput(session, "prior_offset_sd_expert_details",    value = "")
  updateTextAreaInput(session, "prior_offset_sd_proxy_details",     value = "")
  
  # Prior offset measurement point
  updateSelectInput( session, "prior_offset_data_type",       selected = "")
  updateTextAreaInput(session, "prior_offset_empirical_details", value = "")
  updateTextAreaInput(session, "prior_offset_modelled_details",  value = "")
  updateTextAreaInput(session, "prior_offset_proxy_details",     value = "")
  updateTextAreaInput(session, "prior_offset_expert_affiliations", value = "")
  updateNumericInput(session, "mx_prior_offset_mean",   value = "")
  updateNumericInput(session, "mx_prior_offset_sd",     value = "")
  updateNumericInput(session, "prior_offset_p_low",     value = "")
  updateNumericInput(session, "prior_offset_p50",       value = "")
  updateNumericInput(session, "prior_offset_p_high",    value = "")
  updateSelectInput( session, "prior_offset_ci_level",  selected = "0.90")
  updateNumericInput(session, "offset_sample_size_prior", value = "")
  updateNumericInput(session, "mx_prior_offset_var",        value = "")
  # Post offset measurement point
  updateSelectInput( session, "post_offset_data_type",        selected = "")
  updateTextAreaInput(session, "post_offset_empirical_details",  value = "")
  updateTextAreaInput(session, "post_offset_modelled_details",   value = "")
  updateTextAreaInput(session, "post_offset_proxy_details",      value = "")
  updateTextAreaInput(session, "post_offset_expert_affiliations", value = "")
  updateNumericInput(session, "mx_post_offset_mean",    value = "")
  updateNumericInput(session, "mx_post_offset_sd",      value = "")
  updateNumericInput(session, "post_offset_p_low",      value = "")
  updateNumericInput(session, "post_offset_p50",        value = "")
  updateNumericInput(session, "post_offset_p_high",     value = "")
  updateSelectInput( session, "post_offset_ci_level",   selected = "0.90")
  updateNumericInput(session, "offset_sample_size_post", value = "")
  updateNumericInput(session, "mx_post_offset_var",         value = "")
  
  updateSelectInput( session, "selected_confidence",    selected= "")
  updateTextAreaInput(session, "offset_confidence_justify",   value = "")
  
  updateNumericInput(session, "time_till_end",          value   = "")
  updateTextAreaInput(session, "offset_time_till_end_justify", value = "")
  
  updateNumericInput(session, "discount_rate",          value   = "")
  updateTextAreaInput(session, "offset_discount_rate_justify", value = "")
}


restore_inputs_from_list <- function(session, ins) {
  getOr <- function(x, nm, default = NULL) if (!is.null(x[[nm]])) x[[nm]] else default
  
  # --- Biodiversity ---
  updateTextInput(   session, "biodiversity_type",      value   = getOr(ins, "biodiversity_type", ""))
  updateTextInput(   session, "biodiversity_component", value   = getOr(ins, "biodiversity_component", ""))
  updateTextInput(   session, "biodiversity_attribute", value   = getOr(ins, "biodiversity_attribute", ""))
  updateTextInput(   session, "measurement_unit",       value   = getOr(ins, "measurement_unit", ""))
  updateNumericInput(session, "benchmark_value",        value   = getOr(ins, "benchmark_value", 30))
  updateSelectInput( session, "distribution",           selected= getOr(ins, "distribution", "Normal"))
  #updateNumericInput(session, "num_simulations",        value   = getOr(ins, "num_simulations", 100))
  
  # --- Impact ---
  updateSelectInput( session, "impact_area_unit",       selected= getOr(ins, "impact_area_unit", "m"))
  updateNumericInput(session, "impact_area",            value   = getOr(ins, "impact_area", 0.35))
  updateSelectInput( session, "impact_area_data_type",  selected= getOr(ins, "impact_area_data_type", "Empirical"))
  updateTextAreaInput(session, "impact_area_empirical_details", value = getOr(ins, "impact_area_empirical_details", ""))
  updateTextAreaInput(session, "impact_area_modelled_details",  value = getOr(ins, "impact_area_modelled_details", ""))
  updateTextAreaInput(session, "impact_area_expert_details",    value = getOr(ins, "impact_area_expert_details", ""))
  updateTextAreaInput(session, "impact_area_proxy_details",     value = getOr(ins, "impact_area_proxy_details", ""))
  
  # Prior impact measurement point
  updateSelectInput( session, "prior_impact_data_type",         selected = getOr(ins, "prior_impact_data_type", ""))
  updateTextAreaInput(session, "prior_impact_empirical_details", value   = getOr(ins, "prior_impact_empirical_details", ""))
  updateTextAreaInput(session, "prior_impact_modelled_details",  value   = getOr(ins, "prior_impact_modelled_details", ""))
  updateTextAreaInput(session, "prior_impact_proxy_details",     value   = getOr(ins, "prior_impact_proxy_details", ""))
  updateTextAreaInput(session, "prior_impact_expert_affiliations", value = getOr(ins, "prior_impact_expert_affiliations", ""))
  updateNumericInput(session, "mx_prior_impact_mean",   value  = getOr(ins, "mx_prior_impact_mean", NA))
  updateNumericInput(session, "mx_prior_impact_sd",     value  = getOr(ins, "mx_prior_impact_sd", NA))
  updateNumericInput(session, "prior_impact_p_low",     value  = getOr(ins, "prior_impact_p_low", NA))
  updateNumericInput(session, "prior_impact_p50",       value  = getOr(ins, "prior_impact_p50", NA))
  updateNumericInput(session, "prior_impact_p_high",    value  = getOr(ins, "prior_impact_p_high", NA))
  updateSelectInput( session, "prior_impact_ci_level",  selected = getOr(ins, "prior_impact_ci_level", "0.90"))
  updateNumericInput(session, "impact_sample_size_prior", value = getOr(ins, "impact_sample_size_prior", NA))
  updateNumericInput(session, "mx_prior_impact_var",        value = getOr(ins, "mx_prior_impact_var", NA))
  # Post impact measurement point
  updateSelectInput( session, "post_impact_data_type",          selected = getOr(ins, "post_impact_data_type", ""))
  updateTextAreaInput(session, "post_impact_empirical_details",  value   = getOr(ins, "post_impact_empirical_details", ""))
  updateTextAreaInput(session, "post_impact_modelled_details",   value   = getOr(ins, "post_impact_modelled_details", ""))
  updateTextAreaInput(session, "post_impact_proxy_details",      value   = getOr(ins, "post_impact_proxy_details", ""))
  updateTextAreaInput(session, "post_impact_expert_affiliations", value  = getOr(ins, "post_impact_expert_affiliations", ""))
  updateNumericInput(session, "mx_post_impact_mean",    value  = getOr(ins, "mx_post_impact_mean", NA))
  updateNumericInput(session, "mx_post_impact_sd",      value  = getOr(ins, "mx_post_impact_sd", NA))
  updateNumericInput(session, "post_impact_p_low",      value  = getOr(ins, "post_impact_p_low", NA))
  updateNumericInput(session, "post_impact_p50",        value  = getOr(ins, "post_impact_p50", NA))
  updateNumericInput(session, "post_impact_p_high",     value  = getOr(ins, "post_impact_p_high", NA))
  updateSelectInput( session, "post_impact_ci_level",   selected = getOr(ins, "post_impact_ci_level", "0.90"))
  updateNumericInput(session, "impact_sample_size_post", value = getOr(ins, "impact_sample_size_post", NA))
  updateNumericInput(session, "mx_post_impact_var",        value = getOr(ins, "mx_post_impact_var", NA))

  # --- Offset ---
  updateSelectInput( session, "offset_area_unit", selected = getOr(ins, "offset_area_unit", "m"))
  updateNumericInput(session, "offset_area",      value    = getOr(ins, "offset_area", 1))

  # Prior offset measurement point
  updateSelectInput( session, "prior_offset_data_type",         selected = getOr(ins, "prior_offset_data_type", ""))
  updateTextAreaInput(session, "prior_offset_empirical_details", value   = getOr(ins, "prior_offset_empirical_details", ""))
  updateTextAreaInput(session, "prior_offset_modelled_details",  value   = getOr(ins, "prior_offset_modelled_details", ""))
  updateTextAreaInput(session, "prior_offset_proxy_details",     value   = getOr(ins, "prior_offset_proxy_details", ""))
  updateTextAreaInput(session, "prior_offset_expert_affiliations", value = getOr(ins, "prior_offset_expert_affiliations", ""))
  updateNumericInput(session, "mx_prior_offset_mean",   value  = getOr(ins, "mx_prior_offset_mean", NA))
  updateNumericInput(session, "mx_prior_offset_sd",     value  = getOr(ins, "mx_prior_offset_sd", NA))
  updateNumericInput(session, "prior_offset_p_low",     value  = getOr(ins, "prior_offset_p_low", NA))
  updateNumericInput(session, "prior_offset_p50",       value  = getOr(ins, "prior_offset_p50", NA))
  updateNumericInput(session, "prior_offset_p_high",    value  = getOr(ins, "prior_offset_p_high", NA))
  updateSelectInput( session, "prior_offset_ci_level",  selected = getOr(ins, "prior_offset_ci_level", "0.90"))
  updateNumericInput(session, "offset_sample_size_prior", value = getOr(ins, "offset_sample_size_prior", NA))
  updateNumericInput(session, "mx_prior_offset_var",        value = getOr(ins, "mx_prior_offset_var", NA))
  # Post offset measurement point
  updateSelectInput( session, "post_offset_data_type",          selected = getOr(ins, "post_offset_data_type", ""))
  updateTextAreaInput(session, "post_offset_empirical_details",  value   = getOr(ins, "post_offset_empirical_details", ""))
  updateTextAreaInput(session, "post_offset_modelled_details",   value   = getOr(ins, "post_offset_modelled_details", ""))
  updateTextAreaInput(session, "post_offset_proxy_details",      value   = getOr(ins, "post_offset_proxy_details", ""))
  updateTextAreaInput(session, "post_offset_expert_affiliations", value  = getOr(ins, "post_offset_expert_affiliations", ""))
  updateNumericInput(session, "mx_post_offset_mean",    value  = getOr(ins, "mx_post_offset_mean", NA))
  updateNumericInput(session, "mx_post_offset_sd",      value  = getOr(ins, "mx_post_offset_sd", NA))
  updateNumericInput(session, "post_offset_p_low",      value  = getOr(ins, "post_offset_p_low", NA))
  updateNumericInput(session, "post_offset_p50",        value  = getOr(ins, "post_offset_p50", NA))
  updateNumericInput(session, "post_offset_p_high",     value  = getOr(ins, "post_offset_p_high", NA))
  updateSelectInput( session, "post_offset_ci_level",   selected = getOr(ins, "post_offset_ci_level", "0.90"))
  updateNumericInput(session, "offset_sample_size_post", value = getOr(ins, "offset_sample_size_post", NA))
  updateNumericInput(session, "mx_post_offset_var",        value = getOr(ins, "mx_post_offset_var", NA))
  
  updateSelectInput( session, "selected_confidence",    selected= getOr(ins, "selected_confidence", "Confident 75-90%"))
  updateTextAreaInput(session, "offset_confidence_justify",   value = getOr(ins, "offset_confidence_justify", ""))
  
  updateNumericInput(session, "time_till_end",          value   = getOr(ins, "time_till_end", 1))
  updateTextAreaInput(session, "offset_time_till_end_justify", value = getOr(ins, "offset_time_till_end_justify", ""))
  
  updateNumericInput(session, "discount_rate",          value   = getOr(ins, "discount_rate", 3))
  updateTextAreaInput(session, "offset_discount_rate_justify", value = getOr(ins, "offset_discount_rate_justify", ""))
}


