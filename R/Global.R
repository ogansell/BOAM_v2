# Load Required Packages --------------------------------------------------
library(shiny)
library(dplyr)
library(bslib)
library(rmarkdown)
library(lorem)
library(DT)
library(shinyjs)
library(kableExtra)
library(stringr)
library(openxlsx)

source('./R/general_funcs.R')
source('./R/calculation_funcs.R')
source('./R/ui_funcs.R')

addResourcePath("custom", "www")

# === Idle reminder config (minutes) ============================================
IDLE_MODAL_MIN <- 30
# ==============================================================================

# Confidence Levels -------------------------------------------------------
confidence_levels <- data.frame(
  levels = c("","Low confidence >50% <75%", "Confident 75-90%", "Very confident >90%"),
  scores = c(NA, 0.62, 0.825, 0.955)
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

# These must all contain valid values for the calculate button to work
# Base numeric IDs always required (distribution-specific IDs handled dynamically in validate_and_run_simulation)
numeric_ids <- c(
  "mx_prior_impact_mean", "mx_prior_impact_sd",
  "mx_post_impact_mean",  "mx_post_impact_sd",
  "mx_prior_offset_mean", "mx_prior_offset_sd",
  "mx_post_offset_mean",  "mx_post_offset_sd",
  "benchmark_value",
  "impact_area",
  "offset_area",
  "discount_rate",
  "time_till_end",
  # Poisson sample sizes (conditional — used for clearing red outlines only)
  "impact_sample_size_prior", "impact_sample_size_post",
  "offset_sample_size_prior", "offset_sample_size_post",
  # Negative Binomial variance inputs (conditional)
  "mx_prior_impact_var", "mx_post_impact_var",
  "mx_prior_offset_var", "mx_post_offset_var"
)

# ---- Draft: fields to capture/restore ----
# IMPORTANT: These IDs must match the actual input IDs in ui.R exactly.
draft_fields <- c(
  # Project Details
  "project_name","prepared_by","date",
  "proposal_overview","ecological_context","biodiversity_impacts","offset_package",
  
  # Project Calculations - Biodiversity
  "biodiversity_type","biodiversity_component","biodiversity_attribute",
  "measurement_unit","benchmark_value","distribution",
  
  # Impact Area
  "impact_area_unit","impact_area","impact_area_data_type",
  "impact_area_empirical_details","impact_area_modelled_details",
  "impact_area_expert_details","impact_area_proxy_details",
  
  # Prior Impact
  "mx_prior_impact_mean","mx_prior_impact_sd",
  "impact_sample_size_prior",   # Poisson
  "mx_prior_impact_var",        # Negative Binomial
  "prior_impact_data_type",
  "prior_impact_empirical_details","prior_impact_modelled_details",
  "prior_impact_expert_affiliations","prior_impact_proxy_details",
  # SHELF prior impact
  "prior_impact_p_low","prior_impact_p50","prior_impact_p_high","prior_impact_ci_level",
  
  # Post Impact
  "mx_post_impact_mean","mx_post_impact_sd",
  "impact_sample_size_post",    # Poisson
  "mx_post_impact_var",         # Negative Binomial
  "post_impact_data_type",
  "post_impact_empirical_details","post_impact_modelled_details",
  "post_impact_expert_affiliations","post_impact_proxy_details",
  # SHELF post impact
  "post_impact_p_low","post_impact_p50","post_impact_p_high","post_impact_ci_level",
  
  # Offset Area
  "offset_area_unit","offset_area",
  
  # Prior Offset
  "mx_prior_offset_mean","mx_prior_offset_sd",
  "offset_sample_size_prior",   # Poisson
  "mx_prior_offset_var",        # Negative Binomial
  "prior_offset_data_type",
  "prior_offset_empirical_details","prior_offset_modelled_details",
  "prior_offset_expert_affiliations","prior_offset_proxy_details",
  # SHELF prior offset
  "prior_offset_p_low","prior_offset_p50","prior_offset_p_high","prior_offset_ci_level",
  
  # Post Offset
  "mx_post_offset_mean","mx_post_offset_sd",
  "offset_sample_size_post",    # Poisson
  "mx_post_offset_var",         # Negative Binomial
  "post_offset_data_type",
  "post_offset_empirical_details","post_offset_modelled_details",
  "post_offset_expert_affiliations","post_offset_proxy_details",
  # SHELF post offset
  "post_offset_p_low","post_offset_p50","post_offset_p_high","post_offset_ci_level",
  
  # Offset confidence / time / discount
  "selected_confidence", "offset_confidence_justify",
  "time_till_end", "offset_time_till_end_justify",
  "discount_rate", "offset_discount_rate_justify"
)

# Everything in Project Calculations = draft_fields minus Project Details fields
project_calc_fields <- setdiff(
  draft_fields,
  c("project_name","prepared_by","date",
    "proposal_overview","ecological_context","biodiversity_impacts","offset_package")
)