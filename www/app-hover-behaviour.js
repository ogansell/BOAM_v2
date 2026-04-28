$(function () {

  //---------------------------------------------------------------------------
  // TOOLTIP TEXT
  //---------------------------------------------------------------------------
  const tooltips = {
    project_name: 'If running scenarios or versions ensure this is identified in the project name',
    prepared_by: 'Enter your name and organisation',
    biodiversity_type: 'Biodiversity Types are the key biodiversity features of concern. A Biodiversity Type can be terrestiral or aquatic, an ecosystem, a specific habitat, or a species Each Biodiversity Type requires its own model to be run.',
    biodiversity_component: 'Biodiversity Types are the key biodiversity features of concern. A Biodiversity Type can be terrestiral or aquatic, an ecosystem, a specific habitat, or a species. Each Biodiversity Type requires its own model to be run.',
    biodiversity_attribute: 'Biodiversity Attributes collectively make up the Biodiversity Component. Biodiversity Attributes are the measured values balanced within the model. Biodiversity Attributes are equally weighted',
    measurement_unit: 'Measurement units must be relevant to the associated Biodiversity Attribute and the same unit of measurement must be used for both Impact and Offset data inputs ',
    num_simulations: 'Further detail on determining the number of simulations is provided in the Supplementary User Manual (see the ABOUT tab)',
    distribution: 'Further detail on appropriate statistical distribution is provided in the Supplementary User Manual (see the ABOUT tab)',
    benchmark_value: 'Benchmark values (reflecting the highest possible condition) provide a common reference point for evaluating losses and gains. Benchmark values are required for each Biodiversity Attribute and must in the same measures as used to describe the Biodiversity Attribute at the Impact Site. Further detail on benchmark values are provided in the User Manual (see the ABOUT tab)',
    
    selected_confidence: 'Hover over text inserted here - 12',
    
    impact_area: 'The area supporting the Biodiversity Type and over which the Biodiversity Attribute will be impacted by the proposed activity',
    impact_area_data_type: 'Indicate the way in which the Biodiversity Attribute was measured or estimated.',
   impact_area_empirical_details: 'A brief description of the methods used to derive the Mean Attribute Measure Prior to Impact. This information contextualises the reliability of the attribute estimate.',
    impact_area_modelled_details: 'A brief description of the methods used to derive the Mean Attribute Measure Prior to Impact. This information contextualises the reliability of the attribute estimate.',
    impact_area_expert_details: 'A brief description of the methods used to derive the Mean Attribute Measure Prior to Impact. This information contextualises the reliability of the attribute estimate.',
    impact_area_proxy_details: 'A brief description of the methods used to derive the Mean Attribute Measure Prior to Impact. This information contextualises the reliability of the attribute estimate.',
    mx_prior_impact_mean: 'Mean value of the Biodiversity Attribute before any  project impact occurs.',
    mx_prior_offset_mean: 'Hover over text inserted here - 16',
    mx_prior_impact_sd: 'The standard deviation of the Mean Attribute Measure Prior to Impact.',
    prior_impact_sd_data_type: 'The method used to derive the standard deviation of the mean attribute measure prior to impact.',
    prior_impact_sd_empirical_details: 'If applicable -- a brief description of the methods used to derive the mean Biodiversity Attribute Measure Post Impact. This information contextualises the reliability of the attribute estimate.',
    prior_impact_sd_modelled_details: 'If applicable -- a brief description of the methods used to derive the mean Biodiversity Attribute Measure Post Impact. This information contextualises the reliability of the attribute estimate.',
    prior_impact_sd_expert_details: 'If applicable -- a brief description of the methods used to derive the mean Biodiversity Attribute Measure Post Impact. This information contextualises the reliability of the attribute estimate.',
    prior_impact_sd_proxy_details: 'If applicable -- a brief description of the methods used to derive the mean Biodiversity Attribute Measure Post Impact. This information contextualises the reliability of the attribute estimate.',
    mx_post_impact_mean: 'Mean value of the Biodiversity Attribute before any  project impact occurs. ',
    mx_post_impact_sd: 'The expected standard deviation of the mean attribute measure post impact.',
    
    offset_area: 'The area over which the proposed offset action related to this Biodiversity Attribute will be implemented',

    mx_post_offset_mean: 'Mean value of the Biodiversity Attribute before prior to any Offset Action(s) being implemented. ',
    mx_prior_offset_sd: 'The standard deviation of the mean attribute measure prior to offset. ',
    
    prior_offset_sd_data_type: 'A brief description of the methods used to derive the mean attribute measure prior to offset. This information contextualises the reliability of the attribute estimate.',
    prior_offset_sd_empirical_details: 'A brief description of the methods used to derive the mean attribute measure prior to offset. This information contextualises the reliability of the attribute estimate.',
    prior_offset_sd_modelled_details: 'A brief description of the methods used to derive the mean attribute measure prior to offset. This information contextualises the reliability of the attribute estimate.',
    prior_offset_sd_expert_details: 'A brief description of the methods used to derive the mean attribute measure prior to offset. This information contextualises the reliability of the attribute estimate.',
    prior_offset_sd_proxy_details: 'A brief description of the methods used to derive the mean attribute measure prior to offset. This information contextualises the reliability of the attribute estimate.',
   
    mx_post_offset_mean: 'Expected mean value of the attribute at the offet site at the Time till End (Years).',
    mx_post_offset_sd: 'The expected standard deviation of the mean attribute measure post offset.',
    
    post_offset_sd_data_type: 'A brief description of the methods used to derive the mean attribute measure prior to offset. This information contextualises the reliability of the attribute estimate.',
    post_offset_sd_empirical_details: 'A brief description of the methods used to derive the mean attribute measure prior to offset. This information contextualises the reliability of the attribute estimate.',
    post_offset_sd_modelled_details: 'A brief description of the methods used to derive the mean attribute measure prior to offset. This information contextualises the reliability of the attribute estimate.',
    post_offset_sd_expert_details: 'A brief description of the methods used to derive the mean attribute measure prior to offset. This information contextualises the reliability of the attribute estimate.',
    post_offset_sd_proxy_details: 'A brief description of the methods used to derive the mean attribute measure prior to offset. This information contextualises the reliability of the attribute estimate.',
   
    
    
    
    selected_confidence: 'This indicates the level of confidence that the proposed offset action will generate the anticipated gain (Attribute Measure Post Offset) within the specified Time till End (Years). Proposed actions where the confidence in the outcome is less than 50% are not appropriate in an offsetting context.',
    offset_confidence_justify: 'Provide explanation to support the chosen Confidence Level',
    
    time_till_end: 'The time (years) until the anticipated Attribute Measure Post Offset will be acheived.',
    offset_time_till_end_justify: 'Provide explanation to support the indicated time horizon to achieve the anticipated end point.',
    discount_rate: 'Discount rates are used to account for time lags between losses and gains.Enter a discount rate as a percentage. More discussion the use of discount rates is provided in the User Manual and Govt Good Practice Guidance.',
    offset_discount_rate_justify: 'Provide explanation for choice of discount rate.',
    
    
 
  };

  // Apply tooltips
Object.entries(tooltips).forEach(([id, text]) => {
  let el = $('#' + id);
  if (el.is('select')) {
    el.parent().popover({
      trigger: 'hover',
      placement: 'right',
      content: text,
      container: 'body'
    });
  } else {
    el.popover({
      trigger: 'hover',
      placement: 'right',
      content: text,
      container: 'body'
    });
  }
});

  //---------------------------------------------------------------------------
  // INLINE HELP TEXT (MERGED SYSTEM)
  //---------------------------------------------------------------------------
  const inlineHelp = {
    proposal_overview: `
      <div class='inline-help' id='proposal_overview_help'>
        <strong>Proposal Overview Guidance:</strong><br>
        Provide enough detail to fully describe the proposal, including the activity and the location.
      </div>
    `,
    ecological_context: `
      <div class='inline-help' id='ecological_context_help'>
        <strong>Ecological Context and Impact Summary Guidance:</strong><br>
        Provide a summary of all ecological values at the impact site, the impacts on those values,
        and the residual adverse effects following avoidance, minimisation, and remediation. <br>
        Ecosystems, habitats, or species of particular conservation, policy, or cultural importance 
        should also be identified.
      </div>
    `,
    biodiversity_impacts: `
      <div class='inline-help' id='biodiversity_impacts_help'>
        <strong>Biodiversity Impacts Addressed Outside of the Model Guidance:</strong><br>
        This should include all ecological values at the site that will be impacted that will be addressed through either 1) the initial steps of the effects management hierarchy, 2) compensation measures (where net gain is not demonstrable), 3) net gain outcomes are to be evaluated by alternative means such as SEV for freshwater. 
      </div>
    `,
    offset_package: `
      <div class='inline-help' id='offset_package_help'>
        <strong>Summary Description of Proposed Offset Package Guidance:</strong><br>
        Provide a summary of:<br>
1) All proposed offset action(s) proposed for each of the residual adverse effects and the offset site(s) at which the offset actions will be implemented.<br>
2) The landscape context, spatial configuration, and how the proposed offset design is anticipated to be implementated. <br>
3) Confirmation of additionality of proposed offset measures and feasibility of securing proposed offset sites.<br>
Any residual adverse effects that are proposed for compensation measures should also be identified here. 
      </div>
    `
    // Add more fields here if needed
  };

  // Attach inline-help handlers
  Object.entries(inlineHelp).forEach(([id, helpHtml]) => {

    // Show help on focus
    $('#' + id).on('focus', function () {
      const helpId = '#' + id + '_help';
      if (!$(helpId).length) {
        $(this).after(helpHtml);
      }
    });

    // Remove help on blur
    $('#' + id).on('blur', function () {
      const helpId = '#' + id + '_help';
      $(helpId).remove();
    });

  });


  //---------------------------------------------------------------------------
  // SCROLL HANDLERS
  //---------------------------------------------------------------------------
  Shiny.addCustomMessageHandler('scrollToBottom', function () {
    setTimeout(() => {
      window.scrollTo({ top: document.body.scrollHeight, behavior: 'smooth' });
    }, 300);
  });

  Shiny.addCustomMessageHandler('scrollToTop', function () {
    setTimeout(() => {
      window.scrollTo({ top: 0, behavior: 'smooth' });
    }, 300);
  });


  //---------------------------------------------------------------------------
  // HEADER MODAL TRIGGER
  //---------------------------------------------------------------------------
  $('#header_bar').on('click', function () {
    Shiny.setInputValue('show_header_modal', Math.random());
  });

});
