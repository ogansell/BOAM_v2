# BOAM — Biodiversity Offsetting Accounting Model
## Claude Code Project Guide

---

## Project Overview

BOAM is an R/Shiny web application for ecological biodiversity impact and offset assessments in New Zealand. The core output is **Net Present Biodiversity Value (NPBV)**, which quantifies net biodiversity impact after accounting for offsets.

**Deployed on:** ShinyApps.io  
**Primary users:** Fleur (domain expert / tester) and clients conducting regulatory offset assessments

---

## File Structure

```
app.R                  # Entry point: shinyApp(ui = ui, server = server)
Global.R               # Package loading, theme, numeric_ids, draft_fields, project_calc_fields
ui.R                   # All UI definitions (fluidPage, tabPanels, inputs)
server.R               # All server logic (reactives, observers, outputs)
calculation_funcs.R    # Monte Carlo simulation, summary computation, table rendering
general_funcs.R        # Modals, draft import/export, report rendering, edit/reset helpers
ui_funcs.R             # reset_project_calc_inputs(), restore_inputs_from_list()
report.Rmd             # PDF report template (xelatex, kableExtra)
preamble.tex           # LaTeX preamble for report styling
www/                   # Static assets: CSS, JS, fonts, logo, images
R/                     # Source directory (general_funcs.R, calculation_funcs.R, ui_funcs.R sourced from here)
```

---

## Architecture & Key Patterns

### Monte Carlo Simulation
- **10,000 draws** per attribute (fixed `set.seed(2548)`)
- Raw simulation vectors stored in `results` reactiveValues: `results$impact`, `results$offset`, `results$npbv`
- **Prediction intervals are empirical quantiles** (P5/P95) — never symmetric ±CI
- Area is applied as a **deterministic multiplier** after simulation, not embedded in draws

### Component-Level Aggregation
- `compute_summary()` aggregates attribute rows to component level using **inverse-variance weighting** (IVW) on the stored P5/P95 values
- `compute_summary_df()` is the display version (adds HTML formatting); `compute_summary()` is the clean data version used in the report
- **Known issue:** IVW at the summary layer is a legacy approach — the preferred method is equal-weight pooling of raw draws via `draw_store`, but this is not yet fully implemented at the component level

### Distributions Supported
| Distribution | User inputs | Internal param derivation |
|---|---|---|
| Normal | mean, SD | direct |
| Poisson | mean (λ), sample size (n) | SD = √λ/√n; simulates rpois then divides by n |
| Negative Binomial | mean (μ), variance (s²) | k = μ²/(s²−μ) via Method of Moments |

### Expert Elicitation (SHELF)
- Triggered when `*_data_type == "Expert elicited"` for any measurement point
- User enters P_low, P50, P_high + CI level
- Derived: `mean = P50`, `SD = (P_high − P_low) / (2 × qnorm((1 + CI) / 2))`
- Always simulated as **Normal** regardless of global distribution selector
- `shelf_to_mean_sd()` and `resolve_mean_sd()` in `calculation_funcs.R`

### NPBV Formula
```
impact_score = (post_impact − prior_impact) / benchmark × impact_area
offset_score = (post_offset − prior_offset) / benchmark × confidence_score / discount_factor × offset_area
npbv = impact_score + offset_score
discount_factor = (1 + discount_rate/100)^time_till_end
```

---

## Input ID Discipline — CRITICAL

**Input ID mismatches cause silent NULL collection and eventual R process crashes (session timeouts).**

Three places must stay in sync:
1. `ui.R` — actual `inputId` strings in `textInput()`, `numericInput()`, `selectInput()`, etc.
2. `Global.R` — `draft_fields` vector (all fields saved to JSON draft)
3. `Global.R` — `numeric_ids` vector (fields validated before simulation runs)
4. `ui_funcs.R` — `reset_project_calc_inputs()` and `restore_inputs_from_list()` (must update/restore every field)

When adding or renaming any input:
- Update `ui.R` (the actual widget)
- Update `draft_fields` in `Global.R` if it should be saved/restored
- Update `numeric_ids` in `Global.R` if it needs numeric validation
- Update `reset_project_calc_inputs()` in `ui_funcs.R`
- Update `restore_inputs_from_list()` in `ui_funcs.R` — `import_draft_file()` delegates to this for all Project Calculations fields, so it does not need separate updates (see Draft Save/Load)

---

## Key Reactive Flow

```
input$calculate
  → validate_and_run_simulation()   [calculation_funcs.R]
    → validate_numeric_inputs()
    → simulate_impact_scores()
      → resolve_mean_sd()
      → draw_samples_eff()
    → results$impact / $offset / $npbv (reactiveValues)
  → enable("save_and_reset")

input$save_and_reset
  → save_and_reset_logic()          [calculation_funcs.R]
    → empirical quantiles from results vectors
    → append/overwrite row in saved_results()
    → compute_summary(saved_results())  → summary_results()
    → reset_project_calc_inputs()   [ui_funcs.R]
    → disable("save_and_reset")
```

---

## Report Generation

- `render_report()` in `general_funcs.R` copies `report.Rmd` + `preamble.tex` + fonts to `tempdir()`, renders via `rmarkdown::render()`, copies output back
- Two report variants: short (`long_report = FALSE`) and full (`long_report = TRUE`, adds Appendix A with per-row detail)
- **P5/P95 formatting in report:** `Mean [P5, P95]`
- **Red highlighting:** triggered when `NPBV_P5 < 0` — uses `cell_spec(..., color = "red")` via kableExtra
- LaTeX engine: **xelatex**; uses custom fonts from `www/` (texgyreheros .otf files)
- `%` signs in measurement units must be escaped for LaTeX — handled by `has_special_chars()` + `cell_spec(..., escape = TRUE)`

### PDF Rendering on ShinyApps.io — Deployment Requirement
- ShinyApps.io has **no LaTeX preinstalled**. PDF rendering relies on `rsconnect` bundling a minimal **TinyTeX** alongside the app.
- `library(tinytex)` must be present in `Global.R` so `rsconnect::deployApp()` detects the LaTeX dependency and bundles it.
- Required LaTeX packages (pulled in by `report.Rmd`/`preamble.tex`/kableExtra): `fontspec`, `xcolor`, `pdflscape`, `tcolorbox` (+ `pgf`, `environ`, `trimspaces`, `tikzfill`, `pdfcol`), `caption`, `listings`, `multirow`, `wrapfig`, `colortbl`, plus standard kableExtra deps (`booktabs`, `longtable`, `array`, `float`, `tabu`, `threeparttable`, `threeparttablex`, `varwidth`, `ulem`, `makecell`).
- **Before deploying:** render `report.Rmd` locally at least once (TinyTeX auto-installs any missing packages on first use). This ensures your local TinyTeX has everything `rsconnect` needs to detect and bundle.
- Watch the `rsconnect::deployApp()` log for TinyTeX bundling — if it's skipped, PDF download will fail on shinyapps.io with a "tex not found"-type error even though it works locally.

---

## Draft Save/Load (JSON)

- **Save:** `draft_export` downloadHandler collects all `draft_fields` inputs + `saved_results()` table → JSON
- **Load:** `import_draft_file()` reads JSON, restores Project Details fields directly, then delegates all Project Calculations fields to `restore_inputs_from_list()` (shared with Edit Mode), restores `saved_results()` and recomputes `summary_results()`
- Format: `{ inputs: {...}, saved_results: [{...}, ...] }`
- **Don't duplicate field-restoration logic in `import_draft_file()`** — always route Project Calculations fields through `restore_inputs_from_list()` so it stays in sync with `ui_funcs.R` (a prior duplicated version drifted out of sync and silently dropped ~44 fields on draft load)
- `build_draft_payload()` (`general_funcs.R`) builds the shared `{ inputs: {...}, saved_results: [...] }` payload used by both `draft_export` and the autosave feature below
- `restore_draft_data()` (`general_funcs.R`) is the shared restore logic (used by both `import_draft_file()` and autosave restore) — `import_draft_file()` is now a thin wrapper that reads/parses the uploaded JSON file then calls `restore_draft_data()`

### Autosave (browser localStorage)

- After every `input$save_and_reset` (i.e. each time a biodiversity attribute is completed and saved), the server builds the draft payload via `build_draft_payload()` and sends it to the client via `session$sendCustomMessage("autosaveDraft", payload)`
- Client-side handler (`ui.R`) stores it in `localStorage` under `boam_autosave_draft`, along with a timestamp — this survives a server-side session timeout/crash since it lives in the browser, not the R process
- On page load (`shiny:connected`), the client checks `localStorage` for an autosave and sends it to the server as `input$autosave_check`; if found, `restore_autosave_modal()` asks the user whether to restore it
- `input$restore_autosave` → `restore_draft_data()` repopulates the session from the autosaved payload; `input$discard_autosave` clears it
- Starting a new report (`confirm_new`) clears the autosave via `session$sendCustomMessage("clearAutosave", list())`
- This is a workaround for the underlying ShinyApps.io idle-timeout issue, not a fix for it — it just means in-progress work isn't lost when the session disconnects

---

## Excel Upload (Bulk Data Entry)

- **Validate:** `validate_xlsx_upload()` (`general_funcs.R`) reads the uploaded `.xlsx` via `read.xlsx()` — **must pass `sep.names = " "`** on every `read.xlsx()` call, otherwise openxlsx's default `sep.names = "."` turns headers like `"Biodiversity Type"` into `"Biodiversity.Type"`, which silently breaks `rename_to_ids()` (no columns get renamed, every `row$<id>` is `NULL`)
- Expected sheets: `Project Details`, `Impact Attributes`, `Offset Attributes`, `Documentation` (optional), `Instructions` (reference only)
- Column label ↔ internal ID maps: `xlsx_impact_columns()`, `xlsx_offset_columns()`, `xlsx_doc_columns()`; `rename_to_ids()` applies them
- Join key across Impact/Offset/Documentation sheets: `biodiversity_type` + `biodiversity_component` + `biodiversity_attribute` (see `join_keys`)
- Expert-elicited (SHELF) data type is **not supported** via spreadsheet — rejected during validation
- **Process:** `process_xlsx_upload()` (`general_funcs.R`) re-simulates every row via `resimulate_row()`, replaces `saved_results()`/`draw_store`/`summary_results()` entirely, and restores Project Details inputs
- **NA vs NULL gotcha:** blank cells from `read.xlsx()` come back as `NA` (e.g. `NA_character_`), not `NULL`. Guards like `if (!is.null(x) && ...)` are insufficient — also need `!is.na(x)` before any numeric/length check (`nchar()`, `>`, etc.), since `TRUE && NA` throws `"missing value where TRUE/FALSE needed"`

---

## Edit Mode

When a user clicks "Edit" on a saved row:
- `editing(TRUE)`, `edit_row_idx(row)` set
- `restore_inputs_from_list()` repopulates all inputs from the saved row
- Button labels change: "Run Simulations" → "Re-Run Simulations", "Save to Report & Reset" → "Update Entry In Report"
- On save: `save_and_reset_logic()` overwrites `df[edit_row_idx(), ]` instead of appending
- "Cancel" button appears (rendered by `output$cancel_edit_ui`)

---

## CSS Classes & JS

**Key CSS classes** (in `www/styles.css`):
- `.colored-box.red-box` / `.blue-box` / `.purple-box` — section panels
- `.compact-grid` — CSS grid for inputs
- `.row-start` — forces a new grid row
- `.invalid-input` — red border on failed validation
- `.npbv-alert` — red text for negative NPBV
- `.editing-row` — highlights the row being edited in the DT table

**Custom JS message handlers** (defined in `ui.R`):
- `clearAllInvalidInputs(ids[])` — removes `.invalid-input` from all listed IDs
- `addInvalidClass(id)` / `removeInvalidClass(id)` — per-field validation feedback
- `scrollToBottom` / `scrollToTop` — scroll page after save/edit (handlers in `www/app-hover-behaviour.js`)

**Idle session warning:**
- `IDLE_MODAL_MIN` (`Global.R`) sets the inactivity timeout in minutes
- Client-side JS countdown (`ui.R`) sends `idle_warn` after `IDLE_MODAL_MIN` of inactivity, and `dismiss_idle` on the next user activity
- `server.R` shows `idle_modal()` (`general_funcs.R`) on `idle_warn`, and removes it on `dismiss_idle` / `stay_active`
- Note: `www/app_working_load_button.R` is a large unused leftover file with its own `IDLE_MODAL_MIN <- 2` — not sourced by the app, don't edit it for this feature

---

## Confidence Levels

```r
confidence_levels <- data.frame(
  levels = c("", "Low confidence >50% <75%", "Confident 75-90%", "Very confident >90%"),
  scores = c(NA, 0.62, 0.825, 0.955)
)
```
The score multiplies the offset in the NPBV formula.

---

## Colour Palette

| Use | Hex |
|---|---|
| Dark green (header, buttons) | `#194036` |
| Accent green | `#0F6E56` |
| Background | `#F5F4F2` |
| Foreground | `#384246` |
| Primary (Bootstrap) | `#FFC51D` |

---

## Common Pitfalls

1. **Don't embed area in MC draws** — apply area as a scalar multiplier after simulation to avoid inflating uncertainty
2. **Don't use symmetric CIs** — always use empirical P5/P95 from simulation vectors, not `mean ± z×SD`
3. **Don't use `fmt_pm`** — it's a deprecated alias for `fmt_pi`; use `fmt_pi` directly
4. **`compute_summary()` vs `compute_summary_df()`** — `compute_summary()` returns clean data for the report; `compute_summary_df()` returns HTML-formatted data for the DT table. Don't mix them.
5. **SHELF always Normal** — expert-elicited points use Normal simulation regardless of the global `distribution` selector; the `eff_dist_*` variables handle this override in `simulate_impact_scores()`
6. **Negative Binomial variance constraint** — variance must exceed mean; `k` blows up otherwise. The code guards: `var <- max(sec_val, mu + 0.001)`
7. **`saved_results()` is a reactiveVal of a data.frame** — always call `saved_results()` to read, `saved_results(df)` to write

---

## Style Preferences

- **Minimal, targeted edits** — surgical changes over refactors
- **No backward-compat shims** — fix the root cause, don't add compatibility layers
- Prefer direct column references over positional indexing in dplyr pipelines
- Keep all calculation logic in `calculation_funcs.R`, UI helpers in `ui_funcs.R`, session/modal/file logic in `general_funcs.R`
