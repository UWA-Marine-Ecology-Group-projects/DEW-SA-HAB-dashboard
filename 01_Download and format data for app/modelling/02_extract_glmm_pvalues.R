# ============================================================
# Extract p-values from the BRUV and RLS (UVC) GLMMs
#
# Reads the saved model objects from both pipelines and writes one Excel
# workbook holding, for every metric x location Period model:
#
#   *_term_tests  - Type III joint tests of each fixed-effect term
#                   (Period, Status, Period:Status), via emmeans::joint_tests()
#   *_contrasts   - the Pre-bloom vs Bloom contrast, averaged over status and
#                   again within each status where the model has one
#
# Inputs (both written by the modelling scripts, nothing is re-fitted here):
#   model_results/<bruv_tag>/location_model_objects.rds
#   model_results/<rls_tag>/location_model_objects.rds
#
# Output:
#   model_results/glmm_pvalues.xlsx
#
# Run from the project root, after both modelling scripts have been run:
#   01_Download and format data for app/modelling/01_hab_models_additive_poisson_limit.R
#   01_Download and format data for app/RLS/11_rls_glmm_models.R
# ============================================================

library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(stringr)
library(emmeans)
library(glmmTMB)
library(writexl)

# -----------------------------
# 1. Settings
# -----------------------------

# These must match `analysis_tag` in the two modelling scripts.
bruv_analysis_tag <- "20260730_additive_poisson_limit"
rls_analysis_tag  <- "rls_glmm_results"

bruv_model_root <- file.path("model_results", bruv_analysis_tag)
rls_model_root  <- file.path("model_results", rls_analysis_tag)

output_path <- file.path("model_results", "glmm_pvalues.xlsx")

# Multiple testing. Every metric is tested separately in every location, so
# the natural family of tests is "this metric, across the locations". p_adj
# is Benjamini-Hochberg within source x metric x term; the raw p is kept
# alongside it so you can report either.
p_adjust_method <- "BH"

# The factor names differ between the two pipelines - BRUV uses Status,
# RLS uses lowercase status - so they are matched case-insensitively rather
# than hardcoded per pipeline.
period_candidates <- c("Period", "period")
status_candidates <- c("Status", "status")

# -----------------------------
# 2. Helpers
# -----------------------------

# glmmTMB objects carry a pointer to compiled TMB structures that does not
# survive saveRDS/readRDS. up2date() rebuilds it. Everything below only needs
# fixef() and vcov(), which are stored values rather than pointers, so this is
# belt-and-braces - but it costs nothing and turns an obscure
# "external pointer is not valid" into a working model.
refresh_model <- function(model) {
  if (is.null(model)) {
    return(NULL)
  }
  
  refreshed <- try(glmmTMB::up2date(model), silent = TRUE)
  
  if (inherits(refreshed, "try-error")) model else refreshed
}

# Fixed-effect term labels only: term.labels includes the random effects as
# labels containing "|", which are dropped here.
fixed_term_labels <- function(model) {
  labels <- attr(stats::terms(stats::formula(model)), "term.labels")
  labels[!stringr::str_detect(labels, "\\|")]
}

# Which of the candidate names actually appears in the fixed effects.
find_factor <- function(model, candidates) {
  variables <- unique(unlist(stringr::str_split(fixed_term_labels(model), ":")))
  hit <- intersect(candidates, variables)
  
  if (length(hit) == 0) NA_character_ else hit[1]
}

has_interaction <- function(model, period_var, status_var) {
  if (is.na(period_var) || is.na(status_var)) {
    return(FALSE)
  }
  
  any(
    stringr::str_detect(fixed_term_labels(model), ":") &
      stringr::str_detect(fixed_term_labels(model), stringr::fixed(period_var)) &
      stringr::str_detect(fixed_term_labels(model), stringr::fixed(status_var))
  )
}

# Type III joint tests of every fixed-effect term.
extract_term_tests <- function(model) {
  
  tests <- as.data.frame(emmeans::joint_tests(model))
  
  # joint_tests names its first column "model term"; depending on how the
  # summary is coerced that can arrive as "model.term" instead.
  names(tests)[names(tests) %in% c("model term", "model.term")] <- "term"
  
  tibble::as_tibble(tests) %>%
    dplyr::transmute(
      term,
      df1 = as.numeric(df1),
      df2 = as.numeric(df2),
      F_ratio = as.numeric(F.ratio),
      p_value = as.numeric(p.value)
    )
}

# One emmeans pairwise contrast table, tidied into a common shape.
#
# type = "response" back-transforms, so a log-link model returns a ratio and
# an identity-link model returns a difference. The test itself is the same
# Wald test on the link scale either way; only the reported effect size
# changes, which is why estimate_type is recorded.
tidy_contrast <- function(emm_object, by_var = NA_character_) {
  
  contrast_table <- as.data.frame(
    summary(
      pairs(emm_object, adjust = "none"),
      infer = c(TRUE, TRUE),
      type = "response"
    )
  )
  
  # The direction of the comparison is carried in the `contrast` column
  # itself; this only records what kind of quantity `estimate` is.
  estimate_type <- if ("ratio" %in% names(contrast_table)) {
    "ratio"
  } else if ("odds.ratio" %in% names(contrast_table)) {
    "odds ratio"
  } else {
    "difference"
  }
  
  estimate_col <- dplyr::case_when(
    "ratio" %in% names(contrast_table) ~ "ratio",
    "odds.ratio" %in% names(contrast_table) ~ "odds.ratio",
    TRUE ~ "estimate"
  )
  
  statistic_col <- if ("z.ratio" %in% names(contrast_table)) "z.ratio" else "t.ratio"
  
  out <- tibble::tibble(
    contrast = as.character(contrast_table$contrast),
    status_level = if (!is.na(by_var) && by_var %in% names(contrast_table)) {
      as.character(contrast_table[[by_var]])
    } else {
      NA_character_
    },
    estimate_type = estimate_type,
    estimate = as.numeric(contrast_table[[estimate_col]]),
    SE = as.numeric(contrast_table$SE),
    lower_CL = as.numeric(
      contrast_table[[
        if ("asymp.LCL" %in% names(contrast_table)) "asymp.LCL" else "lower.CL"
      ]]
    ),
    upper_CL = as.numeric(
      contrast_table[[
        if ("asymp.UCL" %in% names(contrast_table)) "asymp.UCL" else "upper.CL"
      ]]
    ),
    statistic = as.numeric(contrast_table[[statistic_col]]),
    p_value = as.numeric(contrast_table$p.value)
  )
  
  out
}

# Pre-bloom vs Bloom, marginally and (where a status term was fitted) within
# each status level.
extract_contrasts <- function(model) {
  
  period_var <- find_factor(model, period_candidates)
  status_var <- find_factor(model, status_candidates)
  
  if (is.na(period_var)) {
    return(tibble::tibble())
  }
  
  interaction_present <- has_interaction(model, period_var, status_var)
  
  marginal <- tidy_contrast(
    emmeans::emmeans(model, specs = period_var)
  ) %>%
    dplyr::mutate(
      contrast_scope = "marginal (averaged over status)",
      interaction_in_model = interaction_present
    )
  
  if (is.na(status_var)) {
    return(marginal)
  }
  
  within_status <- tidy_contrast(
    emmeans::emmeans(
      model,
      specs = period_var,
      by = status_var
    ),
    by_var = status_var
  ) %>%
    dplyr::mutate(
      contrast_scope = "within status",
      interaction_in_model = interaction_present
    )
  
  dplyr::bind_rows(marginal, within_status)
}

# -----------------------------
# 3. Walk one pipeline's saved models
# -----------------------------

# Both pipelines save list(index = <tibble>, fits = <list>), where each fit
# holds a $period_model that is NULL when the model was skipped or failed.
read_model_objects <- function(path, source_label, rerun_hint) {
  
  if (!file.exists(path)) {
    stop(
      "Could not find ", path, ".\n",
      "Re-run ", rerun_hint, " first - it writes that file.",
      call. = FALSE
    )
  }
  
  saved <- readRDS(path)
  
  if (!all(c("index", "fits") %in% names(saved))) {
    stop(
      path, " does not have the expected index/fits structure.",
      call. = FALSE
    )
  }
  
  message(
    "Read ", nrow(saved$index), " ", source_label,
    " metric x location fits from ", path
  )
  
  saved
}

process_pipeline <- function(saved, source_label) {
  
  index <- saved$index %>%
    tibble::as_tibble() %>%
    dplyr::mutate(source = source_label)
  
  # The RLS index carries a region column, the BRUV one does not. Add it as
  # NA when absent so the two sources share a column layout.
  if (!"region" %in% names(index)) {
    index <- dplyr::mutate(index, region = NA_character_)
  }
  
  results <- purrr::map(
    seq_len(nrow(index)),
    function(i) {
      
      ids <- index[i, ]
      model <- refresh_model(saved$fits[[ids$fit_index]]$period_model)
      
      if (is.null(model)) {
        return(list(
          terms = tibble::tibble(),
          contrasts = tibble::tibble(),
          log = dplyr::mutate(
            ids,
            status = "no model",
            detail = "Period model is NULL - skipped or failed to fit"
          )
        ))
      }
      
      term_tests <- try(extract_term_tests(model), silent = TRUE)
      contrasts  <- try(extract_contrasts(model), silent = TRUE)
      
      failed <- inherits(term_tests, "try-error") || inherits(contrasts, "try-error")
      
      if (failed) {
        detail <- paste(
          c(
            if (inherits(term_tests, "try-error")) {
              paste("joint_tests:", conditionMessage(attr(term_tests, "condition")))
            },
            if (inherits(contrasts, "try-error")) {
              paste("contrasts:", conditionMessage(attr(contrasts, "condition")))
            }
          ),
          collapse = " | "
        )
        
        return(list(
          terms = if (inherits(term_tests, "try-error")) tibble::tibble() else
            dplyr::bind_cols(ids[rep(1, nrow(term_tests)), ], term_tests),
          contrasts = if (inherits(contrasts, "try-error")) tibble::tibble() else
            dplyr::bind_cols(ids[rep(1, nrow(contrasts)), ], contrasts),
          log = dplyr::mutate(ids, status = "partial", detail = detail)
        ))
      }
      
      list(
        terms = if (nrow(term_tests) == 0) tibble::tibble() else
          dplyr::bind_cols(ids[rep(1, nrow(term_tests)), ], term_tests),
        contrasts = if (nrow(contrasts) == 0) tibble::tibble() else
          dplyr::bind_cols(ids[rep(1, nrow(contrasts)), ], contrasts),
        log = dplyr::mutate(ids, status = "ok", detail = NA_character_)
      )
    }
  )
  
  list(
    terms = purrr::map_dfr(results, "terms"),
    contrasts = purrr::map_dfr(results, "contrasts"),
    log = purrr::map_dfr(results, "log")
  )
}

# Benjamini-Hochberg within source x metric x term, across the locations.
add_adjusted_p <- function(df, group_cols) {
  
  if (nrow(df) == 0) {
    return(df)
  }
  
  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::mutate(
      n_tests_in_family = sum(!is.na(p_value)),
      p_value_adj = stats::p.adjust(p_value, method = p_adjust_method)
    ) %>%
    dplyr::ungroup()
}

# -----------------------------
# 4. Run both pipelines
# -----------------------------

bruv_saved <- read_model_objects(
  file.path(bruv_model_root, "location_model_objects.rds"),
  "BRUV",
  "'01_Download and format data for app/modelling/01_hab_models_additive_poisson_limit.R'"
)

rls_saved <- read_model_objects(
  file.path(rls_model_root, "location_model_objects.rds"),
  "RLS",
  "'01_Download and format data for app/RLS/11_rls_glmm_models.R'"
)

bruv_results <- process_pipeline(bruv_saved, "BRUV")
rls_results  <- process_pipeline(rls_saved, "RLS")

id_cols <- c("source", "metric", "location", "region")

tidy_sheet <- function(df, extra_cols) {
  if (nrow(df) == 0) {
    return(df)
  }
  
  df %>%
    dplyr::select(
      dplyr::any_of(c(id_cols, extra_cols)),
      dplyr::everything(),
      -dplyr::any_of("fit_index")
    ) %>%
    dplyr::arrange(metric, location)
}

bruv_term_tests <- bruv_results$terms %>%
  add_adjusted_p(c("source", "metric", "term")) %>%
  tidy_sheet(c("term", "df1", "df2", "F_ratio", "p_value", "p_value_adj"))

rls_term_tests <- rls_results$terms %>%
  add_adjusted_p(c("source", "metric", "term")) %>%
  tidy_sheet(c("term", "df1", "df2", "F_ratio", "p_value", "p_value_adj"))

bruv_contrasts <- bruv_results$contrasts %>%
  add_adjusted_p(c("source", "metric", "contrast_scope", "status_level")) %>%
  tidy_sheet(
    c("contrast_scope", "status_level", "contrast", "estimate_type",
      "estimate", "SE", "lower_CL", "upper_CL", "statistic",
      "p_value", "p_value_adj", "interaction_in_model")
  )

rls_contrasts <- rls_results$contrasts %>%
  add_adjusted_p(c("source", "metric", "contrast_scope", "status_level")) %>%
  tidy_sheet(
    c("contrast_scope", "status_level", "contrast", "estimate_type",
      "estimate", "SE", "lower_CL", "upper_CL", "statistic",
      "p_value", "p_value_adj", "interaction_in_model")
  )

extraction_log <- dplyr::bind_rows(bruv_results$log, rls_results$log) %>%
  dplyr::select(dplyr::any_of(id_cols), status, detail)

# -----------------------------
# 5. Notes sheet
# -----------------------------

notes <- tibble::tribble(
  ~item, ~note,
  
  "What these are",
  paste(
    "Fixed-effect p-values from the Period models fitted per metric x",
    "location by the two modelling scripts. Nothing is re-fitted here -",
    "the models are read from the saved location_model_objects.rds files."
  ),
  
  "term_tests sheets",
  paste(
    "Type III joint tests from emmeans::joint_tests(). One row per",
    "fixed-effect term. df2 = Inf means the test is asymptotic (a Wald",
    "chi-square expressed as an F), which is what glmmTMB supports."
  ),
  
  "contrasts sheets",
  paste(
    "Pre-bloom vs Bloom from emmeans' pairs(). contrast_scope 'marginal'",
    "averages over status; 'within status' is the contrast inside each",
    "status level. estimate_type says whether the effect size is a ratio",
    "(log-link models) or a difference (Gaussian identity models), and the",
    "contrast column gives the direction. The p-value is the same Wald",
    "test either way."
  ),
  
  "Duplicate-looking p-values",
  paste(
    "Where a model has only two periods, the Period row in term_tests and",
    "the marginal contrast in contrasts are the same test, so the two",
    "p-values will match. That is expected, not an error."
  ),
  
  "interaction_in_model",
  paste(
    "TRUE means a Period x Status interaction was fitted. The marginal",
    "Period contrast then averages over a status effect that itself",
    "differs between periods, so prefer the 'within status' rows for",
    "those models."
  ),
  
  "p_value_adj",
  paste0(
    "Benjamini-Hochberg (", p_adjust_method, ") adjusted p, computed within ",
    "source x metric x term for the term tests, and within source x metric ",
    "x contrast_scope x status_level for the contrasts - i.e. across the ",
    "locations tested for that metric. n_tests_in_family gives the size of ",
    "each family. Raw p_value is kept so either can be reported."
  ),
  
  "Missing rows",
  paste(
    "A metric x location with no rows was either skipped before fitting",
    "(too many zeros) or failed to converge. See the extraction_log sheet."
  ),
  
  "Generated",
  paste("Written by 02_extract_glmm_pvalues.R on", format(Sys.time(), "%Y-%m-%d %H:%M"))
)

# -----------------------------
# 6. Write the workbook
# -----------------------------

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

writexl::write_xlsx(
  list(
    notes = notes,
    bruv_term_tests = bruv_term_tests,
    bruv_contrasts = bruv_contrasts,
    rls_term_tests = rls_term_tests,
    rls_contrasts = rls_contrasts,
    extraction_log = extraction_log
  ),
  output_path
)

message("Wrote ", output_path)

# Quick console check
extraction_log %>%
  dplyr::count(source, status)
