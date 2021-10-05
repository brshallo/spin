#' Control function used as part of `prep_interval()`
#' could likely be improved with newer extracting funs in tidymodels
ctrl_fit_recipe <- function(x){
  output <- list(fit = workflows::pull_workflow_fit(x),
                 recipe = workflows::pull_workflow_prepped_recipe(x))

  c(output, list(resids =
                   bind_cols(
                     workflows::pull_workflow_mold(x)$outcomes %>% purrr::set_names(".outcome"),
                     predict(output$fit, workflows::pull_workflow_mold(x)$predictors)
                   )
  ))
}

# extract parts of control function and output as dataframe with list-cols
# @param wf_extracts A resample_results object with a column for .extracts
#   that was created using `ctrl_fit_recipe()`
extract_extracts <- function(wf_extracts){
  wf_extracts %>%
    select(.extracts) %>%
    transmute(.extracts = map(.extracts, ".extracts") %>% map(1)) %>%
    tidyr::unnest_wider(.extracts)
}


#' Prep for Prediction Interval
#'
#' This function takes in a workflow and outputs a named list meant to be passed
#' into `predict_interval()`.
#'
#' @param wf Tidymodels workflow containing a recipe and a model.
#' @param fit_data Dataframe used to estimate the prediction intervals.
#' @param n_boot Number of bootstrapped samples used to simulate building the
#'   model (used to determine uncertainty due to model).
#' @return A named list of two tibbles named `model_uncertainty` and
#'   `sample_uncertainty`.
#' @export
#'
#' @examples
#' # See README
prep_interval <- function(wf, fit_data, n_boot = sqrt(nrow(fit_data))){

  ##### Uncertainty due to model specification #####
  ctrl <- tune::control_resamples(extract = ctrl_fit_recipe, save_pred = TRUE)

  # Could have where you input a resample specification...
  resamples_boot <- rsample::bootstraps(fit_data, n_boot)

  wf_model_uncertainty <- wf %>%
    tune::fit_resamples(resamples_boot, control = ctrl)

  model_uncertainty <- extract_extracts(wf_model_uncertainty)

  ##### Uncertainty due to sample #####

  # Am setting-up weighting here between residuals on analysis versus assessment sets
  analysis_preds <- model_uncertainty %>%
    select(resids) %>%
    tidyr::unnest(resids)

  no_info <- select(analysis_preds, .outcome) %>%
    bind_cols(analysis_preds[sample(nrow(analysis_preds)), ".pred"]) %>%
    transmute(.resid = .pred - .outcome)

  analysis_resids <- analysis_preds %>%
    transmute(.resid = .pred - .outcome)

  assessment_resids <- wf_model_uncertainty %>%
    select(.predictions) %>%
    tidyr::unnest(.predictions) %>%
    mutate(.resid = .pred - cur_data()[[3]]) %>%
    select(.resid)

  R <- (mean(abs(assessment_resids$.resid)) - mean(abs(analysis_resids$.resid))) / (mean(abs(no_info$.resid)) - mean(abs(analysis_resids$.resid)))

  W <- 0.632 / (1 - 0.368 * R)

  quantiles <- seq(from = 0, to = 1, length.out = nrow(fit_data))
  quantiles <- quantiles[c(-1, -length(quantiles))]

  sample_uncertainty <- (1 - W) * quantile(analysis_resids$.resid, probs = quantiles) + W * quantile(assessment_resids$.resid, probs = quantiles)
  sample_uncertainty <- tibble(.resid = sample_uncertainty)

  # Outputs
  model_uncertainty <- model_uncertainty %>%
    select(-resids)

  list(
    model_uncertainty = model_uncertainty,
    sample_uncertainty = sample_uncertainty
  )

}
