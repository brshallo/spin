
#' Predict Interval
#'
#' This function takes in the output from `prep_interval()` along with an
#' unprepped hold-out dataset and outputs a prediction interval.
#'
#' @param prepped_interval Object outputted by `prep_interval()`.
#' @param new_data Data to generate predictions on.
#' @param probs Quantiles of predictions to output.
#' @param cross If TRUE makes distribution for selecting quantiles from made up
#'   of all possible combinations of samples of {model fitting uncertainty} and
#'   {sample uncertainty}. If FALSE, create `n_sims` number of simulations for
#'   each sample.
#' @param n_sims An integer of length 1, specifying the number of simulations for each observation. Default of `cross == TRUE`,
#'   means `n_sims` is ignored. If `cross == FALSE`, default of `n_sims` is 10000.
#' @return A tibble containing columns `probs_*` at each quantile specified by `probs`.
#' @export
#'
#' @examples
#' # See README
predict_interval <- function(prepped_interval, new_data, probs = c(0.025, 0.50, 0.975), cross = TRUE, n_sims = 10000){

  op <- options(dplyr.summarise.inform = FALSE)

  model_uncertainty <- prepped_interval$model_uncertainty %>%
    mutate(assessment = map2(fit, recipe,
                             ~predict(.x, recipes::bake(.y, new_data = new_data)) %>%
                               mutate(.id = row_number()))
    ) %>%
    select(assessment) %>%
    tidyr::unnest(assessment) %>%
    group_by(.id) %>%
    mutate(m = .pred - mean(.pred)) %>%
    ungroup()

  output <- purrr::when(
    cross,
    . ~ tidyr::crossing(model_uncertainty, prepped_interval$sample_uncertainty),
    ~ bind_cols(
      slice_sample(
        model_uncertainty,
        n = n_sims * nrow(new_data),
        replace = TRUE
      ),
      slice_sample(
        prepped_interval$sample_uncertainty,
        n = n_sims * nrow(new_data),
        replace = TRUE
      )
    )
  ) %>%
    mutate(c = m + .resid + .pred) %>%
    group_by(.id) %>%
    summarise(qs = quantile(c, probs),
              probs = format(probs, nsmall = 2)) %>%
    ungroup() %>%
    tidyr::pivot_wider(names_from = probs,
                       values_from = qs,
                       names_prefix = "probs_") %>%
    select(-.id)

  on.exit(options(op))

  output
}

