
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pints

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

pints – prediction intervals simulated – is a toy package for building
prediction intervals with out-of-sample data for
[tidymodels](https://github.com/tidymodels) supported
[workflows](https://github.com/tidymodels/workflows) objects. It
consists of two functions: `prep_interval()` for simulating model fits +
sample uncertainty and `predict_interval()` for outputting prediction
intervals on new observations.

To use pints, you need a dataset for training and a tidymodels workflow
(with a preprocessing recipe and model specification):

``` r
# Set-up
library(tidyverse)
library(tidymodels)
library(pints)

set.seed(123)
split <- palmerpenguins::penguins %>% 
  na.omit() %>% 
  initial_split()

train <- training(split)
other_data <- testing(split)

# Specify recipe and model and combine into workflow
rec <- recipe(body_mass_g ~ ., data = train) %>% 
  step_scale(all_numeric_predictors())

mod <- parsnip::mars(prod_degree = 3) %>% 
  set_engine("earth") %>% 
  set_mode("regression")

workflow <- workflows::workflow() %>% 
  add_recipe(rec) %>% 
  add_model(mod) 
```

`prep_interval()` takes in the workflow and a dataset that will be used
for estimating prediction intervals based on out-of-sample errors.

``` r
set.seed(12)
# output for a 95% prediction interval
intervals_prepped <- workflow %>% 
  prep_interval(fit_data = train)
```

The resulting object is passed into `predict_interval()` which can
output prediction intervals on `new_data`.

``` r
set.seed(12)
pred_intervals <- intervals_prepped %>%
  predict_interval(new_data = other_data,
                   probs = c(0.025, 0.50, 0.975))

pred_intervals
#> # A tibble: 83 x 3
#>    probs_0.025 probs_0.500 probs_0.975
#>          <dbl>       <dbl>       <dbl>
#>  1       3153.       3858.       4492.
#>  2       2672.       3414.       4070.
#>  3       3361.       4234.       4971.
#>  4       2702.       3402.       4110.
#>  5       3347.       4177.       5520.
#>  6       2397.       3197.       3921.
#>  7       3271.       3970.       4589.
#>  8       2745.       3489.       4138.
#>  9       3191.       3954.       4587.
#> 10       2727.       3392.       4026.
#> # ... with 73 more rows
```

Let’s overlay the actual `body_mass_g` on the prediction intervals:

``` r
bind_cols(
  select(other_data, actuals = body_mass_g),
  set_names(pred_intervals,
            paste0(".pred", c("_lower", "", "_upper")))
  ) %>% 
  ggplot(aes(x = .pred, y = actuals))+
  geom_point(aes(y = .pred, color = "prediction interval"))+
  geom_errorbar(aes(ymin = .pred_lower, ymax = .pred_upper, color = "prediction interval"))+
  geom_point(aes(y = actuals, color = "actuals"))+
  theme_bw()+
  coord_cartesian(ylim = c(2000, 8000))+
  labs(title = "Simulated Prediction Intervals for body_mass_g",
       subtitle = "For Palmer's Penguins",
       y = "body_mass_g")
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

Intervals seem to have pretty good coverage, even on this holdout data.

**When to use**

Building robust prediction intervals usually happens *after* model
selection, hence the workflow passed into `predict_interval()` should
have all hyperparameters etc. specified[1].

**More Documentation**

*For a more detailed description and review of the current methodology
used in pints, see [Simulating Prediction
Intervals](https://www.bryanshalloway.com/2021/04/05/simulating-prediction-intervals/).*

# Notes, Limitations, Resources, Ideas

pints produces reasonable, simple to create prediction interval
(assuming iid) for any model type supported by the tidymodels ecosystem.
Due to it taking in a *workflow*, pints takes an expansive view of
uncertainty due to model estimation and include both model fit *as well
as* preprocessing steps (which feels like a good idea in most cases).
That said, pints is *very much* in toy / experimental form[2]:

-   A more detailed description of the (rough) methodology currently
    used by pints can be found in the second of three blog posts I wrote
    on building prediction intervals: [Simulating Prediction
    Intervals](https://www.bryanshalloway.com/2021/04/05/simulating-prediction-intervals/)
    where I also include references to related posts by Dan Saattrup
    Nielsen and others (Dan has since published the python package
    [Doubt](https://github.com/saattrupdan/doubt)). See
    [parsnip\#464](https://github.com/tidymodels/parsnip/issues/464) for
    other related links.
-   The methodology in pints assumes errors are roughly iid[3]. See
    other post on [quantile regression
    intervals](https://www.bryanshalloway.com/2021/04/21/quantile-regression-forests-for-prediction-intervals/)
    and linked to resources for other (in many circumstances) more
    flexible approaches.
-   I’d like to investigate blending a parsnip interface like pints onto
    a [conformal inference](https://github.com/ryantibs/conformal)
    approach
-   I’d like to allow for more flexibility in specifying things like the
    resampling methodology (i.e. if have custom resampling scheme that
    makes since for the problem, e.g. in time series), allowing errors
    to be non-iid, …
-   Default builds `sqrt(nrow(data))` models – which can take a long
    time depending on number of observations and model type. May want to
    set-up for doing in parallel or look into other approaches for
    speed-ups.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("brshallo/pints")
```

[1] Though pints may also be useful for evaluating prediction intervals
of different candidate models.

[2] There are also no checks, tests, etc currently in place.

[3] For example, heteroskedasticity, like that shown in this
[gist](https://gist.github.com/brshallo/a9e5d245feae3dfc551a122516a702ff)
does not produce good interval fits.
