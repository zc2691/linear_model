Linear Model
================
Zhaohua Chunyu
2022-11-20

Load NYC Airbnb data.

``` r
data("nyc_airbnb")
nyc_airbnb =
  nyc_airbnb %>% 
  mutate(
    stars = review_scores_location / 2) %>% 
  rename(
    borough = neighbourhood_group
  ) %>% 
  filter(borough != "Staten Island") %>% 
  select(price, stars, borough, neighbourhood, room_type)
```

## Fit the first model

``` r
fit = lm(price ~ stars + borough, data = nyc_airbnb)

fit %>% 
  broom::tidy() %>% 
  mutate(
    term = str_replace(term, "borough", "Borough:")
  ) %>% 
  select(term, estimate, p.value) %>% 
  knitr::kable(digits = 2)
```

| term              | estimate | p.value |
|:------------------|---------:|--------:|
| (Intercept)       |   -70.41 |    0.00 |
| stars             |    31.99 |    0.00 |
| Borough:Brooklyn  |    40.50 |    0.00 |
| Borough:Manhattan |    90.25 |    0.00 |
| Borough:Queens    |    13.21 |    0.15 |

Let’s change reference category.

``` r
fit =
  nyc_airbnb %>%  
  mutate(borough = fct_infreq(borough)) %>% 
  lm(price ~ stars + borough, data = .)

fit %>% 
  broom::tidy() %>% 
  mutate(
    term = str_replace(term, "borough", "Borough:")
  ) %>% 
  select(term, estimate, p.value) %>% 
  knitr::kable(digits = 2)
```

| term             | estimate | p.value |
|:-----------------|---------:|--------:|
| (Intercept)      |    19.84 |     0.1 |
| stars            |    31.99 |     0.0 |
| Borough:Brooklyn |   -49.75 |     0.0 |
| Borough:Queens   |   -77.05 |     0.0 |
| Borough:Bronx    |   -90.25 |     0.0 |

``` r
fit %>% 
  broom::glance() %>% 
  select(AIC)
```

    ## # A tibble: 1 × 1
    ##       AIC
    ##     <dbl>
    ## 1 404237.
