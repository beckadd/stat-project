An Analysis of African Systemic Crises
================
Beck Addison, Jerry Lin, Isabella Swigart, Emma Hirschkop
12/3/2019

Your data analysis goes
    here\!

``` r
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ───────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
africa <- read_csv("../data/african_crises.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   case = col_double(),
    ##   cc3 = col_character(),
    ##   country = col_character(),
    ##   year = col_double(),
    ##   systemic_crisis = col_double(),
    ##   exch_usd = col_double(),
    ##   domestic_debt_in_default = col_double(),
    ##   sovereign_external_debt_default = col_double(),
    ##   gdp_weighted_default = col_double(),
    ##   inflation_annual_cpi = col_double(),
    ##   independence = col_double(),
    ##   currency_crises = col_double(),
    ##   inflation_crises = col_double(),
    ##   banking_crisis = col_character()
    ## )

``` r
global <- read_csv("../data/global_crisis_data.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   Case = col_double(),
    ##   Year = col_double(),
    ##   exch_usd = col_double(),
    ##   exch_usd_alt1 = col_double(),
    ##   exch_usd_alt2 = col_double(),
    ##   exch_usd_alt3 = col_double(),
    ##   `SOVEREIGN EXTERNAL DEBT 2: DEFAULT and RESTRUCTURINGS, 1800-2012--Does not include defaults on WWI debt to United States and United Kingdom but includes post-1975 defaults on Official External Creditors` = col_double()
    ## )

    ## See spec(...) for full column specifications.

    ## Warning: 891 parsing failures.
    ##  row      col expected actual                             file
    ## 1228 exch_usd a double    n/a '../data/global_crisis_data.csv'
    ## 1229 exch_usd a double    n/a '../data/global_crisis_data.csv'
    ## 1230 exch_usd a double    n/a '../data/global_crisis_data.csv'
    ## 1231 exch_usd a double    n/a '../data/global_crisis_data.csv'
    ## 1232 exch_usd a double    n/a '../data/global_crisis_data.csv'
    ## .... ........ ........ ...... ................................
    ## See problems(...) for more details.

``` r
gdps <- read_csv("../data/world_gdp_data.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character()
    ## )
    ## See spec(...) for full column specifications.

Our analysis begins with two datasets: our original African economic
crises dataset, which contains numerical variables like inflation rate,
exchange rate against the US dollar, and debt vs. GDP ratio. Another
helpful variable to analyze would be each country’s GDP, so we
downloaded world GDP data from the World Bank, and joined this data with
our Africa dataset.

``` r
gdps <- gdps %>%
  pivot_longer(
    cols = c(
      -`Country Name`,
      -`Country Code`,
      -`Indicator Name`,
      -`Indicator Code`),
    names_to = "year"
  ) %>%
  rename(
    cc3 = `Country Code`,
    indicator_code = `Indicator Code`,
    indicator_name = `Indicator Name`,
    country = `Country Name`,
    gdp = value
    )

african_gdps <- gdps %>%
  merge(., africa, by = c("country","cc3","year"))

global <- global[-1,] %>%
  select(-`<`) %>%
  rename(
    case = Case,
    cc3 = CC3,
    country = Country,
    year = Year,
    banking_crisis = `Banking Crisis`,
    systemic_crisis = `Systemic Crisis`,
    gold_standard = `Gold Standard`,
    nat_currency = `national currency`,
    sov_ext_debt = 
    `SOVEREIGN EXTERNAL DEBT 1: DEFAULT and RESTRUCTURINGS, 1800-2012--Does not include defaults on WWI debt to United States and United Kingdom and post-1975 defaults on Official External Creditors`,
    sov_ext_debt_post1975 = `SOVEREIGN EXTERNAL DEBT 2: DEFAULT and RESTRUCTURINGS, 1800-2012--Does not include defaults on WWI debt to United States and United Kingdom but includes post-1975 defaults on Official External Creditors`,
    exc_primary = `exch_primary source code`,
    domest_debt_default = `Domestic_Debt_In_Default`,
    domest_debt_notes = `Domestic_Debt_ Notes/Sources`,
    default_ext_notes = `Defaults_External_Notes`,
    gdp_weighted_default = `GDP_Weighted_default`,
    inflat_aver_consumer_prices = `Inflation, Annual percentages of average consumer prices`,
    curr_crises = `Currency Crises`,
    independence = `Independence`,
    inflat_crises = `Inflation Crises`
) %>%
  mutate(
    banking_crisis = as.logical(as.numeric(
      banking_crisis
      )),
    systemic_crisis = as.logical(as.numeric(
      systemic_crisis
    )),
    gold_standard = as.logical(as.numeric(
      gold_standard
    )),
    domest_debt_default = as.logical(as.numeric(
      domest_debt_default
    )),
    independence = as.logical(as.numeric(
      independence
    )),
    curr_crises = as.logical(as.numeric(
      curr_crises
    )),
    inflat_crises = as.logical(as.numeric(
      inflat_crises
    ))
  )
```

    ## Warning: NAs introduced by coercion
    
    ## Warning: NAs introduced by coercion
    
    ## Warning: NAs introduced by coercion
    
    ## Warning: NAs introduced by coercion
    
    ## Warning: NAs introduced by coercion
