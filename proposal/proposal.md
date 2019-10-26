From Cape to Cairo: Following African Economic Crises from the Colonial
Era to through the Modern Age
================
Six-Thirty-Eight
10/22/19

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

### Section 1. Introduction

For our STA 199 project, we chose to examine the stability and
development of the economies of African nations, particularly in the
context of colonialism and decolonialism. After we had read an article
about Zimbabwe’s crippling inflation crisis (in which the nation
completely abandoned paper currencies altogether), we wondered: how has
Africa’s history of exploitation and unrest contributed to the
continent’s overall economic instability in the modern day, and what
factors be correlated with an economic crisis in the continent? Thus, we
collected three datasets to answer this question and the questions that
precipitated from it. The first of these datasets was an excellently
curated “African Economic Banking and Systemic Crises Data” from a
Kaggle
(<https://www.kaggle.com/chirin/africa-economic-banking-and-systemic-crisis-data/version/1#>)
and the global economic crisis dataset from which it was derived,
created by the Harvard Business School
(<https://www.hbs.edu/behavioral-finance-and-financial-stability/data/Pages/global.aspx>).
While these datasets gave insights into instances of “systemic crisis”
and exchange rate and CPI information (which indicates relative
inflation and debt), they critically omitted the GDP of these nations
over time – a fairly modern economic growth metric that we were only
able to find being consistently used from 1960 onward. We sourced and
cleaned a record of this global GDP data from the World Bank
(<https://data.worldbank.org/indicator/ny.gdp.mktp.cd>), detailing world
nations’ GDPs in USD from 1960 to the modern day. We believe that this
dataset will be sufficient to detail the impacts of decolonization on
African nations’ GDPs (as most African decolonization efforts took place
in the mid-to-late 1950s and early 1960s) and provide a working
conclusion to our research questions.

### Section 2. Exploratory data analysis

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
  rename(cc3 = `Country Code`, country = `Country Name`, gdp = value)

africangdps <- gdps %>%
  merge(., africa, by = c("country","cc3","year"))
```

To start, we can visualize the number of years with a banking crisis in
African countries as a bar graph. We can also look at how African GDPs
compare to nations in other continents to emphasize why our question of
instability is important.

``` r
africa %>%
ggplot(aes(x = banking_crisis)) +
  geom_bar(fill = "pink") +
  labs(
    x = "Banking Crisis", 
    title = "Number of Years with Banking Crises in African Countries"
    )
```

![](proposal_files/figure-gfm/banking-crisis-1.png)<!-- -->

We can create a plot of all the countries included in the African
economic dataset and what years data is given for them.

``` r
ggplot(data = africa, mapping = aes(x = year)) +
  geom_histogram(binwidth = 10, fill = "blue", col = "black") +
  labs(
    x = "Year",
    y = "Number of Countries", 
    title = "Increase in Total Number of African
    Country Economic Observations Over Time"
    )
```

![](proposal_files/figure-gfm/countries-included-1.png)<!-- -->

``` r
ggplot(data = africa, mapping = aes(x = country, y = year)) +
  geom_point(size = .75, col = "orange") +
  coord_flip() +
  labs(
  x = "Country",
  y = "Year",
  title = "African Economic Data Observed Each Year by Country"
  )
```

![](proposal_files/figure-gfm/countries-included-2.png)<!-- -->

Overall, observations of African economies greatly increased after 1950.
However, there are some African countries, such as Egypt and South
Africa, whose economies have been monitored for over a century.

Next, we can determine the percentage of systemic crisis years for each
country in the Africa dataset.

``` r
africa %>%
  count(country, systemic_crisis) %>%
  group_by(country) %>%
  mutate(crisis_proportion = 1- (n/sum(n))) %>%
  filter(systemic_crisis == 0) %>%
  select(country, crisis_proportion) %>%
  arrange(desc(crisis_proportion))
```

    ## # A tibble: 13 x 2
    ## # Groups:   country [13]
    ##    country                  crisis_proportion
    ##    <chr>                                <dbl>
    ##  1 Central African Republic            0.328 
    ##  2 Kenya                               0.194 
    ##  3 Nigeria                             0.167 
    ##  4 Zimbabwe                            0.167 
    ##  5 Tunisia                             0.0667
    ##  6 Ivory Coast                         0.0635
    ##  7 Zambia                              0.0556
    ##  8 Algeria                             0.0471
    ##  9 Egypt                               0.0387
    ## 10 Morocco                             0.0267
    ## 11 Angola                              0     
    ## 12 Mauritius                           0     
    ## 13 South Africa                        0

The Central African Republic had the highest proportion of recorded
years with systemic crises, with 32.8% of the years recorded having a
systemic crisis. Out of the 13 African countries in the dataset, only
South Africa, Angola, and Mauritius had no systemic crises.

Now, we’ll explore an interesting question. Independence movements are
tricky - while they might achieve democracy and self-governance for a
country, they often contribute to political and economic instability.
For each country, we’ll determine how many years after independence a
country will typically experience it’s next crisis.

``` r
output <- tibble(country = distinct(africa, country)$country)
output$independence_year <- africa %>%
  filter(independence == 1) %>%
  group_by(country) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  select(year)
  
output$crisis_year <- africa %>%
  filter(independence == 1) %>%
  group_by(country) %>%
  filter(banking_crisis == "crisis") %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(year)
output <- output %>%
  mutate(difference = (crisis_year$year - independence_year$year))
ggplot(data = output, mapping = aes(y = difference)) +
  geom_boxplot() + 
  labs(
    title = "What's the typical amount of years between a country 
    achieving independence and its next financial crisis?",
    y = "Number of Years")
```

![](proposal_files/figure-gfm/independence-1.png)<!-- -->

``` r
output %>%
  summarise(IQR = IQR(difference), median = median(difference))
```

    ## # A tibble: 1 x 2
    ##     IQR median
    ##   <dbl>  <dbl>
    ## 1    11     30

We see that the median amount of years a country will first encounter a
banking crisis after they achieve independence is about 30 years, with
an interquartile range of 11 years. This brings us to an interesting
question - what values characterize countries that had a long period of
economic prosperity after independence, and what characterizes countries
that saw immediate financial crises?

Next, we’ll do a univariate visualization of what the exchange rate
variable looks like:

``` r
ggplot(data = africa, mapping = aes(x = exch_usd)) +
  geom_histogram(bins = 10)
```

![](proposal_files/figure-gfm/exchange-rate-visualize-1.png)<!-- -->

``` r
summary_stats <- africa %>%
  summarise(median = median(exch_usd), std = sd(exch_usd))
```

We see that the median exchange rate against the US dollar is 0.8684 and
the standard deviation is 111.47538. The distribution is right-skewed.

Next, let’s analyze if exchange rate against the dollar and inflation
rate correlate with economic crises.

``` r
africa %>%
ggplot(
  aes(
    x = inflation_annual_cpi,
    y = exch_usd,
    color = banking_crisis)) +
  geom_point() + 
  xlim(-10, 100)
```

![](proposal_files/figure-gfm/inflation-crisis-relationship-1.png)<!-- -->

We see that high levels of inflation often correspond to banking crises,
but this is not always true. There were many cases of banking crises
that existed even when inflation was lower than typical. Moreover,
banking crises also tended to exist when exchange rates against the
dollar were high. This could point to a pretty interesting research
question: what levels of inflation and exchange rate against the dollar
will signal a banking crisis (or any crisis in general?)

### Section 3. Research questions

Questions:

1.  How did major European economies change with colonialism? How did
    African economies change when colonized? Conversely, how did
    European/African economies change with decolonization of the African
    continent?

Note: there are a lot of factors that affect a European economies, and
losing a colony might not be that impactful. Concentrate on African
economies in this analysis.

2.  Was an economic crisis more likely following n years after
    decolonization?

3.  What factors are most associated with a systemic crisis in Africa?
    How can identify a systemic crisis in the dataset? What defines a
    systemic crisis, and how do African systemic crises compare to other
    nations’ crises?

### Section 4. Data

``` r
glimpse(africa)
```

    ## Observations: 1,059
    ## Variables: 14
    ## $ case                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ cc3                             <chr> "DZA", "DZA", "DZA", "DZA", "DZA…
    ## $ country                         <chr> "Algeria", "Algeria", "Algeria",…
    ## $ year                            <dbl> 1870, 1871, 1872, 1873, 1874, 18…
    ## $ systemic_crisis                 <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ exch_usd                        <dbl> 0.052264, 0.052798, 0.052274, 0.…
    ## $ domestic_debt_in_default        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ sovereign_external_debt_default <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ gdp_weighted_default            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ inflation_annual_cpi            <dbl> 3.441456, 14.149140, -3.718593, …
    ## $ independence                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ currency_crises                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ inflation_crises                <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,…
    ## $ banking_crisis                  <chr> "crisis", "no_crisis", "no_crisi…

``` r
glimpse(global)
```

    ## Observations: 15,191
    ## Variables: 27
    ## $ Case                                                                                                                                                                                                         <dbl> …
    ## $ CC3                                                                                                                                                                                                          <chr> …
    ## $ Country                                                                                                                                                                                                      <chr> …
    ## $ Year                                                                                                                                                                                                         <dbl> …
    ## $ `Banking Crisis`                                                                                                                                                                                             <chr> …
    ## $ Banking_Crisis_Notes                                                                                                                                                                                         <chr> …
    ## $ `Systemic Crisis`                                                                                                                                                                                            <chr> …
    ## $ `Gold Standard`                                                                                                                                                                                              <chr> …
    ## $ exch_usd                                                                                                                                                                                                     <dbl> …
    ## $ exch_usd_alt1                                                                                                                                                                                                <dbl> …
    ## $ exch_usd_alt2                                                                                                                                                                                                <dbl> …
    ## $ exch_usd_alt3                                                                                                                                                                                                <dbl> …
    ## $ conversion_notes                                                                                                                                                                                             <chr> …
    ## $ `national currency`                                                                                                                                                                                          <chr> …
    ## $ `exch_primary source code`                                                                                                                                                                                   <chr> …
    ## $ exch_sources                                                                                                                                                                                                 <chr> …
    ## $ Domestic_Debt_In_Default                                                                                                                                                                                     <chr> …
    ## $ `Domestic_Debt_ Notes/Sources`                                                                                                                                                                               <chr> …
    ## $ `SOVEREIGN EXTERNAL DEBT 1: DEFAULT and RESTRUCTURINGS, 1800-2012--Does not include defaults on WWI debt to United States and United Kingdom and post-1975 defaults on Official External Creditors`          <chr> …
    ## $ `SOVEREIGN EXTERNAL DEBT 2: DEFAULT and RESTRUCTURINGS, 1800-2012--Does not include defaults on WWI debt to United States and United Kingdom but includes post-1975 defaults on Official External Creditors` <dbl> …
    ## $ Defaults_External_Notes                                                                                                                                                                                      <chr> …
    ## $ GDP_Weighted_default                                                                                                                                                                                         <chr> …
    ## $ `<`                                                                                                                                                                                                          <chr> …
    ## $ `Inflation, Annual percentages of average consumer prices`                                                                                                                                                   <chr> …
    ## $ Independence                                                                                                                                                                                                 <chr> …
    ## $ `Currency Crises`                                                                                                                                                                                            <chr> …
    ## $ `Inflation Crises`                                                                                                                                                                                           <chr> …

``` r
glimpse(gdps)
```

    ## Observations: 15,576
    ## Variables: 6
    ## $ country          <chr> "Aruba", "Aruba", "Aruba", "Aruba", "Aruba", "A…
    ## $ cc3              <chr> "ABW", "ABW", "ABW", "ABW", "ABW", "ABW", "ABW"…
    ## $ `Indicator Name` <chr> "GDP (current US$)", "GDP (current US$)", "GDP …
    ## $ `Indicator Code` <chr> "NY.GDP.MKTP.CD", "NY.GDP.MKTP.CD", "NY.GDP.MKT…
    ## $ year             <chr> "1960", "1961", "1962", "1963", "1964", "1965",…
    ## $ gdp              <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
