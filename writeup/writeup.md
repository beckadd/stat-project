An Analysis of African Systemic Crises
================
Beck Addison, Jerry Lin, Isabella Swigart, Emma Hirschkop
12/13/2019

Your project writeup goes here\! Before you submit, make sure your
chunks are turned off with `echo = FALSE`.

## Introduction

We can visualize what years data for economies in African countries is
given.

![](writeup_files/figure-gfm/countries-included-1.png)<!-- -->![](writeup_files/figure-gfm/countries-included-2.png)<!-- -->

## Data Analysis

## Question 1: How does independence impact a country’s economic stability?

As African countries made a transition to becoming independent states,
many were plagued by violence or political unrest. A bloody 10-year war
in Algeria culminated in its independence; lengthy wars were fought in
Angola and Mozambique, sectional divides fraught the Congo, and a
political revolt ocurred in Kenya. Many of these movements brought
economic instability to their countries. However, at the same time,
these independence movements were fought in the hopes that establishing
an independent nation would bring greater economic prosperity and
stability to their country.

Thus, on a broad level, we’d like to determine how these independence
movements ultimately affected the stability of their countries’
economies.

Let’s first do some exploratory data analysis. For each country, let’s
ask: how many years after independence will a country typically
experience its next crisis?

![](writeup_files/figure-gfm/independence-1.png)<!-- -->

| Interquartile Range | Median |     Mean |
| ------------------: | -----: | -------: |
|                  11 |     30 | 31.30769 |

We see that the median amount of years a country will first encounter a
banking crisis after they achieve independence is about 30 years, with
an interquartile range of 11 years and a mean of 31.3 years.

This is fascinating, but we’re more interested in seeing if there’s a
difference in the economic stability of independent vs. colonized
African countries. In particular, we’re wondering if post-independence
African countries see a higher proportion of systemic crises (per year)
compared to before independence, when they were colonized. Let’s examine
it:

|                           | Proportion of Years with Crises |
| ------------------------- | ------------------------------: |
| Independent Countries     |                       0.0042194 |
| Non-independent Countries |                       0.0985401 |

Based on our sample, we see that on average, there is a 0.42% chance of
a systemic crisis occurring in any given year for a non-independent
(i.e. colonized) country, while there is a 9.85% chance of a systemic
crisis occurring in any given year for an independent country. The
difference in these proportions is 0.094320737.

We’d like to conduct a hypothesis test to see if the proportion of years
with systemic crises for African countries is higher post-independence
compared to pre-independence. Our null hypothesis is that the proportion
of years with systemic crises between African countries
post-independence and pre-independence is the same; the observed
difference is due to chance. Our alternative hypothesis is that the
proportion of years with systemic crises for African countries is higher
post-independence than pre-independence.

Since we’re testing for independence, we’ll use permutation. We’ll
modify our dataset slightly by factoring success and systemic\_crisis
into a categorical variable so that it’ll work nicely with infer.

![](writeup_files/figure-gfm/crisis-prop-diff-1.png)<!-- -->

| p-value |
| ------: |
|       0 |

Since our p-value of 0 is less than our significance level of 0.05, we
reject the null hypothesis. The data provides convincing evidence that
the proportion of years with systemic crises for African countries is
higher post-independence than pre-independence.

Ultimately, we can conclude that after gaining independence, African
countries experience greater economic instability compared to when they
were under colonial
rule.

## Question 2: Are there differences in economic development and stability between North African and sub-Saharan African Countries?

#### Is there a difference in GDP and frequency of systemic crises between North African and sub-Saharan African countries?

Today, many newspapers, academics, and policymakers classify Africa into
two broad regions. There’s North Africa, consisting of nations like
Algeria and Morocco, and sub-Saharan Africa, which conists of countries
ranging from the Central African Republic to Botswana. Traditionally,
people associate sub-Saharan Africa with being less developed and more
impoverished compared to the rest of the continent. In recent years,
reports by the World Bank have claimed that more and more of the world’s
poor are being concentrated into a few sub-Saharan countries.

We set out to see if this is claim is true; that is, if there’s a
noticeable difference in economic stability and prosperity between North
African and sub-Saharan African countries. We can evaluate these claims
in two ways: by analyzing our data on each country’s GDP and the
proportion of years with systemic crises for each country.

To answer this question, we need to label North African and sub-Saharan
countries in our Africa dataset.

Let’s calculate the median GDP for North African and sub-Saharan
countries. When calculating GDP by region, we’ll use 2013 GDP data since
it’s recent and available for 11 of the 13 African countries in our
dataset.

| Region       | Median GDP (in Billions of $) |
| :----------- | ----------------------------: |
| North Africa |                         106.8 |
| Sub-Saharan  |                          41.6 |

Let’s also visualize the median GDP by region in 2013.
![](writeup_files/figure-gfm/visualize-median-GDP-1.png)<!-- -->

The IQR for sub-Saharan countries is much larger than the IQR for North
African countries, demonstrating larger variability. The country with
the greatest GDP is a sub-Saharan country, which is an outlier for its
region. However, the median 2014 GDP for sub-Saharan countries is less
than North African countries.

The median GDP for North African countries is 106.826 billion dollars;
the median GDP for sub-Saharan countries is 41.5710942 billion dollars.
Therefore, the difference in median GDP between North African and
sub-Saharan countries is 65.2549058 billion dollars.

The first research question we’ll ask is: is the median GDP of North
African countries greater than the median GDP of sub-Saharan countries?

Our null hypothesis is that the median GDP of North African and
sub-Saharan countries is the same; the observed difference is due to
chance. Our alternative hypothesis is that the median GDP of North
African countries is greater than the median GDP of sub-Saharan African
countries.

Since we’re testing for independence, we’ll use permute.
![](writeup_files/figure-gfm/gdp-prop-diff-1.png)<!-- -->

| p-value |
| ------: |
|   0.339 |

Since our p-value of 0.339 is greater than our significance level of
0.05, we fail to reject the null hypothesis. The data does not provide
convincing evidence that the median 2013 GDP of North African countries
is greater than the median GDP of sub-Saharan countries.

Next, let’s calculate the proportion of years with systemic crises for
North African and sub-Saharan countries.

| region       | overall\_crisis\_prop |
| :----------- | --------------------: |
| North Africa |             0.0435897 |
| Sub-Saharan  |             0.0971599 |

The proportion of years with systemic crises for North African countries
is 0.0436; the proportion of years with systemic crises for sub-Saharan
countries is 0.0971. The difference is 0.0535702.

The second research question we’ll ask is: do sub-Saharan countries have
a greater proportion of years with systemic crises than North African
countries?

Our null hypothesis is that the proportion of years with systemic crises
between North African and sub-Saharan countries is the same; the
observed difference is due to chance. Our alternative hypothesis is that
the proportion of years with systemic crises for sub-Saharan countries
is greater than for North African countries.

Since we’re testing for independence, we’ll use permute.
![](writeup_files/figure-gfm/region-prop-diff-1.png)<!-- -->

| p-value |
| ------: |
|       0 |

Since our p-value of 0 is less than the significance level of 0.05, we
reject the null hypothesis. The data provides convincing evidence that
sub-Saharan countries have a greater proportion of years with systemic
crises compared to North African countries.

From our results, we can conclude that historically — when accounting
for all years from the 19th century to today — sub-Saharan African
countries have been more prone to systemic crises than North African
countries. However, the data did not provide convincing evidence that
the median 2013 GDP of North African countries was greater than the
median 2013 GDP of sub-Saharan countries. That’s a positive sign; it
demonstrates that even though there have been historical disparities
between these two regions, in the 21st century, sub-Saharan Africa is
catching up, if not matching up to, North African
economies.

## Question 3: Can we produce a model to predict economic crises in African economies?

To determine what factors most impacted African economies, we designed
three linear regression models with the goal of using these to predict
economic crises in African economies. Given the limits of our own
statistical knowledge, we were not able to give a regression model that
directly predicted a systemic crisis, though we used the nation’s change
in CPI (delta CPI) and change in GDP (delta GDP) as indirect and
discrete measures of a country’s state of crisis. We claimed that, if
the delta GDP of a nation was particularly negative, or if the delta
Consumer Price Index (CPI) – a measure used to study a nation’s
inflation rate – was particularly high, we could claim that these
indicated an economic crisis.

After plotting the models and performing backwards selection, optimizing
for AIC, we found that a significant (p \< 0.001) negative correlation
between a country in a currency crisis and whether they faced a systemic
economic crisis overall; on average, a country undergoing a currency
crisis was expected to have a delta GDP of about -4.74 billion USD,
holding all else constant. Interestingly, we did not find a significant
correlation (p = 0.073) between inflation crisis and delta GDP, nor did
we find a significant correlation (p = 0.137) between a country’s
debt-to-GDP ratio and delta GDP. Finally, we found that there was a
significant positive correlation (p \< 0.001) between a country’s GDP
and changes to their GDP, though it was relatively minor – for every
additional $USD in the country’s GDP, their delta GDP was expected to,
on average, be about nine cents higher.

## Conclusion

Conclusion goes here.

### Critiques

While our models provide a fairly significant explanation for the
variation in the dataset, with adjusted r-squared values of 0.3 or
higher, it is difficult to confidently say whether they can predict
economic crisis or not, given that we are not able to directly compare
economic crisis and the explanatory factors of our models. In addition,
our current models look at the change in GDP and CPI, which we believe
to be a better indicator of economic stability than the GDP or CPI
itself, we might improve our models by looking at the change in GDP as a
proportion of the GDP overall. By doing this, we level our comparisons
across countries such that a crisis for a small economy (where hundreds
of millions USD might make the difference between stability and crisis)
can be compared similarly to a crisis for a large economy (where a delta
GDP of hundreds of billions USD might only make a small difference in
the economic productivity of the nation).

Finally, our model could certainly benefit from better generalizability.
Given the constraints of our dataset and the scarcity of economic data
for African countries prior to 1950, the amount of data provided for
some African countries was so sparse that it would be difficult to
confidently say that our model accurately explained their economic
outcomes.
