# <b> data </b>

#### <b> africa </b>

The african_crises dataset, derived from the global_crisis_data dataset,
examines all of the nations of Africa from 1860 to 2014, and gives data that
describes the presence of an economic crisis and the approximate type of the 
crisis experienced. It contains 1059 observations with 14 variables. They are
described below.This codebook has been adapted from one given here:

https://www.kaggle.com/chirin/africa-economic-banking-and-systemic-crisis-data/version/1#

  case (dbl) : A unique number identifier between 1 and 70 that represents a
  specific African country.
  
  cc3 (chr) : The three-letter code used to refer to a nation.
  
  country (chr) : The name of the country.
  
  year (dbl) : The year in which observations were taken.
  
  systemic_crisis (dbl) : Boolean describing whether the nation underwent a
  systemic economic crisis during the year measured. 0 is False, 1 is True.
  
  exch_usd (dbl) : Exchange rate of the country in USD.
  
  domestic_debt_in_default (dbl) : Boolean indicating whether the nation defaulted on its domestic debt during the year measured. 0 is False, 1 is True.
  
  sovereign_external_debt_default (dbl) : Boolean indicating whether the nation defaulted on its external (foreign) debts during the year measured. 0 is False, 1 is True.
  
  gdp_weighted_default (dbl) : Total debt in default as a percentage of GDP
  
  inflation_annual_cpi (dbl) : The annual CPI Inflation rate.
  
  independence (dbl) : Boolean indicating whether the nation is independent or colonized during the year measured. 0 is False, 1 is True.
  
  currency_crises (dbl) : Boolean indicating whether a currency crisis occurred during the year (a currency crisis is one in which the currency of the nation's central bank does not have enough foreign exchange reserves to sustain the country's fixed exchange rate). 0 is False, 1 is True.
  
  inflation_crises (dbl) : Boolean indicating whether an inflation crisis occurred during the year (an inflation crisis, also known as hyperinflation, occurs when a nation's inflation rate begins accelerating faster than economic growth, causing currency devaluation). 0 is False, 1 is True.
  
  banking_crises (chr) : Boolean indicating whether a banking crisis occurred during the year (a banking crisis is one in which a nation's citizens no longer trust a bank to hold funds, causing widespread bank runs and significant loss of private capital). "no_crisis" is False, "crisis" is True.
  
#### <b> global </b>

The dataset from which african_crises is based. It provides data on more than 70 countries from 1800 to 2016. It has a total of 15576 observations with 27 variables. Many of these variables are preserved in the african_crises dataset. The codebook for this dataset has been loosely adapted from the HBS dataset documentation They are described here:

https://www.hbs.edu/behavioral-finance-and-financial-stability/data/Pages/global.aspx
  

  Case (dbl) : A unique number identifier between 1 and 70 that represents a
  specific African country.
  
  CC3 (chr) : The three-letter code used to refer to a nation.
  
  Country (chr) : The name of the country.
  
  Year (dbl) : The year in which observations were taken.
  
  Banking Crisis (dbl) : Boolean indicating whether a banking crisis occurred during the year (a banking crisis is one in which a nation's citizens no longer trust a bank to hold funds, causing widespread bank runs and significant loss of private capital). 0 is False, 1 is True.
  
  Banking_Crisis_Notes (chr) : Notes on the banking crisis information.
  
  Systemic Crisis (dbl) : Boolean describing whether the nation underwent a
  systemic economic crisis during the year measured. 0 is False, 1 is True.
  
  Gold Standard (dbl) : Boolean indicating whether the nation's currency is based in a gold standard. 0 is True, 1 is False.
  
  exch_usd (dbl) : Exchange rate of the country in USD.
  
  exch_usd_alt1 (dbl) : Exchange rate examined using "pre-announced or de facto crawling peg/crawling band narrower than 2 percent variation"
  
  exch_usd_alt2 (dbl) : Exchange rate examined using "managed floating or pre-announced crawiling or moving band narrower than 2 percent variation"
  
  exch_usd_alt3 (dbl) : Exchange rate examined using "free floating"
  
  conversion_notes (chr) : notes on the conversion of currencies for the purposes of the exchange rate and other measurements in USD.
  
  national currency (chr) : notes on the national currency at the time of measurement.
  
  exch_primary source code (chr) : metadata on exchange rate information.
  
  exch_sources (chr) : data source for exchange rate information.
  
  Domestic_Debt_In_Default (dbl) : Boolean indicating whether the nation defaulted on its domestic debt during the year measured. 0 is False, 1 is True.
  
  Domestic_Debt_Notes/Source (chr) : Notes on domestic debt variable.
  
  SOVEREIGN EXTERNAL DEBT 1: DEFAULT and RESTRUCTURINGS, 1800-2012 -- Does not include defaults on WWI debt to United States and United Kingdom and post-1975 defaults on Official External Creditors (dbl) : boolean indicating whether a nation defaulted or restructured debt based on information given in variable name. 0 is False, 1 is True.
  
  SOVEREIGN EXTERNAL DEBT 2: DEFAULTS and RESTRUCTURINGS, 1800-2012--Does not include defaults on WWI debt to United States and United Kingdom but includes post-1975 defaults on Official External Creditors (dbl) : boolean indicating whether a nation defaulted or restructured debt based on information given in variable name. 0 is False, 1 is True.
  
  Defaults_External_Notes (chr) : Notes on external defaults.

  GDP_weighted_default (dbl) : Total debt in default as a percentage of GDP
  
  Inflation, Annual percentages of average consumer prices (dbl) : The annual CPI Inflation rate.
  
  Independence (dbl) : Boolean indicating whether the nation is independent or colonized during the year measured. 0 is False, 1 is True.
  
  Currency Crises (dbl) : Boolean indicating whether a currency crisis occurred during the year (a currency crisis is one in which the currency of the nation's central bank does not have enough foreign exchange reserves to sustain the country's fixed exchange rate). 0 is False, 1 is True.
  
  Inflation Crises (dbl) : Boolean indicating whether an inflation crisis occurred during the year (an inflation crisis, also known as hyperinflation, occurs when a nation's inflation rate begins accelerating faster than economic growth, causing currency devaluation). 0 is False, 1 is True.
  
#### <b> gdps </b>

A comprehensive record of the GDPs of the world's nations from 1960 to 2018. Provided by the World Bank, this dataset has been cleaned and rearranged for the purposes of this project. The codebook for the cleaned data is below. 

Source of original dataset:
https://data.worldbank.org/indicator/ny.gdp.mktp.cd

  country (chr) : Name of country.
  
  cc3 (chr) : 3-letter country code.
  
  Indicator Name (chr) : Indicator and monetary unit used to measure GDP. For this dataset, it is only in current USD.
  
  Indicator Code (chr) : metadata on Indicator Name - not used in this assignment.
  
  year (dbl) : Year of data collection.
  
  gdp (dbl) : Country's GDP at year of collection.
  
#### <b> african_gdps </b>

A merge between the gdps dataset and the africa dataset by year, country code and country name. It contains all of the variables of the gdps dataset, filtered for only nations in Africa.
  
  

