### recession_predictR

A new and improved way to predict recessions (and recoveries) of the US economy inspired by Harvey's work on the inverted yield curve.  
  
Recession prediction can be thought of as an exercise of two parts, GDP prediction and classification of the state of the economy. GDP forecasts were obtained using linear regressions and the classification was done using a random forest model.  
  
**Datasets**  
 - GDP base level  
 - Yield curve  
 - Electricity generated in the lower 48 states (hourly)  
 - OECD Business Tendency Surveys for Manufacturing: United States  
 - Number of active oil and natural gas rigs  
 - Employment: Long-term unemployment rate, Probability of job loss; Job openings; Ratio of hires to openings  
 - Economic Policy Index  
 - Equity Uncertainty  
  
**How to Access**  
Ensure that you have an Energy Information Administration and St. Louis Fed Research API keys before running the `model_build.Rmd`.  
Ensure that you have the **latest** repo cloned.  
Ensure that the relevant packages are installed, especially `shiny`, before running `model_presentation.Rmd` as it is an `ioslide` with a `shiny` feature embedded.  
  
Cheers,  
Wesley Chioh and Jonathan O'Kane | May 4, 2020 
 
