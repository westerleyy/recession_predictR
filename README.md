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
  
**Before Accessing**  
Ensure that you have an Energy Information Administration and St. Louis Fed Research API keys before running `model_build.Rmd`.  
Ensure that you have the **latest** repo cloned.  
Ensure that the relevant packages are installed, especially `shiny`, before running `model_presentation.Rmd` as it is an `ioslide` with a `shiny` feature embedded.  
 
**How to get to the tl;dr presentation**  
1. Clone the repo.  
2. Do NOT make any changes.  
3. Open the entire repo with the RProj file in RStudio.  
4. Open `model_presentation.Rmd`.  
5. Click `Run Presentation` near the top of the window.  
6. A new window should open with the presentation. If not, your console should have a web address something like http://127.0.0.xxxx. Copy that address into a web browser of your choice and the presentation will start.  

Cheers,  
Wesley Chioh and Jonathan O'Kane | May 5, 2020 
 
