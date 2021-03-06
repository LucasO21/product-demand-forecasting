# Product Demand Forecasting with Modeltime and Timetk

Code script [here](https://github.com/LucasO21/Product-Demand-Forecasting/blob/main/script_product_forecasting.R)

Detailed write up available [here](https://lucasoblog.netlify.app)

A short project, forecasting demand for products at the product category and weekly level for a chain of stores. 

## Approach

#### Exploratory Data Analysis -
This was performed to analyze trends. This included examining ACF and PACF, anomaly diagnostics, seasonality diagnostics, etc. Observations made was taken into account while engineering features for modeling.

### Feature Engineering -
New features were engineered to capture trends in the data. These included - 

* Lags
* Rolling Averages
* Fourier Series

### Modeling -
6 models were tested including - 
* Prophet with Regressors
* Prophet Boost (Prophet & XGBOOST)
* XGBOOST
* MARS
* CUBIST
* Random Forest

### Results -
The top 3 models in terms of accuracy were Prophet with Regressors, MARS and Random Forest. The Prophet with Regressors did not perform well on the future forecast. Additional techniques can be applied to improve results seen in this model

### Tools / Packages Used -
* R Programming Language
* Modeltime Package
* Timetk Package