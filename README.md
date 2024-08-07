# Gold Price Time Series Prediction
This project focuses on forecasting gold prices in the Indian market using ARIMA time series models. The analysis is based on daily gold price data from 2014 to 2022, which was converted to monthly data for better analysis and noise reduction.

Key Features:

- Data preparation and exploration of gold price trends
- Identification and handling of seasonality in gold prices
- Comparison of different ARIMA models
- Implementation of multiplicative seasonal ARIMA models
- Model selection based on AIC and residual analysis
- Rolling forecast error analysis
- 8-month price prediction for 2022

Methodology:

The project employs various time series analysis techniques, including:

- Log transformation
- Regular and seasonal differencing
- ACF and PACF analysis
- ADF tests for stationarity
- Model building and comparison (ARIMA(1,1,1) X (1,1,0)ₛ and ARIMA(0,1,2) X (1,1,0)ₛ)
- Residual analysis
- Rolling forecasts

Results:

The ARIMA(0,1,2) X (1,1,0)ₛ model was identified as the most suitable for predicting gold prices, based on its lower AIC value and clean residual plots.
