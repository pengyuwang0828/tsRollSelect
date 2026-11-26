# tsRollSelect

Compare ETS, ARIMA, Random Forest, and XGBoost for a regularly spaced,
univariate time series. Each model uses the same expanding training
windows and forecast horizon. The package ranks them by RMSE (or MAE),
refits the winner on all observations, and returns point forecasts.

```r
library(tsRollSelect)

result <- select_forecaster(AirPassengers, h = 2, n_origins = 5)
result$best_model
result$scores
result$forecast
```

Supply a numeric vector for a nonseasonal series or an R `ts` object to
specify a seasonal period, such as `frequency = 12` for monthly data with
annual seasonality. Use sufficiently long, complete data. Each backtest
forecasts the next `h` observations and pools errors across all steps.
For `h = 2`, both steps affect selection by default. Use
`select_forecaster(y, h = 2, score_horizons = 2)` to compare only step 2.
The example uses the built-in `AirPassengers` series in place of `y`.
A model failure is reported in the score table.

These scores are used for selection. Reserve a later period when you need an
independent assessment of the chosen approach. This release produces point
forecasts and supports a single regularly spaced series at a time.
