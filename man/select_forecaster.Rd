\name{select_forecaster}
\alias{select_forecaster}
\title{Select and refit a time series forecaster}
\usage{
select_forecaster(y, h = 1L, initial = NULL, n_origins = 5L,
                  metric = c("RMSE", "MAE"), lags = NULL, seed = 1L,
                  score_horizons = NULL)
}
\arguments{
  \item{y}{Finite numeric vector or univariate \code{ts} of regularly
  spaced observations in chronological order. Supply a \code{ts} with an
  integer frequency greater than one to indicate a seasonal period.}
  \item{h}{Positive integer forecast horizon.}
  \item{initial}{Size of the earliest training window. Defaults to the
  larger of 12 and 65 percent of the series length rounded down.}
  \item{n_origins}{Maximum number of forecast origins, at least two.
  Origins are approximately evenly spaced from \code{initial} through the
  last origin with \code{h} observed targets.}
  \item{metric}{Ranking metric, either root mean squared error (RMSE) or
  mean absolute error (MAE). Ties use the other metric, then model name.}
  \item{lags}{Optional positive integer lags for the machine learning models.
  Defaults to 1, 2, 3 and the seasonal period when it leaves at least eight
  training rows.}
  \item{seed}{Positive integer seed used for machine learning fits.}
  \item{score_horizons}{Forecast steps to score. Defaults to all steps from
  1 through \code{h}. The forecast still returns all \code{h} steps.}
}
\description{
Compares ETS, automatic ARIMA, Random Forest, and XGBoost on common rolling
forecast origins, then refits the lowest-error candidate on the full series.
}
\details{
The training window expands at each origin. Errors are pooled across all
scored steps and origins, with equal weight per prediction. Random Forest
and XGBoost use recursive lag forecasts with fixed hyperparameters. A candidate
that fails on any origin is excluded and its error is reported in the scores.
The final fit may require more time than any individual rolling fit.
Selection scores describe the stated backtest and should not be treated as
an independent evaluation of the selected model.
}
\value{
A list with \code{best_model}, \code{metric}, \code{score_horizons}, \code{scores} (including failure
reasons), \code{backtest} (per-origin predictions), \code{forecast} (point
predictions and time indices), and \code{fit} (the refitted model object).
}
\examples{
y <- as.numeric(datasets::Nile)[1:40]
result <- select_forecaster(y, h = 2, n_origins = 2)
result$scores
result$forecast
}
