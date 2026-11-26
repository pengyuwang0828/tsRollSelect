#' Select a forecaster by expanding-window backtesting
select_forecaster <- function(y, h = 1L, initial = NULL, n_origins = 5L,
                              metric = c("RMSE", "MAE"), lags = NULL,
                              seed = 1L, score_horizons = NULL) {
  metric <- match.arg(metric)
  if (!is.numeric(y) || !is.null(dim(y)) || any(!is.finite(y))) {
    stop("y must be a finite, regularly spaced numeric vector or univariate ts.",
         call. = FALSE)
  }
  h <- whole_number(h, "h", 1L)
  if (is.null(score_horizons)) score_horizons <- seq_len(h)
  if (!is.numeric(score_horizons) || !length(score_horizons) ||
      any(!is.finite(score_horizons)) ||
      any(score_horizons < 1 | score_horizons > h |
          score_horizons != floor(score_horizons))) {
    stop("score_horizons must contain integers between 1 and h.", call. = FALSE)
  }
  score_horizons <- sort(unique(as.integer(score_horizons)))
  n_origins <- whole_number(n_origins, "n_origins", 2L)
  seed <- whole_number(seed, "seed", 1L)
  period <- whole_number(stats::frequency(y), "frequency(y)", 1L)
  values <- as.numeric(y)
  n <- length(values)
  if (is.null(initial)) initial <- max(12, floor(0.65 * n))
  initial <- whole_number(initial, "initial", 12L)
  if (n - h <= initial) {
    stop("Need at least two backtest origins: length(y) must exceed initial + h.",
         call. = FALSE)
  }
  if (is.null(lags)) {
    lags <- 1:3
    if (period > 1L && initial - period >= 8L) lags <- sort(unique(c(lags, period)))
  }
  if (!is.numeric(lags) || !length(lags) || any(!is.finite(lags)) ||
      any(lags < 1 | lags != floor(lags) | lags > initial - 8L)) {
    stop("lags must contain positive integers and leave at least eight training rows.",
         call. = FALSE)
  }
  lags <- sort(unique(as.integer(lags)))
  origins <- as.integer(round(seq(initial, n - h,
    length.out = min(n_origins, n - h - initial + 1L))))
  models <- c("ETS", "ARIMA", "Random Forest", "XGBoost")

  comparisons <- lapply(models, function(model) {
    tryCatch({
      pieces <- lapply(origins, function(origin) {
        history <- values[seq_len(origin)]
        fit <- fit_forecaster(model, history, period, lags, seed)
        predicted <- forecast_model(fit, model, history, h, lags)
        index <- origin + seq_len(h)
        data.frame(model = model, origin = origin, horizon = seq_len(h),
          target = index, predicted = predicted, actual = values[index])
      })
      list(predictions = do.call(rbind, pieces), reason = NA_character_)
    }, error = function(e) list(predictions = NULL, reason = conditionMessage(e)))
  })
  scores <- do.call(rbind, lapply(seq_along(models), function(i) {
    rows <- comparisons[[i]]$predictions
    rows <- rows[rows$horizon %in% score_horizons, ]
    errors <- if (is.null(rows)) NA_real_ else rows$actual - rows$predicted
    data.frame(model = models[i], RMSE = sqrt(mean(errors^2)), MAE = mean(abs(errors)),
      status = if (is.null(rows)) "failed" else "ok", reason = comparisons[[i]]$reason)
  }))
  secondary <- if (metric == "RMSE") "MAE" else "RMSE"
  scores <- scores[order(scores[[metric]], scores[[secondary]], scores$model), ]
  rownames(scores) <- NULL
  if (!is.finite(scores[[metric]][1])) {
    stop(paste("All candidates failed.", paste(scores$model, scores$reason,
      sep = ": ", collapse = "; ")), call. = FALSE)
  }
  best <- scores$model[1]
  fit <- fit_forecaster(best, values, period, lags, seed)
  predictions <- forecast_model(fit, best, values, h, lags)
  time_end <- if (stats::is.ts(y)) stats::tsp(y)[2L] else n
  list(best_model = best, metric = metric, score_horizons = score_horizons,
       scores = scores,
       backtest = do.call(rbind, lapply(comparisons, function(x) x$predictions)),
       forecast = data.frame(horizon = seq_len(h), time = time_end + seq_len(h) / period,
                             predicted = predictions),
       fit = fit)
}

whole_number <- function(x, name, minimum) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < minimum || x > .Machine$integer.max || x != floor(x)) {
    stop(paste(name, "must be a single integer >=", minimum), call. = FALSE)
  }
  as.integer(x)
}

lag_features <- function(y, targets, lags) {
  x <- do.call(cbind, lapply(lags, function(lag) y[targets - lag]))
  colnames(x) <- paste0("lag", lags)
  x
}

fit_forecaster <- function(model, y, period, lags, seed) {
  if (model == "ETS") return(forecast::ets(stats::ts(y, frequency = period)))
  if (model == "ARIMA") {
    return(forecast::auto.arima(stats::ts(y, frequency = period), parallel = FALSE))
  }
  targets <- seq.int(max(lags) + 1L, length(y))
  x <- lag_features(y, targets, lags)
  if (model == "Random Forest") {
    return(ranger::ranger(x = as.data.frame(x), y = y[targets],
      num.trees = 200L, num.threads = 1L, seed = seed))
  }
  withr::with_seed(seed, xgboost::xgb.train(
    params = list(objective = "reg:squarederror", max_depth = 3L,
                  eta = 0.1, nthread = 1L),
    data = xgboost::xgb.DMatrix(x, label = y[targets], nthread = 1L),
    nrounds = 50L, verbose = 0L))
}

forecast_model <- function(fit, model, y, h, lags) {
  if (model %in% c("ETS", "ARIMA")) {
    result <- as.numeric(forecast::forecast(fit, h = h)$mean)
  } else {
    # Feed each prediction back as a lag for the next forecast step.
    n <- length(y)
    series <- c(y, numeric(h))
    for (i in n + seq_len(h)) {
      x <- lag_features(series, i, lags)
      series[i] <- if (model == "Random Forest") {
        stats::predict(fit, data = as.data.frame(x), num.threads = 1L)$predictions
      } else {
        stats::predict(fit, xgboost::xgb.DMatrix(x, nthread = 1L))
      }
    }
    result <- series[n + seq_len(h)]
  }
  if (length(result) != h || any(!is.finite(result))) stop("Nonfinite forecast.")
  result
}
