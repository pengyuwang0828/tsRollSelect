library(tsRollSelect)

t <- seq_len(48)
y <- ts(20 + t / 5 + rep(c(0, 2, 5, 1), 12) + sin(t), frequency = 4)
result <- select_forecaster(y, h = 2, initial = 30, n_origins = 3)
pred <- result$backtest
stopifnot(nrow(result$scores) == 4L, all(result$scores$status == "ok"),
          nrow(pred) == 24L, all(pred$target - pred$origin == pred$horizon),
          all(table(pred$model, pred$origin) == 2L),
          result$best_model == result$scores$model[1L],
          nrow(result$forecast) == 2L, all(is.finite(result$forecast$predicted)))

# The last observation is never part of a backtest training window.
changed <- y
changed[length(changed)] <- changed[length(changed)] + 1
changed_result <- select_forecaster(changed, h = 2, initial = 30, n_origins = 3,
                                      score_horizons = 2)
stopifnot(isTRUE(all.equal(pred$predicted, changed_result$backtest$predicted)),
          any(pred$actual != changed_result$backtest$actual),
          inherits(try(select_forecaster(c(NA_real_, 1:20)), silent = TRUE), "try-error"))

step_two <- subset(changed_result$backtest, horizon == 2)
rmse <- tapply((step_two$actual - step_two$predicted)^2, step_two$model,
               function(e) sqrt(mean(e)))
scores <- changed_result$scores
stopifnot(isTRUE(all.equal(as.numeric(rmse[scores$model]), scores$RMSE)))
