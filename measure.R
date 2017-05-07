## measures
## Define a function that calculates the misclassification rate
sens98.fun <- function(task, model, pred, feats, extra.args) {
  spec <- measureTNR(pred$data$truth, pred$data$response, pred$task.desc$negative)
  sens <- measureTPR(pred$data$truth, pred$data$response, pred$task.desc$positive)

  tolerance <- 0.005

  return(ifelse(spec < 0.98 - tolerance, 0, sens))
}

## Generate the Measure object
sens98 <- makeMeasure(
  id = "sens98", name = "Sensitivity at fixed Specificity",
  properties = c("classif", "req.pred", "req.truth"),
  minimize = FALSE, best = 1, worst = 0,
  fun = sens98.fun
)
