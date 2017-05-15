#' @export
makeRLearner.regr.xgboost_mod = function() {
  # for xgboost_0.4-3 mod
  makeRLearnerRegr(
    cl = "regr.xgboost_mod",
    package = "xgboost",
    par.set = makeParamSet(
      # we pass all of what goes in 'params' directly to ... of xgboost
      #makeUntypedLearnerParam(id = "params", default = list()),
      makeDiscreteLearnerParam(id = "booster", default = "gbtree", values = c("gbtree", "gblinear", "dart")),
      makeIntegerLearnerParam(id = "silent", default = 0L, tunable = FALSE),
      makeNumericLearnerParam(id = "eta", default = 0.3, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "gamma", default = 0, lower = 0),
      makeIntegerLearnerParam(id = "max_depth", default = 6L, lower = 1L),
      makeNumericLearnerParam(id = "min_child_weight", default = 1, lower = 0),
      makeNumericLearnerParam(id = "subsample", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "colsample_bytree", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "colsample_bylevel", default = 1, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "num_parallel_tree", default = 1L, lower = 1L),
      makeNumericLearnerParam(id = "lambda", default = 0, lower = 0),
      makeNumericLearnerParam(id = "lambda_bias", default = 0, lower = 0),
      makeNumericLearnerParam(id = "alpha", default = 0, lower = 0),
      makeUntypedLearnerParam(id = "objective", default = "reg:linear", tunable = FALSE),
      makeUntypedLearnerParam(id = "eval_metric", default = "rmse", tunable = FALSE),
      makeNumericLearnerParam(id = "base_score", default = 0.5, tunable = FALSE),

      makeNumericLearnerParam(id = "missing", default = NULL, tunable = FALSE, when = "both",
        special.vals = list(NA, NA_real_, NULL)),
      makeIntegerVectorLearnerParam(id = "monotone_constraints", default = 0, lower = -1, upper = 1),
      makeIntegerLearnerParam(id = "nthread", lower = 1L, tunable = FALSE),
      makeIntegerLearnerParam(id = "nrounds", default = 2000L, lower = 2000L, tunable = FALSE),
      # FIXME nrounds seems to have no default in xgboost(), if it has 1, par.vals is redundant
      makeUntypedLearnerParam(id = "feval", default = NULL, tunable = FALSE),
      makeIntegerLearnerParam(id = "verbose", default = 1L, lower = 0L, upper = 2L, tunable = FALSE),
      makeIntegerLearnerParam(id = "print_every_n", default = 1L, lower = 1L, tunable = FALSE,
        requires = quote(verbose == 1L)),
      makeIntegerLearnerParam(id = "early.stop.round", default = 20, lower = 1L, tunable = FALSE),
      makeLogicalLearnerParam(id = "maximize", default = NULL, special.vals = list(NULL), tunable = FALSE),
      makeDiscreteLearnerParam(id = "normalize_type", default = "tree", values = c("tree", "forest"), requires = quote(booster == "dart")),
      makeNumericLearnerParam(id = "rate_drop", default = 0, lower = 0, upper = 1, requires = quote(booster == "dart")),
      makeNumericLearnerParam(id = "skip_drop", default = 0, lower = 0, upper = 1, requires = quote(booster == "dart"))
    ),
    par.vals = list(verbose = 1L),
    properties = c("numerics", "weights", "featimp", "missings"),
    name = "eXtreme Gradient Boosting",
    short.name = "xgboost",
    note = "All settings are passed directly, rather than through `xgboost`'s `params` argument. `nrounds` has been set to `1` and `verbose` to `0` by default.")
}

one_sd_rule = function(metric_val, sd_val) {
  # here I assume the larger value of metric, the better performance
  # and the lower index the simpler the model
  max_m_idx = which.max(metric_val)
  one_sd_val = metric_val[max_m_idx] - sd_val[max_m_idx]
  idx_found = min(which(metric_val > one_sd_val))
  return(max(1, idx_found))
}

#' @export
trainLearner.regr.xgboost_mod = function(.learner, .task, .subset, .weights = NULL,  ...) {

  sink(type = c("output", "message"))

  parlist = list(...)

  parlist$label = getTaskData(.task, .subset, target.extra = TRUE)$target
  parlist$data = data.matrix(getTaskData(.task, .subset, target.extra = TRUE)$data)

  if (is.null(parlist$objective))
    parlist$objective = "reg:linear"

  if (!is.null(.weights)) {
    parlist$data = xgboost::xgb.DMatrix(data = parlist$data, label = parlist$label, weight = .weights, missing = NaN)
  } else {
    parlist$data = xgboost::xgb.DMatrix(data = parlist$data, label = parlist$label, missing = NaN)
  }


  # Using xgb.cv to determine nrounds
  parlist$nfold = 10
  parlist$metrics = parlist$eval_metric
  parlist$eval_metric = NULL

  message("[xgboost_mod train] begin to run xgb.cv.")
  cv = do.call(xgboost::xgb.cv, parlist)

  print(cv)

 # cv_eval = cv$evaluation_log
 # cv_mean = cv_eval[[paste("train", parlist$eval_metric, "mean", sep = "_")]]
 # cv_sd = cv_eval[[paste("train", parlist$eval_metric, "std", sep = "_")]]
 # parlist$nrounds = one_sd_rule(cv_mean, cv_sd)
  parlist$nrounds = nrow(cv$evaluation_log)

  parlist$nfold = NULL
  parlist$eval_metric = parlist$metrics
  parlist$metrics = NULL
  parlist$missing = NaN

  message("[xgboost_mod train] begin to run xgb.train.")
  do.call(xgboost::xgboost, parlist)
}

#' @export
predictLearner.regr.xgboost_mod = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  predict(m, newdata = data.matrix(.newdata), ...)
}

#' @export
getFeatureImportanceLearner.regr.xgboost_mod = function(.learner, .model, ...) {
  mlr::getFeatureImportanceLearner.regr.xgboost(.learner, .model, ...)
}

registerS3method("makeRLearner", "regr.xgboost_mod", makeRLearner.regr.xgboost_mod)
registerS3method("trainLearner", "regr.xgboost_mod", trainLearner.regr.xgboost_mod)
registerS3method("predictLearner", "regr.xgboost_mod", predictLearner.regr.xgboost_mod)
registerS3method("getFeatureImportanceLearner", "regr.xgboost_mod", getFeatureImportanceLearner.regr.xgboost_mod)
