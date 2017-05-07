# more operations on creating new methods
# registerS3method("makeRLearner", "<awesome_new_learner_class>", makeRLearner.<awesome_new_learner_class>)
# registerS3method("getFeatureImportanceLearner", "<awesome_new_learner_class>",
  # getFeatureImportanceLearner.<awesome_new_learner_class>)

# create xgboost learner for mlr package
makeRLearner.regr.xgboost.latest = function() {
  makeRLearnerRegr(
    cl = "regr.xgboost.latest",
    package = "xgboost",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "eta", default = 0.3, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "max_depth", default = 6L, lower = 1L),
      makeNumericLearnerParam(id = "min_child_weight", default = 1, lower = 0),
      makeNumericLearnerParam(id = "subsample", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "colsample_bytree", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "lambda", default = 0, lower = 0),
      makeNumericLearnerParam(id = "alpha", default = 0, lower = 0),
      makeNumericLearnerParam(id = "base_score", default = 0.5, tunable = FALSE),
      makeIntegerLearnerParam(id = "nthread", lower = 1L, tunable = FALSE),
      makeIntegerLearnerParam(id = "nrounds", default = 100L, lower = 1L),
      makeIntegerLearnerParam(id = "silent", default = 0L, lower = 0L, upper = 1L, tunable = FALSE),
      makeIntegerLearnerParam(id = "verbose", default = 1, lower = 0, upper = 2, tunable = FALSE),
      makeIntegerLearnerParam(id = "print_every_n", default = 1L, lower = 1L, tunable = FALSE, requires = quote(verbose == 1L))
      ),
    par.vals = list(nrounds = 1L, silent = 0L, verbose = 1L),
    properties = c("numerics", "factors", "weights"),
    name = "eXtreme Gradient Boosting",
    short.name = "xgboost",
    note = "All settings are passed directly, rather than through `xgboost`'s `params` argument. `nrounds` has been set to `1` and `verbose` to `0` by default."
  )
}

# create lightGBM learner for mlr package
makeRLearner.regr.lightGBM.latest = function() {
  makeRLearnerRegr(
    cl = "regr.lightGBM.latest",
    package = "lightGBM",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "num_leaves", default = 10L, lower = 2L),
      makeIntegerLearnerParam(id = "num_iteration", default = 6L, lower = 1L),
      makeNumericLearnerParam(id = "learning_rate", default = 0.1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "min_gain_to_split", default = 0, lower = 0),
      makeNumericLearnerParam(id = "feature_fraction", default = 1, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "min_data_in_leaf", default = 10L, lower = 1L),
      makeIntegerLearnerParam(id = "min_sum_hessian_in_leaf", default = 3L, lower = 1L),
      makeNumericLearnerParam(id = "lambda_l1", default = 0.5, lower = 0),
      makeNumericLearnerParam(id = "lambda_l2", default = 0.5, lower = 0),
      makeIntegerLearnerParam(id = "num_threads", lower = 1, tunable = FALSE)
      ),
    par.vals = list(num_threads = 30),
    properties = c("numerics", "factors", "weights"),
    name = "Light Gradient Boosting Machine",
    short.name = "lightGBM",
    note = "All settings are passed directly, rather than through `lightGBM`'s `params` argument."
  )
}


makeRLearner.classif.svm2 = function() {
  makeRLearnerClassif(
    cl = "classif.svm2",
    package = "e1071",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "type", default = "C-classification", values = c("C-classification", "nu-classification")),
      makeNumericLearnerParam(id = "cost",  default = 1, lower = 0, requires = quote(type == "C-classification")),
      makeNumericLearnerParam(id = "nu", default = 0.5, requires = quote(type == "nu-classification")),
      makeNumericVectorLearnerParam("class.weights", len = NA_integer_, lower = 0),
      makeDiscreteLearnerParam(id = "kernel", default = "radial", values = c("linear", "polynomial", "radial", "sigmoid")),
      makeIntegerLearnerParam(id = "degree", default = 3L, lower = 1L, requires = quote(kernel == "polynomial")),
      makeNumericLearnerParam(id = "coef0", default = 0, requires = quote(kernel == "polynomial" || kernel == "sigmoid")),
      makeNumericLearnerParam(id = "gamma", lower = 0, requires = quote(kernel != "linear")),
      makeNumericLearnerParam(id = "cachesize", default = 40L),
      makeNumericLearnerParam(id = "tolerance", default = 0.001, lower = 0),
      makeLogicalLearnerParam(id = "shrinking", default = TRUE),
      makeIntegerLearnerParam(id = "cross", default = 0L, lower = 0L, tunable = FALSE),
      makeLogicalLearnerParam(id = "fitted", default = TRUE, tunable = FALSE),
      makeLogicalVectorLearnerParam(id = "scale", default = TRUE, tunable = TRUE)
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob", "class.weights"),
    class.weights.param = "class.weights",
    name = "Support Vector Machines (libsvm)",
    short.name = "svm",
    callees = "svm"
  )
}

trainLearner.classif.svm2 = function(.learner, .task, .subset, .weights = NULL,  ...) {
  if (sum(getTaskDesc(.task)$n.feat[c("factors", "ordered")]) > 0) {
    f = getTaskFormula(.task)
    e1071::svm(f, data = getTaskData(.task, .subset), probability = .learner$predict.type == "prob", ...)
  } else {
    d = getTaskData(.task, .subset, target.extra = TRUE)
    e1071::svm(d$data, d$target, probability = .learner$predict.type == "prob", ...)
  }
}

predictLearner.classif.svm2 = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type == "response") {
    predict(.model$learner.model, newdata = .newdata, ...)
  } else {
    attr(predict(.model$learner.model, newdata = .newdata, probability = TRUE, ...), "probabilities")
  }
}

registerS3method("makeRLearner", "classif.svm2", makeRLearner.classif.svm2)



makeRLearner.classif.ksvm2 = function() {
  makeRLearnerClassif(
    cl = "classif.ksvm",
    package = "kernlab",
    # FIXME: stringdot pars and check order, scale and offset limits
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "scaled", default = TRUE),
      makeDiscreteLearnerParam(id = "type", default = "C-svc", values = c("C-svc", "nu-svc", "C-bsvc", "spoc-svc", "kbb-svc")),
      makeDiscreteLearnerParam(id = "kernel", default = "rbfdot",
        values = c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot")),
      makeNumericLearnerParam(id = "C",
        lower = 0, default = 1, requires = quote(type %in% c("C-svc", "C-bsvc", "spoc-svc", "kbb-svc"))),
      makeNumericLearnerParam(id = "nu",
        lower = 0, default = 0.2, requires = quote(type == "nu-svc")),
      makeNumericLearnerParam(id = "epsilon", default = 0.1,
        requires = quote(type %in% c("eps-svr", "nu-svr", "eps-bsvm"))),
      makeNumericLearnerParam(id = "sigma",
        lower = 0, requires = quote(kernel %in% c("rbfdot", "anovadot", "besseldot", "laplacedot"))),
      makeIntegerLearnerParam(id = "degree", default = 3L, lower = 1L,
        requires = quote(kernel %in% c("polydot", "anovadot", "besseldot"))),
      makeNumericLearnerParam(id = "scale", default = 1, lower = 0,
        requires = quote(kernel %in% c("polydot", "tanhdot"))),
      makeNumericLearnerParam(id = "offset", default = 1,
        requires = quote(kernel %in% c("polydot", "tanhdot"))),
      makeIntegerLearnerParam(id = "order", default = 1L,
        requires = quote(kernel == "besseldot")),
      makeNumericLearnerParam(id = "tol", default = 0.001, lower = 0),
      makeLogicalLearnerParam(id = "shrinking", default = TRUE),
      makeNumericVectorLearnerParam(id = "class.weights", len = NA_integer_, lower = 0),
      makeLogicalLearnerParam(id = "fit", default = TRUE, tunable = FALSE),
      makeIntegerLearnerParam(id = "cache", default = 40L, lower = 1L)
    ),
    par.vals = list(fit = FALSE),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob", "class.weights"),
    class.weights.param = "class.weights",
    name = "Support Vector Machines",
    short.name = "ksvm",
    note = "Kernel parameters have to be passed directly and not by using the `kpar` list in `ksvm`. Note that `fit` has been set to `FALSE` by default for speed.",
    callees = "ksvm"
  )
}

trainLearner.classif.ksvm2 = function(.learner, .task, .subset, .weights = NULL, degree, offset, scale, sigma, order, length, lambda, normalized,  ...) {
  
  # FIXME: custom kernel. freezes? check mailing list
  # FIXME: unify cla + regr, test all sigma stuff
  
  #     # there's a strange behaviour in r semantics here wgich forces this, see do.call and the comment about substitute
  #     if (!is.null(args$kernel) && is.function(args$kernel) && !is(args$kernel,"kernel")) {
  #       args$kernel = do.call(args$kernel, kpar)
  #     }
  kpar = learnerArgsToControl(list, degree, offset, scale, sigma, order, length, lambda, normalized)
  f = getTaskFormula(.task)
  pm = .learner$predict.type == "prob"
  if (base::length(kpar) > 0L)
    return(kernlab::ksvm(f, data = getTaskData(.task, .subset), kpar = kpar, prob.model = pm, ...))
  else {
    if (sum(getTaskDesc(.task)$n.feat[c("factors", "ordered")]) > 0) {
      return(kernlab::ksvm(f, data = getTaskData(.task, .subset), prob.model = pm, ...))
    } else {
      d = getTaskData(.task, .subset, target.extra = TRUE)
      return(kernlab::ksvm(x = as.matrix(d$data), y = d$target, prob.model = pm, ...))
    }
  }
}

predictLearner.classif.ksvm2 = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob = "probabilities", "response")
  kernlab::predict(.model$learner.model, newdata = .newdata, type = type, ...)
}

registerS3method("makeRLearner", "classif.ksvm2", makeRLearner.classif.ksvm2)
