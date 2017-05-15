# need to set seed
# set.seed(getOption("mlr.debug.seed"))

require("purr")

run_outer_splits <- function(lnr, one_task, outer_split, inner_rdes, p.set, ctrl, measure = sens98, weights = NULL, rds_dir = ".", pname = "") {
  train_idx <- outer_split$train.inds
  test_idx <- outer_split$test.inds
  stopifnot(length(train_idx) == length(test_idx))
  EachSplit <- function(i) {
    c_t_idx <- train_idx[[i]]
    c_e_idx <- test_idx[[i]]
    if (is.null(weights)) {
      c_weight <- NULL
    } else {
      c_weight <- weights[c_t_idx]
    }
    c_task <- subsetTask(one_task, subset = c_t_idx)
    ptune <- tuneParams(lnr, task = c_task, resampling = inner_rdes, par.set = p.set, control = ctrl, measures = measure, weights = weights)
    lnr_set = setHyperPars(lnr, par.vals = ptune$x)
    f_mod = train(lnr_set, c_task)

    c_test <- subsetTask(one_task, c_e_idx)
    pred_val <- predict(f_mod, task = c_test, type = "response")

    # add another for probability prediction
    #prob <- predict(f_mod, task = c_test, type = "probability")

    saveRDS(list(model = f_mod, tune_obj = ptune, pred_outer = pred_res), file = file.path(rds_dir, paste0("MObj_", fname, "Split", i)))

    # add probability
    return(data.table(idx = c_e_idx, iter = i, score = pred_val))
  }
  #pred_list <- EachSplit(1)
  pred_list <- parallelMap(safely(EachSplit), seq_along(train_idx))

  saveRDS(pred_list, file = file.path(rds_dir, paste0("Rout_", fname)))

  return(rbindlist(pred_list))
}


run_multi_learner <- function(lrs, one_task, outer_split, inner_rdes, ctrl, measure = rmse, rds_dir = ".", pname = "") {
  multi_res <- list()
  for (lr in lrs) {
    lnr = lr[["learner"]]
    p.set = lr[["par.set"]]
    multi_res[[lr$name]] <- safely(run_outer_splits)(lnr, one_task, outer_split, inner_rdes, p.set, ctrl, measure = rmse, rds_dir = ".", pname = "")
  }
  saveRDS(multi_res, file = file.path(rds_dir, paste0("Lres_", fname)))
}

get_cv_idx <- function(task, fold, reps, rds_dir = ".", overwrite = FALSE) {
  if (file.exist(file.path(rds_dir, repeat_name)) & !overwrite) {
    return(readRDS(file.path(rds_dir, repeat_name)))
  } else {
    # ctrl = makeTuneControlGrid()
    rdesc = makeResampleDesc("RepCV", fold = fold, reps = reps, predict = "both", stratify = TRUE)

    # create outer CV instance
    outer_cv = makeResampleInstance(rdesc, task = task)

    saveRDS(outer_cv, file.path(rds_dir, repeat_name)))
  }
}
