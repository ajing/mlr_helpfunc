# need to set seed
# set.seed(getOption("mlr.debug.seed"))

require("purrr")

run_outer_splits <- function(lnr, one_task, outer_split, inner_rdes, p.set, ctrl, measure = sens98, rds_dir = ".", pname = "") {
  train_idx <- outer_split$train.inds
  test_idx <- outer_split$test.inds
  stopifnot(length(train_idx) == length(test_idx))
  EachSplit <- function(i) {
    c_t_idx <- train_idx[[i]]
    c_e_idx <- test_idx[[i]]
    c_task <- subsetTask(one_task, subset = c_t_idx)
    ptune <- tuneParams(lnr, task = c_task, resampling = inner_rdes, par.set = p.set, control = ctrl, measures = measure)
    lnr_set = setHyperPars(lnr, par.vals = ptune$x)
    f_mod = train(lnr_set, c_task)

    c_test <- subsetTask(one_task, c_e_idx)
    pred_val <- predict(f_mod, task = c_test, type = "response")

    # add another for probability prediction
    #prob <- predict(f_mod, task = c_test, type = "probability")

    saveRDS(list(model = f_mod, tune_obj = ptune, pred_outer = pred_res), file = file.path(rds_dir, paste0("MObj_", pname, "Split", i, , ".RDS")))

    # add probability
    return(data.table(idx = c_e_idx, iter = i, score = pred_val))
  }
  #pred_list <- EachSplit(1)
  pred_list <- parallelMap(EachSplit, seq_along(train_idx))

  saveRDS(pred_list, file = file.path(rds_dir, paste0("Rout_", pname, ".RDS")))

  return(rbindlist(pred_list))
}

run_multi_single_learner <- function(lrs, tr_tsk, cv_rdes, ctrl, measure, te_dat = NULL) {
  model_list = list()
  pred_list = list()
  for (lr in lrs) {
    lnr = lr[["learner"]]
    p.set = lr[["par.set"]]
    # run on all
    ptune = tuneParams(lnr, task = tr_tsk, resampling = cv_rdes, par.set = p.set, control = ctrl, measures = measure)
    lnr_set = setHyperPars(lnr, par.vals = ptune$x)
    f_mod = train(lnr_set, c_task)
    model_list[[lr[["name"]]]] = f_mod
    if (!is.null(te_dat)) {
      pred_res = predict(f_mod, newdata = te_dat)
      pred_list[[lr[["name"]]]] = pred_res
    }
  }
  return(mod_list = model_list, pred_list = pred_list)
}

run_multi_learner_ens <- function(lrs, one_task, outer_split, inner_rdes, ctrl, measure = rmse, rds_dir = ".", pname = "") {
  multi_res <- list()
  for (lr in lrs) {
    lnr = lr[["learner"]]
    p.set = lr[["par.set"]]
    #multi_res[[lr$name]] <- safely(run_outer_splits)(lnr, one_task, outer_split, inner_rdes, p.set, ctrl, measure = rmse, rds_dir = ".", pname = pname)
    multi_res[[lr$name]] <- run_outer_splits(lnr, one_task, outer_split, inner_rdes, p.set, ctrl, measure = measure, rds_dir = rds_dir, pname = pname)
  }
  saveRDS(multi_res, file = file.path(rds_dir, paste0("Lres_", pname, ".RDS")))
}

get_cv_idx <- function(task, fold, reps, rds_dir = ".", overwrite = FALSE) {
  repeat_name = paste0("Fold", fold, "Rep", reps)

  if (file.exists(file.path(rds_dir, paste0(repeat_name, ".RDS"))) & !overwrite) {
    return(readRDS(file.path(rds_dir, paste0(repeat_name, ".RDS"))))
  } else {
    # ctrl = makeTuneControlGrid()
    rdesc = makeResampleDesc("RepCV", fold = fold, reps = reps, predict = "both")

    # create outer CV instance
    outer_cv = makeResampleInstance(rdesc, task = task)

    saveRDS(outer_cv, file.path(rds_dir, paste0(repeat_name, ".RDS")))

    return(outer_cv)
  }
}
