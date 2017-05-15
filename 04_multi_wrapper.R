## common wrapper for all operations
makeInfoGainMedianReadWrapper <- function(learner) {
  model_prob <- makeLearner(learner, predict.type = "prob")
  model_mread <- makePreprocWrapperMedianReads(model_prob)
  lrn = makeFilterWrapper(model_mread, fw.method = "kruskal.test")
  return(lrn)
}

makeiGmRrPWrapper <- function(learner, fw_abs = 100) {
  model_prob <- makeLearner(learner, predict.type = "prob")
  model_rp <- makePreprocWrapperRandomProject(model_prob)
  model_mread <- makePreprocWrapperMedianReads(model_rp)
  #model_lrn = makeFilterWrapper(model_mread, fw.method = "cforest.importance", fw.abs = fw_abs)
  return(model_mread)
}

makeKTCorrWrapper <- function(learner, v1_feats, verbose = FALSE, keep_v1 = FALSE) {
  model_prob <- makeLearner(learner, predict.type = "prob")
  if (keep_v1) {
    model_ft <- makeFilterWrapper(model_prob, fw.method = "kruskal.test2", fw.mandatory.feat = v1_feats)
  } else {
    model_ft <- makeFilterWrapper(model_prob, fw.method = "kruskal.test2")
  }
  model_corr <- makePreprocWrapperCorr(model_ft, mandatory.feat = v1_feats, verbose = verbose)
  model_mread <- makePreprocWrapperMedianReads(model_corr, mandatory.feat = v1_feats)

  return(model_mread)
}


makeKTCorrWrapper2 <- function(learner, v1_feats, verbose = FALSE) {
  model_prob <- makeLearner(learner, predict.type = "prob")
  model_corr <- makePreprocWrapperCorr(model_prob, verbose = verbose)
  model_fltr <- makeFilterWrapper(model_corr, fw.method = "kruskal.test2", fw.mandatory.feat = v1_feats)
  model_mread <- makePreprocWrapperMedianReads(model_fltr, mandatory.feat = v1_feats)
  return(model_mread)
}


## failed
# makeKTCorrWrapper3 <- function(learner, v1_feats) {
#   model_prob <- makeLearner(learner, predict.type = "prob")
#   model_fltr1 <- makeFilterWrapper(model_prob, fw.method = "kruskal.test")
#   model_corr <- makePreprocWrapperCorr(model_fltr1)
#   model_fltr2 <- makeFilterWrapper(model_corr, fw.method = "kruskal.test", fw.abs = 10000, fw.mandatory.feat = v1_feats)
#   model_mread <- makePreprocWrapperMedianReads(model_fltr2, mandatory.feat = v1_feats)
#   return(model_mread)
# }


makeMRWrapper <- function(learner, v1_feats) {
  model_prob <- makeLearner(learner, predict.type = "prob")
  model_ft <- makeFilterWrapper(model_prob, fw.method = "mrmr")
  model_mread <- makePreprocWrapperMedianReads(model_ft, mandatory.feat = v1_feats)
  return(model_mread)
}

makeTestWrapper <- function(learner, v1_feats) {
  model_prob <- makeLearner(learner, predict.type = "prob")
  model_ft <- makeFilterWrapper(model_prob, fw.method = "kruskal.test", fw.mandatory.feat = v1_feats)

  return(model_ft)
}
