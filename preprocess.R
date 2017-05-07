##
## preprocess
##


## removing highly correlated features hclust
# makePreprocWrapperHclust <- function(learner, perc_cutoff = 0.8, mandatory.feat = NULL) {
#   trainfun = function(data, target, args = list(c_cutoff)) {
#     cns = colnames(data)
#     nums = setdiff(cns[sapply(data, is.numeric)], target)
#     x = data[, nums, drop = FALSE]
#
#     stopifnot(!anyNA(x))
#
#     message(paste0("[makePreprocWrapperHopach] the curent dimention of x left:", paste(dim(x), collapse = ",")))
#
#     x = train_red[, 2:20, with = FALSE]
#     d = 1 - cor(x)
#
#     hc = hclust(as.dist(d))
#     cl.hc <- cutree(hc, k = 5)
#
#     #match(cl.hc, 1:5)
#
#     hp = hopach::hopach(t(x[, 1:4]), d="cor")
#
#     features = unique(c(colnames(x)[-highlyCorrelated], mandatory.feat))
#
#     message(paste0("[makePreprocWrapperHopach] the number of features left:", length(features)))
#
#     control = args
#     control$features = features
#     data = data[, setdiff(cns, nums), drop = FALSE]
#
#     stopifnot(nrow(data) == nrow(x))
#     data = cbind(data, as.data.frame(x[, features]))
#
#     stopifnot(target %in% colnames(data))
#     return(list(data = data, control = control))
#   }
#   predictfun = function(data, target, args, control) {
#     cns = colnames(data)
#     nums = setdiff(cns[sapply(data, is.numeric)], target)
#     keeped = control$features
#
#     x = data[, keeped]
#     data = data[, setdiff(cns, nums), drop = FALSE]
#
#     data = cbind(data, as.data.frame(x))
#     return(data)
#   }
#   makePreprocWrapper(
#     learner,
#     train = trainfun,
#     predict = predictfun,
#     par.set = makeParamSet(
#       makeNumericLearnerParam("c_cutoff", lower = 0, upper = 1)
#     ),
#     par.vals = list(c_cutoff = cor_cutoff)
#   )
# }
#
# ## removing highly correlated features hopach
# makePreprocWrapperHopach <- function(learner, perc_cutoff = 0.8, mandatory.feat = NULL) {
#   trainfun = function(data, target, args = list(c_cutoff)) {
#     cns = colnames(data)
#     nums = setdiff(cns[sapply(data, is.numeric)], target)
#     x = data[, nums, drop = FALSE]
#
#     stopifnot(!anyNA(x))
#
#     message(paste0("[makePreprocWrapperHopach] the curent dimention of x left:", paste(dim(x), collapse = ",")))
#
#     hp = hopach::hopach(t(x[, 1:4]), d="cor")
#
#     features = unique(c(colnames(x)[-highlyCorrelated], mandatory.feat))
#
#     message(paste0("[makePreprocWrapperHopach] the number of features left:", length(features)))
#
#     control = args
#     control$features = features
#     data = data[, setdiff(cns, nums), drop = FALSE]
#
#     stopifnot(nrow(data) == nrow(x))
#     data = cbind(data, as.data.frame(x[, features]))
#
#     stopifnot(target %in% colnames(data))
#     return(list(data = data, control = control))
#   }
#   predictfun = function(data, target, args, control) {
#     cns = colnames(data)
#     nums = setdiff(cns[sapply(data, is.numeric)], target)
#     keeped = control$features
#
#     x = data[, keeped]
#     data = data[, setdiff(cns, nums), drop = FALSE]
#
#     data = cbind(data, as.data.frame(x))
#     return(data)
#   }
#   makePreprocWrapper(
#     learner,
#     train = trainfun,
#     predict = predictfun,
#     par.set = makeParamSet(
#       makeNumericLearnerParam("c_cutoff", lower = 0, upper = 1)
#     ),
#     par.vals = list(c_cutoff = cor_cutoff)
#   )
# }




## removing highly correlated features
makePreprocWrapperCorr <- function(learner, cor_cutoff = 0.8, mandatory.feat = NULL, verbose = FALSE) {
  trainfun = function(data, target, args = list(c_cutoff)) {
    cns = colnames(data)
    nums = setdiff(cns[sapply(data, is.numeric)], target)
    x = data[, nums, drop = FALSE]

    stopifnot(!anyNA(x))

    correlationMatrix <- cor(x)

    if (verbose) {
      message(paste0("[makePreprocWrapperCorr] the curent cutoff:", args$c_cutoff))
    }

    # message(paste0("[makePreprocWrapperCorr] the curent dimention of x left:", paste(dim(x), collapse = ",")))

    highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = args$c_cutoff)

    stopifnot(length(highlyCorrelated) < ncol(x))
    features <- unique(c(colnames(x)[setdiff(1:ncol(x), highlyCorrelated)], mandatory.feat))

    if (verbose) {
      message(paste0("[makePreprocWrapperCorr] the number of features left:", length(features)))
    }

    control = args
    control$features = features
    data = data[, setdiff(cns, nums), drop = FALSE]

    stopifnot(nrow(data) == nrow(x))
    data = cbind(data, as.data.frame(x[, features]))

    stopifnot(target %in% colnames(data))
    return(list(data = data, control = control))
  }
  predictfun = function(data, target, args, control) {
    cns = colnames(data)
    nums = setdiff(cns[sapply(data, is.numeric)], target)
    keeped = control$features

    x = data[, keeped]
    data = data[, setdiff(cns, nums), drop = FALSE]

    data = cbind(data, as.data.frame(x))
    return(data)
  }
  makePreprocWrapper(
    learner,
    train = trainfun,
    predict = predictfun,
    par.set = makeParamSet(
      makeNumericLearnerParam("c_cutoff", lower = 0, upper = 1)
    ),
    par.vals = list(c_cutoff = cor_cutoff)
  )
}


## removing those low expressers
makePreprocWrapperMedianReads <- function(learner, m_cutoff = 4L, mandatory.feat = NULL, verbose = FALSE) {
  trainfun = function(data, target, args = list(median_cutoff)) {
    cns = colnames(data)
    nums = setdiff(cns[sapply(data, is.numeric)], target)
    x = data[, nums, drop = FALSE]

    med = lapply(x, function(x) median(x, na.rm = TRUE))
    features = names(med[(med > args$median_cutoff) %in% TRUE])
    features = unique(c(features, mandatory.feat))

    x = x[, features]

    if (verbose) {
      message(paste0("[makePreprocWrapperMedianReads] the number of features left:", length(features)))
      message(paste0("[makePreprocWrapperMedianReads] the current dimension of x:", paste(dim(x), collapse = ",")))
    }

    control = args
    control$features = features
    data = data[, setdiff(cns, nums), drop = FALSE]

    stopifnot(nrow(data) == nrow(x))
    data = cbind(data, as.data.frame(x))

    stopifnot(target %in% cns)
    return(list(data = data, control = control))
  }
  predictfun = function(data, target, args, control) {
    cns = colnames(data)
    nums = setdiff(cns[sapply(data, is.numeric)], target)
    keeped = control$features

    x = data[, keeped]
    data = data[, setdiff(cns, nums), drop = FALSE]

    data = cbind(data, as.data.frame(x))
    return(data)
  }
  makePreprocWrapper(
    learner,
    train = trainfun,
    predict = predictfun,
    par.set = makeParamSet(
      makeNumericLearnerParam("median_cutoff")
    ),
    par.vals = list(median_cutoff = m_cutoff)
  )
}


## using random projection to reduce the dimension
library("RandPro")
makePreprocWrapperRandomProject <- function(learner, eps = 0.4, verbose = FALSE) {
  trainfun = function(data, target, args = list(pp.eps)) {
    cns = colnames(data)
    nums = setdiff(cns[sapply(data, is.numeric)], target)
    x = data[, nums, drop = FALSE]

    row_num <- nrow(x)
    col_num <- ncol(x)

    if (args$pp.eps == 0) {
      gauss_mat <- diag(col_num)
    } else {
      gauss_mat <- form_gauss_matrix(col_num, row_num, TRUE, eps = args$pp.eps)
    }

    if (verbose) {
      message(paste0("[makePreprocWrapperRandomProject] The dimension of gaussian matrix: nrow:", nrow(gauss_mat), ", ncol:", ncol(gauss_mat)))
    }

    new_x <- as.matrix(x) %*% gauss_mat

    control = args
    control$gauss_mat = gauss_mat
    control$rp_feats = nums
    data = data[, setdiff(cns, nums), drop = FALSE]

    stopifnot(nrow(data) == nrow(new_x))
    data = cbind(data, as.data.frame(new_x))

    return(list(data = data, control = control))
  }
  predictfun = function(data, target, args, control) {
    cns = colnames(data)
    nums = control$rp_feats
    gauss_mat = control$gauss_mat

    new_x = as.matrix(data[, nums, drop = FALSE]) %*% gauss_mat
    data = data[, setdiff(cns, nums), drop = FALSE]

    data = cbind(data, as.data.frame(new_x))
    return(data)
  }
  makePreprocWrapper(
    learner,
    train = trainfun,
    predict = predictfun,
    par.set = makeParamSet(
      makeNumericLearnerParam("pp.eps")
    ),
    par.vals = list(pp.eps = eps)
  )
}
