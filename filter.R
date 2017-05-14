##
## filters
##

# makeFilter(
#   name = "medianread.filter",
#   desc = "Calculating the median reads for each gene",
#   pkg = "",
#   supported.tasks = c("classif", "regr", "surv"),
#   supported.features = c("numerics"),
#   fun = function(task, nselect, decreasing = TRUE, ...) {
#     feats = getTaskFeatureNames(task)
#     data = getTaskData(task)
#     imp = sapply(data[, feats], function(x) median(x, na.rm = TRUE))
#
#     imp = imp[order(imp, decreasing = decreasing)]
#     imp[imp <= nselect]
#   }
# )

makeFilter(
  name = "kruskal.test2",
  desc = "Fast Kruskal Test for binary and multiclass classification tasks",
  pkg = character(0L),
  supported.tasks = "classif",
  supported.features = c("numerics"),
  fun = function(task, nselect, ...) {
    data = getTaskData(task)
    tar.name = getTaskTargetNames(task)
    #stopifnot(is.factor(data[, tar.name]))
    sapply(getTaskFeatureNames(task), function(feat.name) {
      t = kruskal.test(x = data[, feat.name], g = data[, tar.name])
      unname(t$statistic)
    })
  }
)
