fluxing = function (mat, biomasses = NULL, losses, efficiencies, bioms.prefs = TRUE, 
          bioms.losses = TRUE, ef.level = "prey", method = "straight_flux") # simple addition of the 'method' term
{
  if (!is.numeric(mat)) {
    stop("'mat' must be numeric")
  }
  if (dim(mat)[1] != dim(mat)[2]) {
    stop("mat should be a square matrix")
  }
  if (!is.null(biomasses)) {
    if (!is.vector(biomasses)) {
      stop("biomasses should be a vector")
    }
    else {
      if (length(biomasses) != dim(mat)[1]) {
        stop("length of biomasses vector should equal to dimensions of mat")
      }
    }
    if (!is.numeric(biomasses)) {
      stop("'biomasses' must be numeric")
    }
    else if (any(biomasses < 0)) {
      stop("'biomasses' must be all >=0")
    }
  }
  else if (bioms.prefs) {
    stop("bioms.prefs set to TRUE but no biomasses provided")
  }
  if (!is.numeric(losses)) {
    stop("'losses' should be numeric")
  }
  else if (any(losses < 0)) {
    stop("'losses' contain negative value(s)")
  }
  if (!is.numeric(efficiencies)) {
    stop("'efficiencies' must be numeric")
  }
  if (ef.level == "pred") {
    colsums = colSums(mat)
    if (sum(is.na(efficiencies[colsums == 0])) > 0) {
      efficiencies[colsums == 0] = 1
    }
  }
  if (ef.level == "prey") {
    rowsums = rowSums(mat)
    if (sum(is.na(efficiencies[rowsums == 0])) > 0) {
      efficiencies[rowsums == 0] = 1
    }
  }
  if (ef.level == "link.specific") {
    if (sum(efficiencies[mat == 0]) > 0) {
      warning("Efficiencies of some non existing links are not 0")
    }
  }
  if (!(ef.level %in% c("prey", "pred", "link.specific"))) {
    stop("ef.level should be set to 'pred', 'prey' or 'link.specific'")
  }
  if (ef.level == "prey" && is.matrix(efficiencies)) {
    warning("'ef.level' is set to 'prey' and expect a vector of efficiencies but get a matrix instead.\n ef.level was then set to 'link.specific'")
    ef.level = "link.specific"
  }
  if (any(efficiencies < 0) || any(efficiencies > 1)) {
    stop("'efficiencies' must all be in interval [0,1]")
  }
  if (is.vector(efficiencies)) {
    if (ef.level == "link.specific") {
      stop("'efficiencies' should be a matrix not a vector when efficiencies are link specific")
    }
    if (length(efficiencies) != dim(mat)[1]) {
      stop("'efficiencies' vector length sould be equal to number of species (dimension of mat)")
    }
  }
  else if (dim(efficiencies != dim(mat))) {
    stop("'efficiencies' matrix dimension different from 'mat'")
  }
  column.sum = colSums(mat)
  if (bioms.prefs) {
    mat[, column.sum > 0] = apply(as.matrix(mat[, column.sum > 
                                                  0]), 2, function(vec) vec * biomasses/sum(vec * 
                                                                                              biomasses))
  }
  else {
    mat[, column.sum > 0] = sweep(as.matrix(mat[, column.sum > 
                                                  0]), 2, column.sum[column.sum > 0], "/")
  }
  if (!is.vector(losses)) {
    losses = rowSums(losses)
  }
  if (bioms.losses == T) {
    losses = losses * biomasses
  }
  if (ef.level == "pred") {
    F = solve(diag(efficiencies) - mat) %*% losses
  }
  if (ef.level == "prey" & method == "tbp") {
    vec.in = as.vector(t(mat) %*% efficiencies)
    vec.1p = rep(0, dim(mat)[1])
    vec.1p[colSums(mat) == 0] = 1
    F = solve(diag(vec.in + vec.1p)) %*% losses # this is the fix for using calculated secondary production measurements rather than estimating energy flux through metabolic equations.
  } else if (ef.level == "prey" & method == "straight_flux") {
    vec.in = as.vector(t(mat) %*% efficiencies)
    vec.1p = rep(0, dim(mat)[1])
    vec.1p[colSums(mat) == 0] = 1
    F = solve(diag(vec.in + vec.1p) - mat) %*% losses
  }
  if (ef.level == "link.specific") {
    U = mat * efficiencies
    vec.one = rep(1, dim(efficiencies)[1])
    vec.1p = rep(0, dim(mat)[1])
    vec.1p[colSums(mat) == 0] = 1
    vec.in = as.vector(t(U) %*% vec.one + vec.1p)
    F = solve(diag(vec.in) - mat) %*% losses
  }
  if (any(F < 0)) {
    stop("model chosen is unable to determine fluxes accoringly to data")
  }
  flux.mat = sweep(mat, 2, F, "*")
  return(flux.mat)
}
