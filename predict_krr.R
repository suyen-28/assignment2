#' Predict method for Kernel Ridge Regression
#'
#' @param object A fitted krr model
#' @param newdata New predictor matrix
#'
#' @export
predict.krr <- function(object, newdata, ...) {
  if (!is.matrix(newdata)) newdata <- as.matrix(newdata)

  X_train <- object$X
  rho     <- object$rho
  alpha   <- object$coefficients

  n_train <- nrow(X_train)
  n_new   <- nrow(newdata)

  K_new <- matrix(0, nrow = n_new, ncol = n_train)
  for (i in 1:n_new) {
    for (j in 1:n_train) {
      diff_ij <- newdata[i, ] - X_train[j, ]
      K_new[i, j] <- exp(- rho * sum(diff_ij^2))
    }
  }

  as.vector(K_new %*% alpha)
}
