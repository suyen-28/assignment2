#' Fit Kernel Ridge Regression (KRR) model
#'
#' @param X Numeric matrix of predictors (n x p)
#' @param y Numeric response vector of length n
#' @param rho Kernel parameter
#' @param lambda Penalty parameter
#' @return A list of class "krr" containing coefficients, fitted values, etc.
#'
#' @export
krr_fit <- function(X, y, rho = 1, lambda = 1e-4) {
  if (!is.matrix(X)) X <- as.matrix(X)
  n <- nrow(X)
  if (length(y) != n) stop("Length of y must match nrow(X).")

  K <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in 1:n) {
      K[i, j] <- exp(- rho * sum((X[i, ] - X[j, ])^2))
    }
  }

  alpha_hat <- solve(K + diag(lambda, n), y)

  result <- list(
    coefficients = alpha_hat,
    fitted_values = as.vector(K %*% alpha_hat),
    rho = rho,
    lambda = lambda,
    X = X,
    y = y
  )
  class(result) <- "krr"
  return(result)
}

