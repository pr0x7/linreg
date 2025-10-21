#' Ridge Regression using QR Decomposition
#'
#' Performs ridge regression using QR decomposition.
#'
#' @param formula A formula specifying the model (e.g., y ~ x1 + x2)
#' @param data A data.frame containing the variables in the model.
#' @param lambda Regularization parameter (λ ≥ 0).
#' @return An object of class "ridgereg".
#' @details
#' Ridge regression solves (X'X + λI)β = X'y using QR decomposition for stability.
#' @examples
#' model <- ridgereg(mpg ~ wt + hp, data = mtcars, lambda = 1)
#' print(model)
#' @export
ridgereg <- function(formula, data, lambda = 0) {
  if (lambda < 0) stop("lambda must be non-negative")

  mf <- model.frame(formula, data)
  y <- model.response(mf)
  X <- model.matrix(formula, data)

  # Normalize predictors (except intercept)
  X_no_int <- scale(X[, -1, drop = FALSE])
  X <- cbind(Intercept = 1, X_no_int)

  # QR decomposition
  qrX <- qr(X)
  Q <- qr.Q(qrX)
  R <- qr.R(qrX)

  # Ridge coefficients via QR
  I <- diag(ncol(R))
  I[1, 1] <- 0  # do not penalize intercept
  beta_hat <- solve(t(R) %*% R + lambda * I, t(R) %*% t(Q) %*% y)

  y_hat <- X %*% beta_hat
  residuals <- y - y_hat

  obj <- list(
    coefficients = as.vector(beta_hat),
    fitted.values = as.vector(y_hat),
    residuals = as.vector(residuals),
    lambda = lambda,
    formula = formula,
    data = data
  )
  class(obj) <- "ridgereg"
  obj
}
