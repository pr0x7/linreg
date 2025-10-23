#' Ridge Regression using QR Decomposition
#'
#' Performs ridge regression using QR decomposition with centering and scaling
#' to match the behavior of MASS::lm.ridge().
#'
#' @param formula A formula specifying the model (e.g., y ~ x1 + x2)
#' @param data A data.frame containing the variables in the model.
#' @param lambda Regularization parameter (λ ≥ 0).
#' @return An object of class "ridgereg".
#' @details
#' Ridge regression solves (X'X + λI)β = X'y using QR decomposition for numerical stability.
#' Predictors are standardized and the intercept is adjusted back to the
#' original scale to make results comparable with MASS::lm.ridge().
#' @examples
#' model <- ridgereg(mpg ~ wt + hp, data = mtcars, lambda = 1)
#' print(model)
#' @export
ridgereg <- function(formula, data, lambda = 0) {
  if (lambda < 0) stop("lambda must be non-negative")

  # Prepare model frame and matrices
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  X <- model.matrix(formula, data)

  # Extract predictors (no intercept)
  X_no_int <- X[, -1, drop = FALSE]

  # Center and scale predictors and response (like MASS::lm.ridge)
  x_means <- colMeans(X_no_int)
  x_sds <- apply(X_no_int, 2, sd)
  Xs <- scale(X_no_int, center = x_means, scale = x_sds)
  y_mean <- mean(y)
  ys <- y - y_mean

  # Add intercept column
  Xs <- cbind(Intercept = 1, Xs)

  # QR decomposition
  qrX <- qr(Xs)
  Q <- qr.Q(qrX)
  R <- qr.R(qrX)

  # Ridge coefficients via QR
  I <- diag(ncol(R))
  I[1, 1] <- 0  # do not penalize intercept
  beta_hat <- solve(t(R) %*% R + lambda * I, t(R) %*% t(Q) %*% ys)

  # Adjust coefficients back to original scale
  beta_hat[-1] <- beta_hat[-1] / x_sds
  intercept <- y_mean - sum(beta_hat[-1] * x_means)
  beta_hat[1] <- intercept

  # Convert to named numeric vector
  beta_hat <- as.numeric(beta_hat)
  names(beta_hat) <- colnames(X)

  # Compute fitted values & residuals on original scale
  y_hat <- as.vector(X %*% beta_hat)
  residuals <- y - y_hat

  # Remove names for test consistency
  names(y_hat) <- NULL
  names(residuals) <- NULL

  # Build model object
  obj <- list(
    coefficients = beta_hat,
    fitted.values = y_hat,
    residuals = residuals,
    lambda = lambda,
    formula = formula,
    data = data,
    terms = terms(formula, data = data)
  )
  class(obj) <- "ridgereg"
  obj
}





