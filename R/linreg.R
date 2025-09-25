#' Fit a Linear Regression Model Using QR Decomposition
#'
#' \code{linreg()} is a custom implementation of multiple linear regression,
#' similar to \code{\link[stats]{lm}}, but built from first principles using
#' QR decomposition for coefficient estimation. It returns an object of class
#' \code{"linreg"} with methods for printing, summarizing, plotting, and
#' predicting.
#'
#' @param formula An object of class \code{\link[stats]{formula}} describing
#'   the model to be fitted.
#' @param data A \code{data.frame} containing the variables in the model.
#'
#' @details
#' This function performs least squares regression using the QR decomposition
#' of the design matrix \eqn{X}. Standard errors, t-values, and p-values are
#' computed explicitly from the variance–covariance matrix of the coefficients.
#'
#' The returned object is of class \code{"linreg"}, for which S3 methods
#' (\code{print}, \code{summary}, \code{plot}, \code{predict}, \code{coef},
#' \code{resid}) can be implemented. These mimic the behavior of corresponding
#' methods for objects of class \code{"lm"}.
#'
#' @return An object of class \code{"linreg"} with the following components:
#' \item{coefficients}{Estimated regression coefficients (numeric vector).}
#' \item{coef.names}{Names of the coefficients (character vector).}
#' \item{fitted}{Fitted values (\eqn{\hat{y}}).}
#' \item{residuals}{Residuals (\eqn{y - \hat{y}}).}
#' \item{df}{Residual degrees of freedom (\eqn{n - p}).}
#' \item{sigma2}{Residual variance estimate.}
#' \item{vcov}{Variance–covariance matrix of the coefficients.}
#' \item{stats}{A \code{data.frame} with estimates, standard errors,
#'   t-values, and p-values for each coefficient.}
#' \item{call}{The matched function call.}
#' \item{formula}{The model formula.}
#' \item{terms}{The terms object used.}
#' \item{xlevels}{Levels of the factors in the model.}
#' \item{qr}{The QR decomposition of the design matrix.}
#'
#' @examples
#' # Fit a regression model
#' fit <- linreg(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
#'
#' # Inspect results
#' print(fit)
#' summary(fit)
#' coef(fit)
#' resid(fit)
#' plot(fit)
#'
#' # Predict on new data
#' new_data <- iris[1:5, ]
#' predict(fit, newdata = new_data)
#'
#' @seealso \code{\link[stats]{lm}} for the base R implementation of linear regression.
#' @export

linreg <- function(formula, data) {
  mf <- model.frame(formula, data)
  y  <- model.response(mf)
  X  <- model.matrix(formula, mf)

  n <- nrow(X); p <- ncol(X)
  qrX <- qr(X)
  # coefficients via QR:
  beta <- qr.coef(qrX, y)

  fitted <- as.vector(X %*% beta)
  resid  <- as.vector(y - fitted)
  df <- n - p
  sigma2 <- sum(resid^2) / df

  # compute vcov via R from QR
  R <- qr.R(qrX)
  Rinv <- backsolve(R, diag(p))                 # R^{-1}
  XtX_inv <- Rinv %*% t(Rinv)                    # (X'X)^{-1}
  vcov_beta <- sigma2 * XtX_inv

  se <- sqrt(diag(vcov_beta))
  tval <- as.vector(beta / se)
  pval <- 2 * pt(-abs(tval), df)

  terms_obj <- terms(formula, data = mf)
  #xlevels <- stats::.getXlevels(terms_obj, mf)
  xlevels <- lapply(mf[sapply(mf, is.factor)], levels)

  out <- list(
    coefficients = as.vector(beta),
    coef.names = colnames(X),
    fitted = fitted,
    residuals = resid,
    df = df,
    sigma2 = sigma2,
    vcov = vcov_beta,
    stats = data.frame(Estimate = as.vector(beta), Std.Error = se, t.value = tval, p.value = pval, row.names = colnames(X)),
    call = match.call(),
    formula = formula,
    terms = terms_obj,
    xlevels = xlevels,
    qr = qrX
  )
  class(out) <- "linreg"
  out
}
