utils::globalVariables(c("sres", "fitted", "resid"))

#' @export
print <- function(x, ...) {
  UseMethod("print")
}
#' Print method for linreg objects
#' @param x An object of class "linreg"
#' @param ... Additional arguments (not used)
#' @export print.linreg
#' @export
print.linreg <- function(x, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  coeffs <- x$coefficients
  names(coeffs) <- x$coef.names
  print(coeffs)
  cat("\n")
  invisible(x)
}

#' Summary method for linreg objects
#' @param object An object of class "linreg"
#' @param ... Additional arguments (not used)
#' @export
summary.linreg <- function(object, ...) {
  cat("\nCall:\n")
  print(object$call)

  cat("\nResiduals:\n")
  res_summary <- summary(object$residuals)
  print(res_summary)

  cat("\nCoefficients:\n")
  stars <- ifelse(object$stats$p.value < 0.001, "***",
                  ifelse(object$stats$p.value < 0.01, "**",
                         ifelse(object$stats$p.value < 0.05, "*",
                                ifelse(object$stats$p.value < 0.1, ".", " "))))

  coef_table <- cbind(object$stats, " " = stars)
  print(coef_table)

  cat("---\n")
  cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  cat("Residual standard error:", sqrt(object$sigma2), "on", object$df, "degrees of freedom\n")
}

#' Extract coefficients from linreg objects
#' @param object An object of class "linreg"
#' @param ... Additional arguments (not used)
#' @return Named vector of coefficients
#' @export
coef.linreg <- function(object, ...) {
  coeffs <- object$coefficients
  names(coeffs) <- object$coef.names
  return(coeffs)
}



#' Extract residuals from linreg objects (alternative name)
#' @param object An object of class "linreg"
#' @param ... Additional arguments (not used)
#' @return Vector of residuals
#' @export residuals.linreg
#' @export
residuals.linreg <- function(object, ...) {
  return(object$residuals)
}

#' Extract fitted values from linreg objects
#' @param object An object of class "linreg"
#' @param ... Additional arguments (not used)
#' @return Vector of fitted values
#' @export
fitted.linreg <- function(object, ...) {
  return(object$fitted)
}

#' Predict method for linreg objects
#' @param object An object of class "linreg"
#' @param newdata A data frame with new data for prediction
#' @param ... Additional arguments (not used)
#' @return Vector of predicted values
#' @export
#' @importFrom stats delete.response model.frame model.matrix
predict.linreg <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    return(object$fitted)
  }

  Terms <- stats::delete.response(object$terms)
  mf <- stats::model.frame(Terms, newdata, xlev = object$xlevels)
  X_new <- stats::model.matrix(Terms, mf)

  predictions <- as.vector(X_new %*% object$coefficients)
  return(predictions)
}

#' Generic function for pred
#' @param object An object
#' @param ... Additional arguments
#' @export
pred <- function(object, ...) {
  UseMethod("pred")
}

#' Shorthand predict method for linreg objects
#' @param object An object of class "linreg"
#' @param newdata A data frame with new data for prediction
#' @param ... Additional arguments (not used)
#' @return Vector of predicted values
#' @export
pred.linreg <- function(object, newdata = NULL, ...) {
  return(predict.linreg(object, newdata, ...))
}


#' Plot method for linreg objects
#' @param x An object of class "linreg"
#' @param ... Additional arguments (not used)
#' @export plot.linreg
#' @export
#' @import ggplot2
#' @method plot linreg
plot.linreg <- function(x, ...) {
  stopifnot(inherits(x, "linreg"))
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' must be installed to plot linreg objects.")
  }

  # Use the correct component names from your linreg object
  fitted <- x$fitted  # NOT x$fitted.values
  resid  <- x$residuals
  sigma  <- sqrt(x$sigma2)
  std_resid <- resid / sigma

  df1 <- data.frame(fitted = fitted, resid = resid)
  df2 <- data.frame(fitted = fitted, sres = sqrt(abs(std_resid)))

  p1 <- ggplot2::ggplot(df1, ggplot2::aes(fitted, resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_smooth(method = "loess", se = FALSE, formula = y ~ x) +  # Added method
    ggplot2::labs(
      title = "Residuals vs Fitted",
      x = "Fitted values",  # Simplified label
      y = "Residuals"
    ) +
    ggplot2::theme_minimal()

  p2 <- ggplot2::ggplot(df2, ggplot2::aes(fitted, sres)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "loess", se = FALSE, formula = y ~ x) +  # Added method
    ggplot2::labs(
      title = "Scale-Location",
      x = "Fitted values",  # Simplified label
      y = expression(sqrt("|Standardized residuals|"))
    ) +
    ggplot2::theme_minimal()

  print(p1)
  print(p2)
  invisible(list(plot1 = p1, plot2 = p2))  # Better to return the plots
}




#' @export
print.ridgereg <- function(x, ...) {
  cat("Ridge Regression Model (QR Decomposition)\n")
  cat("Formula:", deparse(x$formula), "\n")
  cat("Lambda:", x$lambda, "\n\n")
  cat("Coefficients:\n")
  print(round(x$coefficients, 4))
  invisible(x)
}

#' Coefficients for ridgereg objects
#' @param object A ridgereg object
#' @param ... Additional arguments
#' @export
coef.ridgereg <- function(object, ...) object$coefficients

#' Fitted values for ridgereg objects
#' @param object A ridgereg object
#' @param ... Additional arguments
#' @export
fitted.ridgereg <- function(object, ...) object$fitted.values

#' Residuals for ridgereg objects
#' @param object A ridgereg object
#' @param ... Additional arguments
#' @export
residuals.ridgereg <- function(object, ...) object$residuals

#' Predict method for ridgereg objects
#' @param object A ridgereg object
#' @param newdata New data frame for prediction
#' @param ... Additional arguments
#' @export
predict.ridgereg <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) return(object$fitted.values)

  X_new <- model.matrix(object$formula, newdata)
  # Standardize newdata like training set
  X_train <- model.matrix(object$formula, object$data)
  mu <- colMeans(X_train[, -1, drop = FALSE])
  sd <- apply(X_train[, -1, drop = FALSE], 2, sd)
  X_new[, -1] <- sweep(X_new[, -1, drop = FALSE], 2, mu, "-")
  X_new[, -1] <- sweep(X_new[, -1, drop = FALSE], 2, sd, "/")

  as.vector(X_new %*% object$coefficients)
}

#' Summary method for ridgereg objects
#' @param object A ridgereg object
#' @param ... Additional arguments
#' @export
summary.ridgereg <- function(object, ...) {
  cat("Ridge Regression Summary\n")
  cat("Formula:", deparse(object$formula), "\n")
  cat("Lambda:", object$lambda, "\n\n")
  cat("Coefficients:\n")
  print(round(object$coefficients, 4))
  cat("\nResidual standard error:",
      sqrt(sum(object$residuals^2)/(length(object$residuals)-length(object$coefficients))), "\n")
  cat("Number of observations:", length(object$residuals), "\n")
  invisible(object)
}





