#============================================================================
  # S3 METHODS - ALL FIXED
  # ============================================================================

#' Print method for linreg objects
#' @param x An object of class "linreg"
#' @param ... Additional arguments (not used)
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

#' Extract residuals from linreg objects
#' @param object An object of class "linreg"
#' @param ... Additional arguments (not used)
#' @return Vector of residuals
#' @export
resid.linreg <- function(object, ...) {
  return(object$residuals)
}

#' Extract residuals from linreg objects (alternative name)
#' @param object An object of class "linreg"
#' @param ... Additional arguments (not used)
#' @return Vector of residuals
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

#' Generic for plot
#' @param x An object of class "linreg"
#' @param ... Additional arguments (not used)
#' @export
plot <- function(x, ...) {
  UseMethod("plot")
}

#' Plot method for linreg objects
#' @param x An object of class "linreg"
#' @param ... Additional arguments (not used)
#' @import ggplot2
#' @importFrom ggplot2 ggplot aes geom_point geom_hline geom_smooth labs theme_bw geom_text .data
#' @export
plot.linreg <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
   stop("ggplot2 is needed for plot method. Please install it.", call. = FALSE)
  }

  fitted_vals <- x$fitted
  residuals_vals <- x$residuals
  standardized_residuals <- residuals_vals / sqrt(x$sigma2)
  sqrt_abs_std_resid <- sqrt(abs(standardized_residuals))

  plot_data <- data.frame(
    fitted = fitted_vals,
    residuals = residuals_vals,
    std_residuals = standardized_residuals,
    sqrt_abs_std_resid = sqrt_abs_std_resid,
    obs_num = seq_along(residuals_vals)
  )

  outliers <- which(abs(standardized_residuals) > 2)

  # Plot 1: Residuals vs Fitted
  p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$fitted, y = .data$residuals)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::geom_smooth(se = FALSE, color = "red", method = "loess", formula = y ~ x) +
    ggplot2::labs(
      title = "Residuals vs Fitted",
      subtitle = deparse(x$call),
      x = "Fitted values",
      y = "Residuals"
    ) +
    ggplot2::theme_bw()

  if (length(outliers) > 0) {
    p1 <- p1 + ggplot2::geom_text(
      data = plot_data[outliers, ],
      ggplot2::aes(label = .data$obs_num, x = .data$fitted, y = .data$residuals),
      hjust = -0.2, vjust = -0.2
    )
  }

  # Plot 2: Scale-Location
  p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$fitted, y = .data$sqrt_abs_std_resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = FALSE, color = "red", method = "loess", formula = y ~ x) +
    ggplot2::labs(
      title = "Scale-Location",
      subtitle = deparse(x$call),
      x = "Fitted values",
      y = expression(sqrt("|Standardized residuals|"))
    ) +
    ggplot2::theme_bw()

  if (length(outliers) > 0) {
    p2 <- p2 + ggplot2::geom_text(
      data = plot_data[outliers, ],
      ggplot2::aes(label = .data$obs_num, x = .data$fitted, y = .data$sqrt_abs_std_resid),
      hjust = -0.2, vjust = -0.2
    )
  }

  print(p1)
  print(p2)
}


methods(plot)



