test_that("ridgereg() returns correct object structure", {
  model <- ridgereg(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris, lambda = 1)
  expect_s3_class(model, "ridgereg")
  expect_named(model, c("coefficients", "fitted.values", "residuals", "lambda", "formula", "data", "terms"))
})

test_that("ridge coefficients are close to lm.ridge()", {
  skip_if_not_installed("MASS")

  data(mtcars)
  lambda <- 1

  my_mod <- ridgereg(mpg ~ wt + hp, data = mtcars, lambda = lambda)
  mass_mod <- MASS::lm.ridge(mpg ~ wt + hp, data = mtcars, lambda = lambda)

  actual <- round(coef(my_mod), 2)
  expected <- round(coef(mass_mod), 2)

  # Normalize names to avoid "(Intercept)" vs "" mismatch
  names(actual)[1] <- ""
  names(expected)[1] <- ""

  # Compare coefficients numerically (ignore tiny numeric differences)
  expect_equal(
    actual,
    expected,
    tolerance = 0.05
  )
})


test_that("predict() works on training data and new data", {
  data(mtcars)
  model <- ridgereg(mpg ~ wt + hp, data = mtcars, lambda = 1)

  # Predictions on training data
  preds1 <- predict(model)
  expect_equal(length(preds1), nrow(mtcars))
  expect_true(is.numeric(preds1))

  # Predictions on new data
  newdata <- head(mtcars)
  preds2 <- predict(model, newdata)
  expect_equal(length(preds2), nrow(newdata))
  expect_true(is.numeric(preds2))
})

test_that("fitted() and residuals() behave correctly", {
  data(mtcars)
  model <- ridgereg(mpg ~ wt + hp, data = mtcars, lambda = 1)
  fitted_vals <- fitted(model)
  resid_vals <- residuals(model)

  # Fitted + residual = observed
  expect_equal(round(fitted_vals + resid_vals, 6), round(mtcars$mpg, 6))
  expect_true(all(is.finite(fitted_vals)))
  expect_true(all(is.finite(resid_vals)))
})

test_that("increasing lambda shrinks coefficients", {
  data(mtcars)
  mod_small <- ridgereg(mpg ~ wt + hp, data = mtcars, lambda = 0.1)
  mod_big   <- ridgereg(mpg ~ wt + hp, data = mtcars, lambda = 100)

  expect_true(all(abs(coef(mod_big)[-1]) < abs(coef(mod_small)[-1])))
})

test_that("ridgereg handles invalid lambda", {
  data(mtcars)
  expect_error(ridgereg(mpg ~ wt + hp, data = mtcars, lambda = -1))
})

test_that("coef(), fitted(), residuals() return correct types", {
  data(mtcars)
  model <- ridgereg(mpg ~ wt + hp, data = mtcars, lambda = 1)
  expect_true(is.numeric(coef(model)))
  expect_true(is.numeric(fitted(model)))
  expect_true(is.numeric(residuals(model)))
})


