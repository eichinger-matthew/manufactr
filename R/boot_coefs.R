#' A function to bootstrap coefficients from a model and dataset.
#'
#'
#' This function combines bootstrap resampling of a data set with an estimated
#' model to generate a sampling distribution for every model coefficient.
#' It returns a list of the bootstrapped coefficients, their means, and their
#' covariance matrix. The user can also specify whether they want to sample
#' parameter vectors from the multivariate normal distribution implied by the
#' covariance matrix.
#'
#' @param mod Model object from which to extract data and coefficients.
#' @param nboots Number of times to bootstrap the original data set, model,
#' and coefficients. Defaults to 500.
#' @param sample_params Logical value indicating whether to sample random draws
#' from the multivariate normal distribution implied by the bootstrapped
#' covariance matrix. Defaults to FALSE.
#' @return A list with the bootstrapped parameters.
#' @export
#' @examples
#' mod <- lm(mpg ~ wt + hp, data = mtcars)
#' a <- boot_coefs(mod = mod)
#' str(a)
#' a$coefs
#' a$means
#' a$cov_mat


# function to bootstrap cofficients directly, rather than all datasets
boot_coefs <- function(mod, nboots = 500, sample_params = FALSE){
  n <- nrow(mod$model)
  nboot <- 1000
  coefs <- matrix(NA, nrow = nboot, ncol = length(coef(mod)))
  for(i in 1:nboot){
    # resample data
    resample <- mod$model[sample(1:n, n, replace = TRUE),]
    # get model call
    mcall <- mod$call
    # estimate model
    a <- lm(mcall, data = resample)
    # get coefs and save to matrix
    coefs[i,] <- coef(a)
  }
  # make list
  coefs <- data.frame(coefs)
  colnames(coefs) <- names(mod$coefficients)
  info <-
    list("coefs" = coefs,
         "means" = apply(coefs, 2, mean),
         "cov_mat" = cov(coefs))
  # if they want resampled parameters
  if(isTRUE(sample_params)){
    # sample from multivariate normal
    b <- MASS::mvrnorm(n = 1000, mu = info$means, Sigma = info$cov_mat)
    info[["sampled_params"]] <- b
  }
  # return list
  return(info)
}


