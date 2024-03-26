#' A function to simulate predicted values and confidence intervals from an estimated model.
#'
#'
#' This function calculates predicted values or predicted probabilities conditioned
#' on values of the independent variables specified by the user. It takes a model
#' as input, the point estimates of its parameters, and the covariance matrix
#' of its parameters. It then uses bootstrap resampling to randomly draw
#' parameter values from the multivariate normal distribution implied by the
#' model, and combines these with the conditional values of the independent
#' variables to produce predicted values. This function is meant
#' (but does not have to) be used with the boot_coefs() function, which
#' generates the point estimates and covariance matrix of the model parameters.
#'
#' @param mod Model object from which to extract coefficient names.
#' @param means Vector of coefficient estimates from the model.
#' @param cov_matrix Covariance matrix of model coefficients.
#' @param cond_values A list of name-value pairs of the independent variables
#' at which the user wants to simulate predicted values. Defaults to mean or mode.
#' and coefficients. Defaults to 500.
#' @param n Number of times to resample from the parameter distribution.
#' Defaults to 2000.
#' @param cilo Lower bound of confidence interval to calculate from the simulated
#' values. Defaults to 0.025.
#' @param cihi Upper bound of confidence interval to calculate from the simulated
#' values. Defaults to 0.975.
#' @return A dataframe with the values of the independent variables specified
#' by the user, along with the average predicted value and confidence interval.
#' @export
#' @examples
#' df <- mtcars
#' df$cyl <- factor(df$cyl)
#' mod <- lm(mpg ~ wt + hp + cyl, data = df)
#'
#' # use without having bootstrapped the coefficients.
#' b <- sim_preds(mod, means = coef(mod), cov_matrix = vcov(mod))


sim_preds <- function(mod, means, cov_matrix, cond_values = list(),
                      n = 2000, cilo = 0.025, cihi = 0.975){
  # check whether user supplied conditional values
  if(length(cond_values) == 0){
    # if they did not, use assemble() to get the conditional values
    cond_values <- manufactr::assemble(mod = mod, output = "matrix", factor_coding = "onehot")
    show <- paste("User did not supply conditional values.",
                   "Using the assemble() function to generate them.")
    print(show)
  } else{
    print("User supplied conditional values.")
  }

  # check that dimensions of cond_values and means are conformable
  if(length(means) != ncol(cond_values)){
    stop("The dimensions of the means and the covariance matrix are not conformable")
  } else{
    # get number of rows in cond_values
    num_iters <- nrow(cond_values)
    outs <- list()
    for(i in 1:num_iters){
      # sample from multivariate normal
      params <- mvrnorm(n, means, cov_matrix)
      # sample from model residuals
      error <- rnorm(n, mean(mod$residuals), sd(mod$residuals))

      # matrix multiply to get linear predictor
      yhat <- params %*% matrix(cond_values[i,]) + error

      # sort and get 95 confidence interval
      yhat <- sort(yhat, decreasing = F)
      avg_yhat <- mean(yhat)
      lo <- yhat[cilo * ceiling(n)]
      hi <- yhat[cihi * floor(n)]

      # make dataframe of cond_values and results
      out <- data.frame(t(matrix(cond_values[i,])), avg_yhat, lo, hi)
      colnames(out) <- c(names(coef(mod)), "avg_yhat", "cilo", "cihi")
      outs[[i]] <- out
    }
    # bind outcomes
    fin <- do.call(rbind, outs)
    return(fin)
  }
}

