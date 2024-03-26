#' A function to bootstrap a dataframe n times and return a list of the samples.
#'
#'
#' This function lets the user easily bootstrap a dataframe and return a list of
#' the samples. The user can specify the number of observation per bootstrap and
#' the total number of bootstraps.
#'
#' @param df Dataframe to bootstrap.
#' @param obs_per_boot Number of observations to draw for each bootstraped sample.
#' Defaults to the number of rows in the dataframe.
#' @param num_boots Number of bootstrap samples to draw. Defaults to 100. The
#' number of bootstraps should realistically be be at least 1,000, but with
#' large dataframes this can be computationally expensive.
#' @return A list of bootstrapped samples.
#' @export
#' @examples
#' a <- boot_data(df = mtcars)
#' length(a)
#'
#' # specifying observations per bootstrap
#' a <- boot_data(df = mtcars, obs_per_boot = 150)
#' nrow(a[[1]])
#'
#' # specifying number of bootstraps
#' a <- boot_data(df = mtcars, num_boots = 200)
#' length(a)


boot_data <- function(df, obs_per_boot = nrow(df), num_boots = 100) {
  # make list
  ls <- vector("list", num_boots)
  for(i in 1:num_boots){
    ls[[i]] <- df[sample(nrow(df), obs_per_boot, replace = TRUE), ]
  }
  return(ls)
}



