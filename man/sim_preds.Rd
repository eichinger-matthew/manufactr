% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_preds.R
\name{sim_preds}
\alias{sim_preds}
\title{A function to simulate predicted values and confidence intervals from an estimated model.}
\usage{
sim_preds(
  mod,
  means,
  cov_matrix,
  cond_values = list(),
  n = 2000,
  cilo = 0.025,
  cihi = 0.975
)
}
\arguments{
\item{mod}{Model object from which to extract coefficient names.}

\item{means}{Vector of coefficient estimates from the model.}

\item{cov_matrix}{Covariance matrix of model coefficients.}

\item{cond_values}{A list of name-value pairs of the independent variables
at which the user wants to simulate predicted values. Defaults to mean or mode.
and coefficients. Defaults to 500.}

\item{n}{Number of times to resample from the parameter distribution.
Defaults to 2000.}

\item{cilo}{Lower bound of confidence interval to calculate from the simulated
values. Defaults to 0.025.}

\item{cihi}{Upper bound of confidence interval to calculate from the simulated
values. Defaults to 0.975.}
}
\value{
A dataframe with the values of the independent variables specified
by the user, along with the average predicted value and confidence interval.
}
\description{
This function calculates predicted values or predicted probabilities conditioned
on values of the independent variables specified by the user. It takes a model
as input, the point estimates of its parameters, and the covariance matrix
of its parameters. It then uses bootstrap resampling to randomly draw
parameter values from the multivariate normal distribution implied by the
model, and combines these with the conditional values of the independent
variables to produce predicted values. This function is meant
(but does not have to) be used with the boot_coefs() function, which
generates the point estimates and covariance matrix of the model parameters.
}
\examples{
df <- mtcars
df$cyl <- factor(df$cyl)
mod <- lm(mpg ~ wt + hp + cyl, data = df)

# use without having bootstrapped the coefficients.
b <- sim_preds(mod, means = coef(mod), cov_matrix = vcov(mod))

}
