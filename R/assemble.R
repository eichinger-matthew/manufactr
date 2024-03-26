#' A dataframe and matrix assembly function.
#'
#' This function allows you to generate a dataframe or matrix with specific
#' values for the independent variables of a model. Defaults to mode for factor
#' variables and mean for numeric variables. You can specify values for
#' specific variables using a list format. You can also specify whether you
#' want the object returned in a dataframe or matrix format, as well as
#' whether you want the factor variables to be one-hot encoded or not.
#'
#' @param mod Model object to extract data from.
#' @param output Desired output format. Defaults to "dataframe".
#' @param factor_coding Desired format for categorical variables. Defaults
#' to "mode" but can be changed to "onehot" for one-hot encoding.
#' @param cond_values List of name-value pairs for what values the independent
#' variables take. Defaults to empty list, which means the function will
#' use the mode for categorical variables and the mean for numeric variables.
#' @return A dataframe or matrix with the specified values of the independent
#' variables.
#' @export
#' @examples
#' mod <- lm(mpg ~ hp + cyl, data = mtcars)
#' assemble(mod)
#' assemble(mod, output = "matrix")
#' assemble(mod, factor_coding = "onehot", cond_values = list("hp" = seq(100, 200, 10), "cyl" = 8))



# function to generate dataframe call for prediction
assemble <- function(mod, output = "dataframe",
                     factor_coding = "mode", cond_values = list()){
  # make empty list to store values
  newvals <- list()
  if(names(coef(mod))[1] == "(Intercept)"){
    newvals[[names(coef(mod))[1]]] <- 1
  } else{
    # do nothing
  }
  # if condition for format of factor variables
  if(factor_coding == "mode"){
    # loop over independent variables in design matrix
    for(i in 1:(ncol(mod$model)-1)){
      # get variable name
      vname <- names(mod$model)[i+1]
      # check if user wants to supply variable with specific value
      if(names(mod$model[i+1]) %in% names(cond_values)){
        # if yes, set value to what user wants
        newvals[[vname]] <- cond_values[[vname]]
      } else{
        # if not, check if variable is a factor, character, or integer
        if(is.factor(mod$model[,i+1]) | is.character(mod$model[,i+1]) |
           is.integer(mod$model[,i+1])){
          # if yes, get factor value associated with mode
          mode <- names(sort(table(mod$model[i+1]), decreasing = T))[1]
          # assign factor value to list name
          newvals[[vname]] <- mode
        } else {
          # if not a factor, calculate mean
          avg <- mean(mod$model[,i+1])
          # assign mean to list name
          newvals[[vname]] <- avg
        }
      }
    }
  }
  # repeat process above, but when one-hot encoding variables
  else if(factor_coding == "onehot"){
    for(i in 1:(ncol(mod$model)-1)){
      # check if variable is a factor, character, or integer
      if(is.factor(mod$model[,i+1]) | is.character(mod$model[,i+1]) |
         is.integer(mod$model[,i+1])){
        # if yes, then add list entry for each level of factor aside from ref
        for(j in levels(mod$model[,i+1])[-1]){
          newvals[[j]] <- 0
        }
        # then check if user supplied value for factor variable
        if(names(mod$model[i+1]) %in% names(cond_values)){
          # if yes, get the value they set
          vals <- cond_values[[names(mod$model[i+1])]]
          # if value they gave was reference
          if(vals == levels(mod$model[,i+1])[1] |
             vals == noquote(levels(mod$model[,i+1])[1])){
            # pass and do nothing
          } else{
            # if users supplied different value than reference
            # set their supplied value to 1
            newvals[[vals]] <- 1
          }
          }else{
            # if user did not supply anything
            # find mode of variable
            mode <- names(sort(table(mod$model[,i+1]), decreasing = T))[1]
            # check if mode is reference
            if(mode == levels(mod$model[,i+1])[1]){
              # do nothing
            } else{
              # if mode is not reference, set mode to 1
              newvals[[mode]] <- 1
            }
          }
        } else { # if variable is not a factor
          # check if user supplied values
          if(names(mod$model[i+1]) %in% names(cond_values)){
            # if yes, assign supplied values
            vals <- cond_values[[names(mod$model[i+1])]]
            newvals[[names(mod$model[i+1])]] <- vals
          } else{ # if they did not supply values
            # calculate mean
            avg <- mean(mod$model[,i+1])
            # set value in newvals
            newvals[[names(mod$model)[i+1]]] <- avg
          }
        }
    }
  }
  # return output in desired format
  if(output == "dataframe"){
    return(data.frame(newvals))
    }else{
      return(as.matrix(data.frame(newvals)))
    }
}
