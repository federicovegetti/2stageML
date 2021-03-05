# l1_reg() 

#' Applies reg_extr by group and returns a data frame with coefficients, standard errors, number of observations and fit statistics (for linear models only) for each group.
#' @param formula The formula of the regression model, written as \code{y ~ x_1 + x_2 + x_n}
#' @param data A data frame containing all the variables and the observations in all levels
#' @param group The name of the variable identifying the groups.
#' @param x_fact Indicates which independent variables are to be treated as factor variables. Default is \code{NULL}.
#' @param method Which type of regression to run, currently only \code{"linear"} (cor linear regression) and \code{"logit"} (for logistic regression on binary dependent variables) are allowed. Default is \code{"linear"}.
#' @param min_n Minimum number of observations allowed in order to fit the regression. Default it \code{20}.
#' @param min_ng Minimum number of observations within each level of factor variables. Default it \code{5}.
#' @param focal_var Variable for which estimate and standard errors should be returned. Only 3 options: "(Intercept)" for the level-1 intercept, "(all)" for the entire set of estimates and standard errors (this is the default behavior), or "var_name", where this represents the name of the level-1 variable for which we want to use the estimate as a DV in the second-stage equation.
#' @return A data frame with the coefficients, standard errors, number of observations and adjusted RSquare (for linear models only) for each group.



l1_reg <- function(formula, 
                   data, 
                   group, 
                   x_fact = NULL, 
                   method = "linear",
                   focal_var = "(all)",
                   min_n = 20, 
                   min_ng = 5) {
  
  if(length(x_fact) == 0) {
    
    res_list <- lapply(
      split(data, f = data[, group], drop = T),
      function(x) reg_extr(formula, data = x, group = group, 
                           min_n = min_n, min_ng = min_ng, 
                           method = method, 
                           focal_var = focal_var))
    
  } else {
    
    res_list <- lapply(
      split(data, f = data[, group], drop = T),
      function(x) reg_extr(formula, data = x, group = group, 
                           min_n = min_n, min_ng = min_ng, 
                           method = method, x_fact = x_fact,
                           focal_var = focal_var))
    
  }
  
  allNms <- unique(unlist(lapply(res_list, names)))
  
  result <- data.frame(
    do.call("rbind",
            lapply(res_list,
                   function(x) data.frame(c(x, sapply(setdiff(allNms, names(x)),
                                                      function(y) NA))))),
    row.names = NULL)
  
  if(method == "linear") {
    result <- result[c(setdiff(names(result), c("n", "adj.rsq")), c("n", "adj.rsq"))]
  } else {
    result <- result[c(setdiff(names(result), c("n")), c("n"))]
  }
  
  result
  
}
