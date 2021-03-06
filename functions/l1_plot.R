#' Function for plotting the level-1 coefficients from a regression model that is run group-by-group
#' @param outcome The name of the L1 model outcome, as a character string.
#' @param predictors A vector of character strings that specifies the L1 predictors.
#' @param focal_var The L1 predictor for which we want to obtain a plot of coefficients. The user can either specify a variable name from the list of predictors given above, or \code{(Intercept)}.
#' @param data The data frame on which the regression models are to be estimated.
#' @param method The specification we want to run. Currently users have a choice between
#' \code{linear} and \code{logit}.
#' @param n_col Number of columns on which to display the panels of coefficients. Default is \code{1}.
#' @param conf_level Level for the confidence interval. Ranges from \code{0} to \code{1}
#' @param ordered Logical value dictating whether or not the coefficients should be arranged from highest to lowest in the plot.

require(ggplot2)

l1_plot <- function(outcome,
					predictors,
					focal_var,
					group,
					data,
					method,
					n_col = 1,
					conf_level = 0.95,
					ordered = TRUE) {

	source("l1_reg.R")

	if(outcome %in% predictors) {
		stop("Outcome variable is also included among predictors.")
	}

	if(!all(union(outcome, predictors) %in% names(data))) {
		stop("At least one variable name does not match the data frame column names.")
	}

	if(!(focal_var %in% predictors)){
		stop("The variable which you want to plot is not included in the list of predictors for the outcome.")
	}

	if(!(group %in% names(data))){
		stop("The variable specified as grouping indicator does not match the data frame column names.")
	}

	# Obtain needed formula for "l1_reg()"
	rhs <- paste(predictors, collapse = " + ")
	formula_mod <- as.formula(paste(outcome, paste(" ~ ", rhs)))
	rm(rhs)

	coef_df <- l1_reg(formula = formula_mod,
	                  data = data,
	                  method = method,
	                  focal_var = focal_var,
	                  group = group)

	coef_df <- coef_df[ ,1:4]
	names(coef_df) <- c("group","b","se","n")

	# Computing additional quantities needed to determine the confidence intervals
	# to be used in the plotting
	alpha = 1 - conf_level
	coef_df$d_free <- coef_df$n - length(predictors) - 1
	coef_df$crit_val <- qt(1 - alpha/2, df = coef_df$d_free)

	# Computing confidence intervals
	coef_df$ci_low <- coef_df$b - coef_df$crit_val*coef_df$se
	coef_df$ci_hi <- coef_df$b + coef_df$crit_val*coef_df$se

	coef_df <- coef_df[ ,c("group","b","ci_low","ci_hi")]

	# Ordering coefficients
  if(ordered) {
    coef_df <- coef_df[order(-coef_df$b), ]
    coef_df$group <- factor(coef_df$group,
                            levels = coef_df$group)
  }
	# Creating facets, if specified
	if(n_col > 1) {
	  if(ordered) {
	    coef_df$facets <- rep(n_col:1,
	                          each = ceiling(dim(coef_df)[1]/n_col),
	                          length.out = dim(coef_df)[1])
	  } else {
	    coef_df$facets <- rep(1:n_col,
	                          each = ceiling(dim(coef_df)[1]/n_col),
	                          length.out = dim(coef_df)[1])
	  }

	  n_facets <- length(unique(coef_df$facets))
	} else {
	  n_facets <- 1
	}

	if(n_facets == 1) {
	  ggplot(coef_df,
	         aes(x = group,
	             y = b)) +
	    geom_point(size = 3) +
	    geom_errorbar(aes(x = group,
	                      ymin = ci_low,
	                      ymax = ci_hi),
	                  size = 1.25,
	                  width = 0.01) +
	    geom_hline(yintercept = 0, size = 1.5, color = "red") +
	    theme_bw() +
	    coord_flip() +
	    labs(x = "Group",
	         y = "Coefficient")
	} else {
	  ggplot(coef_df,
	         aes(x = group,
	             y = b)) +
	    geom_point(size = 3) +
	    geom_errorbar(aes(x = group,
	                      ymin = ci_low,
	                      ymax = ci_hi),
	                  size = 1.25,
	                  width = 0.01) +
	    theme_bw() +
	    geom_hline(yintercept = 0, size = 1.5, color = "red") +
	    facet_wrap(.~facets, ncol = n_col, scales = "free_y") +
	    coord_flip() +
	    labs(x = "Group",
	         y = "Coefficient")
	}
}

# EOF