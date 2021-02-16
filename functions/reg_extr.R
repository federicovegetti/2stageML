# reg_extr() Extracts coefficients from regression models and returns a single row data frame

#' @param formula The formula of the regression model, written as \code{y ~ x_1 + x_2 + x_n}
#' @param data A data frame containing all the variables and the observations in all levels
#' @param x_fact Indicates which independent variables are to be treated as factor variables. Default is \code{NULL}.
#' @param method Which type of regression to run, currently only \code{"linear"} (cor linear regression) and \code{"logit"} (for logistic regression on binary dependent variables) are allowed. Default is \code{"linear"}.
#' @param group The name of the variable identifying the groups. Only relevant when the function is called inside of \code{l1_reg}. Default is \cote{NULL}.
#' @param min_n Minimum number of observations allowed in order to fit the regression. Default it \code{20}.
#' @param min_ng Minimum number of observations within each level of factor variables. Default it \code{5}.
#' @param focal_var Variable for which estimate and standard errors should be returned.
#' @return A 1-row data frame with the coefficients, standard errors, number of observations and adjusted RSquare (for linear models only).


reg_extr <- function(formula,
                     data,
                     x_fact = NULL,
                     method = "linear",
                     focal_var = "(all)",
                     group = NULL,
                     min_n = 20,
                     min_ng = 5){

  if (method %in% c("linear", "logit") == F) {
    stop("'method' can be either 'linear' or 'logit'")
  }

  df <- tibble::as_tibble(data)
  dv <- all.vars(formula[[2]])
  ivs <- all.vars(formula[[3]])

  if(length(ivs) == 0) {

    if(length(x_fact) != 0) {
      stop("you can't specify 'x_fact' if 'formula' is 'y ~ 1'")
    }

    if (length(group) == 0) {
      df <- df[, dv]
    } else {
      df <- df[, c(dv, group)]
    }
    df <- df[complete.cases(df), ]

  } else {

    if (length(group) == 0) {
      df <- df[, c(dv, ivs)]
    } else {
      df <- df[, c(dv, ivs, group)]
    }
    df <- df[complete.cases(df), ]
  }

  df[, x_fact] <- lapply(df[, x_fact], factor)

  if(nrow(df) < min_n | any(unlist(sapply(df[, x_fact], function(x) table(x) < min_ng)))) {
    # out <- data.frame(
    #   b = NA,
    #   se = NA,
    #   row.names = NULL,
    #   stringsAsFactors = F
    # )
    stop("fuuuck")
  }
  formula <- paste0(dv, " ~ ", paste(ivs, collapse = " + "))
  if (method == "linear") {
    mod <- lm(formula, data = df)
  } else {
    mod <- glm(formula, data = df,
               family = binomial(link = "logit"))
  }

  cf <- summary(mod)$coefficients
  # Set subsetting procedure here with nested "ifelse()" call
  retRange <- NA
  if (focal_var=="(all)") {
    retRange <- 1:(length(ivs)+1)
  } else {
    retRange <- ifelse(focal_var=="(Intercept)",
                       1,
                       ifelse(focal_var %in% ivs,
                              which(rownames(cf)==focal_var),
                              stop("Argument focal_var accepts only 3 possible values. Please check the spelling of your variable name.")))
  }

  if(length(ivs) == 0) {
    out <- data.frame(
      b = cf[, 1],
      se = cf[, 2],
      row.names = NULL,
      stringsAsFactors = F
    )
  } else {
    out <- data.frame(
      matrix(cf[retRange, 1:2], ncol = 2),
      row.names = NULL,
      stringsAsFactors = F
    )
    names(out) <- c("b", "se")
  }

  out$term <- rownames(cf)[retRange]
  for(v in x_fact) {
    out$term <- gsub(v, paste0(v, "_"), out$term)
  }
  out$term <- gsub("\\(Intercept\\)", "intercept", out$term)

  if (length(group) == 0) {
    out$group <- 1
  } else {
    # out$group <- as.character(unique(df[, group]))
    out <- cbind(unique(df[, group]), out)
  }

  out <- reshape(
    out,
    direction = "wide", idvar = group, timevar = "term", sep = "_"
  )
  out$n <- nobs(mod)

  if (method == "linear") {
    out$adj.rsq <- summary(mod)$adj.r.squared
  }

  out

}
