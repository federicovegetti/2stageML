# Extract coefficients from regression models (linear and logit) and put them into a single row 
# Some basic debug done

reg_extr <- function(formula, data, x_fact = NULL, method = "linear", group = NULL, min_n = 20, min_ng = 5){
  
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
  
  if(length(ivs) == 0) {
    out <- data.frame(
      b = cf[, 1],
      se = cf[, 2],
      row.names = NULL,
      stringsAsFactors = F
    )
  } else {
    out <- data.frame(
      cf[, 1:2],
      row.names = NULL,
      stringsAsFactors = F
    )
    names(out) <- c("b", "se")
  }
  
  out$term <- rownames(cf)
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
