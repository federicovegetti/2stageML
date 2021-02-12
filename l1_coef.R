# Applies reg_extr by group, and puts everything into a data frame

l1_coef <- function(formula, data, group, x_fact = NULL, method = "linear", min_n = 20, min_ng = 5) {
  
  if(length(x_fact) == 0) {
    
    res_list <- lapply(
      split(data, f = data[, group], drop = T),
      function(x) reg_extr(formula, data = x, group = group, 
                           method = "linear"))
    
  } else {
    
    res_list <- lapply(
      split(data, f = data[, group], drop = T),
      function(x) reg_extr(formula, data = x, group = group, 
                           method = "linear", x_fact = x_fact))
    
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
