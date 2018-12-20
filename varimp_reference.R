#Reference
#https://www.ibe.med.uni-muenchen.de/organisation/mitarbeiter/070_drittmittel/janitza/rf_ordinal/tutorial.pdf
library(party)
library(varImp)
varimpRPS <- function (object, mincriterion = 0, conditional = FALSE,
                       threshold = 0.2, nperm = 1, OOB = TRUE, pre1.0_0 = conditional)
{  
  response <- object@responses
  input <- object@data@get("input")
  xnames <- colnames(input)
  inp <- initVariableFrame(input, trafo = NULL)
  y <- object@responses@variables[[1]]
  if(length(response@variables) != 1)
    stop("cannot compute variable importance measure for multivariate response")
  if (conditional || pre1.0_0) {
    if(!all(complete.cases(inp@variables)))
      stop("cannot compute variable importance measure with missing values")
  }
  CLASS <- all(response@is_nominal)
  ORDERED <- all(response@is_ordinal)
  if (CLASS || ORDERED) {
    if(CLASS) warning("this variable importance measure should only be used if the response is ordinal, otherwise results are not meaningful")
    error <- function(x, oob) {
      indicator_mat <- matrix(as.numeric(rep(y, each = length(levels(y)))), nrow = length(levels(y)), byrow = FALSE)
      T_F <- apply(indicator_mat, 2, function(x) as.numeric(x <= 1:length(levels(y))))
      (sum((sapply(x, function(x) cumsum(x)) - T_F)[,oob]^2))/sum(oob) # division by number of oob observations becomes only relevant if bootstrap samples are drawn
    }
  }
  else {
    stop("this variable importance is not applicable to numeric response")
  }
  w <- object@initweights
  if (max(abs(w - 1)) > sqrt(.Machine$double.eps))
    warning(sQuote("varimp"), " with non-unity weights might give misleading results")
  ## list for several permutations
  perror <- matrix(0, nrow = nperm*length(object@ensemble), ncol = length(xnames))
  ## this matrix is initialized with values 0 so that a tree that does not 
  ## contain the current variable adds importance 0 to its average importance
  colnames(perror) <- xnames
  for (b in 1:length(object@ensemble)){
    tree <- object@ensemble[[b]]
    ## if OOB == TRUE use only oob observations, otherwise use all observations in learning sample
    if(OOB){oob <- object@weights[[b]] == 0} else{ oob <- rep(TRUE, length(xnames))}
    p <- .Call("R_predict", tree, inp, mincriterion, -1L, PACKAGE = "party")
    eoob <- error(p, oob)
    ## for all variables (j = 1 ... number of variables) 
    for(j in unique(varIDs(tree))){
      for (per in 1:nperm){
        if (conditional || pre1.0_0) {
          tmp <- inp
          ccl <- create_cond_list(conditional, threshold, xnames[j], input)
          if (is.null(ccl)) {
            perm <- sample(which(oob))
          } else {
            perm <- conditional_perm(ccl, xnames, input, tree, oob)
          }
          tmp@variables[[j]][which(oob)] <- tmp@variables[[j]][perm]
          p <- .Call("R_predict", tree, tmp, mincriterion, -1L,
                     PACKAGE = "party")
        } else {
          p <- .Call("R_predict", tree, inp, mincriterion, as.integer(j),
                     PACKAGE = "party")
        }
        ## run through all rows of perror
        perror[(per+(b-1)*nperm), j] <- (error(p, oob) - eoob)
      } ## end of for (per in 1:nperm)
    } ## end of for(j in unique(varIDs(tree)))
  } ## end of for (b in 1:length(object@ensemble))
  
  perror <- as.data.frame(perror)
  #return(MeanDecreaseAccuracy = perror) ## return the whole matrix (= nperm*ntree values per variable)
  return(MeanDecreaseAccuracy = colMeans(perror)) ## return only averages over permutations and trees
}

environment(varimpRPS) <- environment(varimp) 

varimpMAE <- function (object, mincriterion = 0, conditional = FALSE, scores = NULL,
                       threshold = 0.2, nperm = 1, OOB = TRUE, pre1.0_0 = conditional)
{
  response <- object@responses
  input <- object@data@get("input")
  xnames <- colnames(input)
  inp <- initVariableFrame(input, trafo = NULL)
  y <- object@responses@variables[[1]]
  if(length(response@variables) != 1)
    stop("cannot compute variable importance measure for multivariate response")
  if (conditional || pre1.0_0) {
    if(!all(complete.cases(inp@variables)))
      stop("cannot compute variable importance measure with missing values")
  }
  CLASS <- all(response@is_nominal)
  ORDERED <- all(response@is_ordinal)
  if (ORDERED || CLASS) {
    if(CLASS) warning("this variable importance measure should only be used if the response is ordinal, otherwise results are not meaningful")
    if(is.null(scores)) scores <- 1:length(levels(y))
    error <- function(x, oob) mean(abs(sapply(x, function(z) scores[which.max(z)]) - scores[as.numeric(y)])[oob])
  }else {
    error <- function(x, oob) mean(abs((unlist(x) - y))[oob])
  }
  w <- object@initweights
  if (max(abs(w - 1)) > sqrt(.Machine$double.eps))
    warning(sQuote("varimp"), " with non-unity weights might give misleading results")
  ## list for several permutations
  perror <- matrix(0, nrow = nperm*length(object@ensemble), ncol = length(xnames))
  ## this matrix is initialized with values 0 so that a tree that does not 
  ## contain the current variable adds importance 0 to its average importance
  colnames(perror) <- xnames
  for (b in 1:length(object@ensemble)){
    tree <- object@ensemble[[b]]
    ## if OOB == TRUE use only oob observations, otherwise use all observations in learning sample
    if(OOB){oob <- object@weights[[b]] == 0} else{ oob <- rep(TRUE, length(xnames))}
    p <- .Call("R_predict", tree, inp, mincriterion, -1L, PACKAGE = "party")
    eoob <- error(p, oob)
    ## for all variables (j = 1 ... number of variables) 
    for(j in unique(varIDs(tree))){
      for (per in 1:nperm){
        if (conditional || pre1.0_0) {
          tmp <- inp
          ccl <- create_cond_list(conditional, threshold, xnames[j], input)
          if (is.null(ccl)) {
            perm <- sample(which(oob))
          } else {
            perm <- conditional_perm(ccl, xnames, input, tree, oob)
          }
          tmp@variables[[j]][which(oob)] <- tmp@variables[[j]][perm]
          p <- .Call("R_predict", tree, tmp, mincriterion, -1L,
                     PACKAGE = "party")
        } else {
          p <- .Call("R_predict", tree, inp, mincriterion, as.integer(j),
                     PACKAGE = "party")
        }
        ## run through all rows of perror
        perror[(per+(b-1)*nperm), j] <- (error(p, oob) - eoob)
      } ## end of for (per in 1:nperm)
    } ## end of for(j in unique(varIDs(tree)))
  } ## end of for (b in 1:length(object@ensemble))
  
  perror <- as.data.frame(perror)
  #return(MeanDecreaseAccuracy = perror) ## return the whole matrix (= nperm*ntree values per variable)
  return(MeanDecreaseAccuracy = colMeans(perror)) ## return only averages over permutations and trees
}

environment(varimpMAE) <- environment(varimp) 


# Permutation variable importane measure computed from the mean squared error
# note: MSE-based permutation VIM is not a new measure but in the current party implementation it can only be computed from regression trees

varimpMSE <- function (object, mincriterion = 0, conditional = FALSE, scores = NULL,
                       threshold = 0.2, nperm = 1, OOB = TRUE, pre1.0_0 = conditional)
{  
  response <- object@responses
  input <- object@data@get("input")
  xnames <- colnames(input)
  inp <- initVariableFrame(input, trafo = NULL)
  y <- object@responses@variables[[1]]
  if(length(response@variables) != 1)
    stop("cannot compute variable importance measure for multivariate response")
  if (conditional || pre1.0_0) {
    if(!all(complete.cases(inp@variables)))
      stop("cannot compute variable importance measure with missing values")
  }
  CLASS <- all(response@is_nominal)
  ORDERED <- all(response@is_ordinal)
  if (ORDERED || CLASS) {
    if(CLASS) warning("this variable importance measure should only be used if the response is ordinal, otherwise results are not meaningful")
    if(is.null(scores)) scores <- 1:length(levels(y))
    error <- function(x, oob) mean((sapply(x, function(z) scores[which.max(z)]) - scores[as.numeric(y)])[oob]^2)
  }else{
    error <- function(x, oob) mean((unlist(x) - y)[oob]^2) 
  }
  w <- object@initweights
  if (max(abs(w - 1)) > sqrt(.Machine$double.eps))
    warning(sQuote("varimp"), " with non-unity weights might give misleading results")
  ## list for several permutations
  perror <- matrix(0, nrow = nperm*length(object@ensemble), ncol = length(xnames))
  ## this matrix is initialized with values 0 so that a tree that does not 
  ## contain the current variable adds importance 0 to its average importance
  colnames(perror) <- xnames
  for (b in 1:length(object@ensemble)){
    tree <- object@ensemble[[b]]
    ## if OOB == TRUE use only oob observations, otherwise use all observations in learning sample
    if(OOB){oob <- object@weights[[b]] == 0} else{ oob <- rep(TRUE, length(xnames))}
    p <- .Call("R_predict", tree, inp, mincriterion, -1L, PACKAGE = "party")
    eoob <- error(p, oob)
    ## for all variables (j = 1 ... number of variables) 
    for(j in unique(varIDs(tree))){
      for (per in 1:nperm){
        if (conditional || pre1.0_0) {
          tmp <- inp
          ccl <- create_cond_list(conditional, threshold, xnames[j], input)
          if (is.null(ccl)) {
            perm <- sample(which(oob))
          } else {
            perm <- conditional_perm(ccl, xnames, input, tree, oob)
          }
          tmp@variables[[j]][which(oob)] <- tmp@variables[[j]][perm]
          p <- .Call("R_predict", tree, tmp, mincriterion, -1L,
                     PACKAGE = "party")
        } else {
          p <- .Call("R_predict", tree, inp, mincriterion, as.integer(j),
                     PACKAGE = "party")
        }
        ## run through all rows of perror
        perror[(per+(b-1)*nperm), j] <- (error(p, oob) - eoob)
      } ## end of for (per in 1:nperm)
    } ## end of for(j in unique(varIDs(tree)))
  } ## end of for (b in 1:length(object@ensemble))
  perror <- as.data.frame(perror)
  #return(MeanDecreaseAccuracy = perror) ## return the whole matrix (= nperm*ntree values per variable)
  return(MeanDecreaseAccuracy = colMeans(perror)) ## return only averages over permutations and trees
}

environment(varimpMSE) <- environment(varimp) 


