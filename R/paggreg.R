#' Polygonal data aggregation
#' 
#' @description The function obtains symbolic data from classical data through the center and radius representation.
#' @param data A data frame with the first column of type factor.
#' @return paggreg returns an objects of class "paggregated".
#' 
#' @details The class "aggregated" is composed by two data sets from center and range represetation. 
#' The first and second data set represent the center and radius, respectively.
#' 
#' @examples 
#' cat <- as.factor(sample(1:20, 1000, replace = TRUE))
#' cv <- runif(1000) #classical variable
#' cvc <- data.frame(category = cat, cv) 
#' p <- paggreg(cvc)
#' @importFrom stats aggregate.data.frame
#' @exportClass paggregated
#' @export 
paggreg <- function(data){
  if(!is.data.frame(data)){
    stop("Insert a data.frame!")
  }
  if(!is.factor(data[, 1])){
    stop("Insert a factor in first column!")
  }
  data_factor <- data[, 1]
  data_factor <- table(data_factor)
  if(sum(data_factor <= 1) != 0){
    stop("Insert data with all factors greater than 1!")
  }

  center <- aggregate.data.frame(data[, -1], list(data[, 1]), mean)
  radius <- aggregate.data.frame(data[, -1], list(data[, 1]), function(x) 2 * sd(x))
  
  pdata <- vector('list', 2)
  
  pdata[[1]] <- center[, -1]
  pdata[[2]] <- radius[, -1]
  names(pdata) <- c('center', 'radius')
  class(pdata) <- 'paggregated'
  pdata
  
}