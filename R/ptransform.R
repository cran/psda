#' Polygon data transformation
#' 
#' It transform labeled data in polygonal datas.
#' 
#' @param data A data frame.
#' @param vertices integer that represent number of vertices of polygon.
#' @return A list of matrices of dimension l x 2 where l represent number of vertices polygon transformed.
#' @examples
#' cat <- as.factor(sample(1:20, 1000, replace = TRUE))
#' cv <- runif(1000) #classical variable
#' cvc <- data.frame(category = cat, cv) 
#' p <- ptransform(cvc, 4)
#' @importFrom stats dist
#' @importFrom stats runif
#' @importFrom stats sd
#' @export
ptransform <- function(data, vertices){
  if(!is.data.frame(data)){
    stop("Insert a dataframe!")
  }
  if(!is.factor(data[, 1])){
    stop("Insert a factor in first column!")
  }
  data_factor <- data[, 1]
  data_factor <- table(data_factor)
  if(sum(data_factor <= 1) != 0){
    stop("Insert data with all factors greater than 1!")
  }
  if(vertices <= 2){
    stop("Insert vertices number greater than 2!")
  }
  if(ncol(data) == 3){
    data <- split(data[, -1], data[,1])
    number_polygons <- length(data)
    center <- t(sapply(data, function(y) apply(y, 2, mean)))
    radius <- t(sapply(data, function(y) apply(y, 2, function(x) 2*sd(x))))
    radius <- apply(radius, 1, function(x) max(abs(x)))
    polygons1 <- list()
    polygons <- matrix(rep(0, vertices*2), ncol = 2)
    l <- 1
    for(i in 1 : number_polygons){
      for(j in 1 : vertices){
        polygons[j, ] <- c(center[i, 1] + radius[i]*cos(2*pi*j/vertices), 
                           center[i, 2] + radius[i]*sin(2*pi*j/vertices))
        polygons1[[l]] <- polygons
      }
      l <- l + 1
    }
  }
  else if(ncol(data) == 2){
    polygons1 <- list()
    polygons <- matrix(rep(0, vertices*2), ncol = 2)
    center <- as.vector(by(data[,2], data[,1], mean))
    radius <- 2*as.vector(by(data[,2], data[,1], sd))
    number_polygons <- length(radius)
    l <- 1
    for(i in 1 : number_polygons){
      for(j in 1 : vertices){
        polygons[j, ] <- c(center[i] + radius[i]*cos(2*pi*j/vertices), 
                           center[i] + radius[i]*sin(2*pi*j/vertices))
        polygons1[[l]] <- polygons
      }
      l <- l + 1
    }
  }
  else{
    stop("Insert a valid dataset!")
  }
  polygons1
}