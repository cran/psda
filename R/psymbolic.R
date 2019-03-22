#' Polygonal Symbolic Data
#' 
#' @description The function obtain a symbolic polygonal variables from data of
#' class 'paggregated', i.e aggregated data. For this, the researcher need to select
#' the number of vertices.
#' @param pdata an object of the class 'paggregated' that represents the representation 
#' of symbolic polygonal data.
#' @param vertices the number of vertices for the polygon.
#' @details psymbolic converts data represented by center and radius representation in 
#' symbolic polygonal data. It is importat that the researcher considers a positive number 
#' for radius. Besides, the variable vertices should be greater than 2 for the number of vertices. 
#' 
#' When the object of class 'paggregated' is composed by a vector for center and one vector for
#' radius a simple symbolic variable is obtained.
#' 
#' @return psdata is an object of class 'polygonal-variables', i.e. an environment, where for each object in the environment is a list with
#' the polygons(matrix with dimention l times 2, where l represents the number of vertices).
#' 
#' @examples 
#' ## Obtaining a simple symbolic polygonal variable
#' cat1 <- as.factor(sample(1:20, 1000, replace = TRUE))
#' cv1 <- runif(1000) #classical variable
#' cvc1 <- data.frame(category = cat1, cv1) 
#' pol1 <- paggreg(cvc1)
#' out <- psymbolic(pol1, 6) #Hexagon
#' out$X1
#' 
#' ## Obtaining three (or more) symbolic polygonal variables
#' cat2 <- as.factor(sample(1:20, 1000, replace = TRUE))
#' cv2 <- matrix(runif(3000), ncol = 3) #classical variable
#' cvc2 <- data.frame(category = cat2, cv2) 
#' pol2 <- paggreg(cvc2)
#' out2 <- psymbolic(pol2, 8) #Octagon
#' out2$X1
#' out2$X2
#' out2$X3 
#' @exportClass polygonal-variables
#' @export

psymbolic <- function(pdata, vertices){
  if(class(pdata) != 'paggregated'){
    stop('Insert an object of the class paggregated')
  }
  
  if(vertices <= 2){
    stop("Insert the number of vertices greater than 2!")
  }
  
  if(is.matrix(pdata$center) | is.data.frame(pdata$center)){
    m <- nrow(pdata$center)
    p <- ncol(pdata$center)
    
    initial <- vector('list', m)
    psdata <- new.env()
    variables <- paste('X', 1 : p, sep = '')
    for(j in 1 : p){
      for(i in 1 : m){
        initial[[i]] <- spolygon(pdata$center[i, j], pdata$radius[i, j], vertices)
      }
      psdata[[variables[j]]] <- initial  
    }
  }
  else if(is.vector(pdata$center)){
    m <- length(pdata$center)
    
    initial <- vector('list', m)
    psdata <- new.env()
    for(i in 1 : m){
      initial[[i]] <- spolygon(pdata$center[i], pdata$radius[i], vertices)
    }
    psdata[['X1']] <- initial
  }
  else{
    stop('Insert a matrix, data.frame or vector for center and radius!')
  }
  class(psdata) <- 'polygonal-variables'
  psdata
}

