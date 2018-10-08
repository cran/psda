#' Polygonal empiric mean
#' 
#' Compute the polygonal empirical mean for polygonal variable.
#' 
#' @param polygons A list of matrices of dimension l x 2, where l represent number of sides polygon.
#' 
#' @return The method returns a vector containing the symbolic polygonal empirical mean in first and second dimension, respectively.
#' @examples 
#' x <- psim(10, 3) #simulate 10 polygons of 3 sides
#' pmean(x)
#' @export
pmean <- function (polygons){
  if(length(polygons) < 1){
    stop("Insert a valid number of polygons!")
  }
  first.moment <- lapply(polygons, pmean_id)
  first.moment <- unlist(first.moment)
  first.moment <- matrix(first.moment, ncol = 2, byrow = T)
  first.moment <- c(mean(first.moment[, 1]), mean(first.moment[, 2]))
  return(first.moment)
}