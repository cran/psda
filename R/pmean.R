#' Polygonal empiric mean
#' 
#' Compute the polygonal empirical mean for polygonal variable.
#' 
#' @param polygons A list of matrices of dimension l x 2, where l represent number of sides polygon.
#' 
#' @return The method returns a vector containing the symbolic polygonal empirical mean in first and second dimension, respectively.
#' @examples 
#' x <- psim(10, 10) #simulate 10 polygons of 10 sides
#' pmean(x)
#' @export
pmean <- function (polygons){
  first_moment <- lapply(polygons, pmean_id)
  first_moment <- unlist(first_moment)
  first_moment <- matrix(first_moment, ncol = 2, byrow = T)
  first_moment <- c(mean(first_moment[, 1]), mean(first_moment[, 2]))
  return(first_moment)
}