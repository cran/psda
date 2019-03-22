#' Plot polygonal symbolic variable
#'
#' @description Prints all overlaid polygons in the display. The polygons obtained through classes.
#' 
#' @param polygon A list of matrices with dimension l x 2 where l represents vertices number of polygon.
#' @examples 
#' x <- psim(10, 10) #simulate 10 polygons of 10 sides
#' pplot(x)
#' @export
pplot <- function(polygon){
  polygon <- lapply(polygon, function(x) {colnames(x) <- NULL; x})
  g = ggplot2::ggplot()
  names(polygon) = 1:length(polygon)
  k <- plyr::ldply(polygon, function(x) data.frame(x))
  g <- ggplot2::ggplot(k, ggplot2::aes(x = k$X1, y = k$X2, group = k$.id)) + ggplot2::geom_polygon(colour = "black", fill = NA)
  return(g)
}