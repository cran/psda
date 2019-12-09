#' Plot polygonal symbolic variable
#'
#' @description Prints all overlaid polygons in the display. The polygons obtained through classes.
#' 
#' @param polygon A list of matrices with dimension l x 2 where l represents vertices number of polygon.
#' @param center logical. iF FALSE(the default) the center of polygon is not displayed.
#' @param color A string that describes the color of center.
#' @examples 
#' x <- psim(10, 10) #simulate 10 polygons of 10 sides
#' pplot(x, center = TRUE, color = 'red')
#' @export
pplot <- function(polygon, center = FALSE, color = 'black'){
  polygon <- lapply(polygon, function(x) {colnames(x) <- NULL; x})
  g <- ggplot2::ggplot()
  names(polygon) = 1:length(polygon)
  k <- plyr::ldply(polygon, function(x) data.frame(x))
  g <- ggplot2::ggplot(k, ggplot2::aes(x = k$X1, y = k$X2, group = k$.id)) + 
    ggplot2::geom_polygon(colour = "black", fill = NA)
  
  g <- g + ggplot2::xlab('Dimension 1') + ggplot2::ylab('Dimension 2')
  
  if(center){
    pc <- data.frame(.id = 1 : length(polygon),  t(sapply(polygon, pmean_id)))
    names(pc) <- names(k)
    g <- g + ggplot2::geom_point(data = pc, 
                                 ggplot2::aes(x = pc$X1, y = pc$X2, group = pc$.id), 
                        color = color)
  }
  return(g)
}