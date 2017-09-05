#' Polygonal internam second moment
#' @description Caltulate polygonal second moment for polygonal data.
#' @param polygon a matrix that represents a polygonal variable.
#' @return The internal variance.
#' @examples 
#' x <- psim(1, 3) #simulate a polygon of 3 sides
#' psmi(x[[1]])
#' @export
psmi <- function(polygon) {
  if(nrow(polygon) < 3){
    stop("Insert a valid polygon!")
  }
  a <- polygon[, 1]
  b <- polygon[, 2]
  sumX <- sum1X <- sumY <- sum1Y <- area1 <- area2 <- 0
  for (i in 1:length(a)) {
    if (i < length(a)) {
      sumX <- sumX + (a[i]^2 + a[i] * a[i + 1] + a[i + 
                                                     1]^2) * (a[i] * b[i + 1] - a[i + 1] * b[i])
      sumY <- sumY + (b[i]^2 + b[i] * b[i + 1] + b[i + 
                                                     1]^2) * (a[i] * b[i + 1] - a[i + 1] * b[i])
      area1 <- area1 + 0.5 * (a[i] * b[i + 1] - a[i + 
                                                    1] * b[i])
    }
    else {
      sum1X <- sum1X + (a[i]^2 + a[i] * a[1] + a[1]^2) * 
        (a[i] * b[1] - a[1] * b[i])
      sum1Y <- sum1Y + (b[i]^2 + b[i] * b[1] + b[1]^2) * 
        (a[i] * b[1] - a[1] * b[i])
      area2 <- area2 + 0.5 * (a[i] * b[1] - a[1] * 
                                b[i])
    }
  }
  area <- area1 + area2
  return(abs(c(sumX + sum1X, sumY + sum1Y)/(12 * area)))
}