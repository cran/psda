#' Extract Polygonal Linear Model Fitted Values
#' 
#' @description The function is used to calculate the fitted center and radius or fitted polygos
#' from polygonal linear regression model.
#' @method fitted plr
#' @rdname fitted
#' @param object an object of the class "\emph{plr}".
#' @param ... further arguments special methods could require.
#' @param polygon logical. If \emph{FALSE} the function returns the center and radius 
#' predicted for polygon. If \emph{TRUE} the function returns an object of the class
#' "\emph{Polygonal}" representing the fitted polygons.
#' @param vertices If \emph{polygon} is \emph{TRUE} a number of vertices should be defined.
#' Besides, the number of vertices should be greater than 2 and equal to number of vertices
#' chosen in symbolic polygonal variables.
#' @return ans the fitted values for polygonal linear regression.
#' @examples 
#' yp <- psim(10, 10) #simulate 10 polygons of 10 sides
#' xp1 <- psim(10, 10) #simulate 10 polygons of 10 sides
#' xp2 <- psim(10, 10) #simulate 10 polygons of 10 sides
#' e <- new.env()
#' e$yp <- yp
#' e$xp1 <- xp1
#' e$xp2 <- xp2
#' fit <- plr(yp~xp1+xp2-1, e)
#' fitted(fit) #shows the center and radius fitted from plr
#' fitted(fit, polygon = TRUE, vertices = 10) #Shows the polygon fitted from plr 
#' @method fitted plr
#' @rdname fitted

#' @export
fitted.plr <- function(object, ..., polygon = FALSE, vertices){
  n <- length(object$fitted.values)
  if(n %% 2 != 0){
    stop('The number of the fitted center is different to radius values!')
  }
  nc <- n / 2
  nr <- n / 2
  center_idx <- 1 : nc 
  
  if(!polygon){
    ans <- object$fitted.values
  }
  else{
    if(vertices < 3){
      stop('Insert a valid number of vertices!')
    }
    fitted_center <-  object$fitted.values[center_idx]
    fitted_radius <- object$fitted.values[-center_idx]
    ans <- list(nc)
    for(i in 1 : nc){
      ans[[i]] <- spolygon(fitted_center[i], fitted_radius[i], vertices)
    }
    class(ans) <- 'polygonal'
  }
  ans 
}