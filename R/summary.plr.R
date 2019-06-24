#' Summarizing Polygonal Linear Regression
#' @description \code{summary} method for class \code{plr}.
#' @param object an object of the class \code{plr}, usually, a result of a call to \code{\link{plr}}.
#' @param digits a non-null value for \code{digits} specifies the minimum number of significant 
#' digits to be printed in values.
#' @param ... further arguments passed to or from other methods.
#' @return residuals calculated as the response variable minus the fitted values.
#' @return sigma the given by square root of the estimated variance of the random error 
#' \deqn{\sigma^2 = \frac{\sum{i = 1}^{n} (y_i - \hat{y}_i)^2}{n - p - 1}} where \emph{p} is 
#' two times the number of independent variables.
#' @return call the matched call.
#' @return aliased named logical vector showing if the original coefficients are aliased.
#' @return terms the \code{\link[stats]{terms}}.
#' @return coefficients a p x 4 matrix with columns for the estimated coefficient, 
#' its standard error, z-statistic and corresponding (two-sided) p-value. 
#' @examples 
#' yp <- psim(50, 10) #simulate 50 polygons of 10 sides
#' xp1 <- psim(50, 10) #simulate 50 polygons of 10 sides
#' xp2 <- psim(50, 10) #simulate 50 polygons of 10 sides
#' e <- new.env()
#' e$yp <- yp
#' e$xp1 <- xp1
#' e$xp2 <- xp2
#' fit <- plr(yp~xp1 + xp2, data = e)
#' s <- summary(fit) 
#' @import stats
#' @method summary plr
#' @exportClass summary.plr
#' @export
summary.plr <- function (object, digits = max(3L, getOption("digits") - 3L), ...) {
  z <- object
  ans <- z
  
  n <- nrow(z$model)
  p <- z$rank
  
  rdf <- n - p - 1
  r <- z$residuals
  n <- length(r)

  ans <- new.env()
  ans$call <- z$call
  ans$aliased <- is.na(coef(z))
  ans$residuals <- r
  
  rss <- sum(r^2)
  resvar <- rss/rdf
  ans$sigma <- resvar
  
  if (p == 0) {
    ans$coefficients <- matrix(NA, 0L, 4L)
    dimnames(ans$coefficients) <- list(NULL, c("Estimate", 
                                               "Std. Error", "z value", "Pr(>|z|)"))
    ans$sigma <- sqrt(resvar)
    return(ans)
  }
  if (is.null(z$terms)) 
    stop("invalid 'plr' object:  no 'terms' component")
  if (!inherits(z, "plr")) 
    warning("calling summary.plr(<fake-plr-object>) ...")
  
  f <- z$fitted.values
  
  mss <- if (attr(z$terms, "intercept")) 
    sum((f - mean(f))^2)
  else sum(f^2)
  rss <- sum(r^2)
  resvar <- rss/rdf
  
  if (is.finite(resvar) && resvar < (mean(f)^2 + var(f)) * 
      1e-30) 
    warning("essentially perfect fit: summary may be unreliable")
  
  se <- sqrt(diag(solve(crossprod(z$model))) * resvar)

  n <- length(z$residuals)
  model <- as.matrix(z$model)
  zval <- z$coefficients / se
  
  ans$coefficients <- cbind(Estimate = z$coefficients, `Std. Error` = se, 
                            `z value` = zval, `Pr(>|z|)` = 2 * pt(abs(zval), rdf, 
                                                                  lower.tail = FALSE))

  ans$terms <- z$terms
  class(ans) <- "summary.plr"
  ans
}