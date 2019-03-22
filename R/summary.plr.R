#' Summarizing Polygonal Linear Regression
#' @description \code{summary} method for class \code{plr}.
#' @param object an object of the class \code{plr}, usually, a result of a call to \code{\link{plr}}.
#' @param digits a non-null value for \code{digits} specifies the minimum number of significant 
#' digits to be printed in values.
#' @param ... further arguments passed to or from other methods.
#' @return residuals calculated as the response variable minus the fitted values.
#' @return adj.r.squared \eqn{R^2} measure \emph{adjusted} penalizing for higher p.
#' @return sigma the square root of the estimated variance of the random error.
#' @return call the matched call.
#' @return df degrees of freedom.
#' @return aliased named logical vector showing if the original coefficients are aliased.
#' @return terms the \code{\link[stats]{terms}}.
#' @return coefficients a p x 4 matrix with columns for the estimated coefficient, 
#' its standard error, t-statistic and corresponding (two-sided) p-value. 
#' @return fstatistics (for models including non-intercept terms) a 3-vector with the value
#'  the F-statistic with its numerator and denominator degrees of freedom.
#' @return r.squared \eqn{R^2}, the fraction of variance explained by the model.
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
  
  rdf <- z$df.residual
  r <- z$residuals
  n <- length(r)
  
  ans <- new.env()
  ans$call <- z$call
  ans$aliased <- is.na(coef(z))
  ans$residuals <- r
  ans$df <- z$df.residual
  
  if (p == 0) {
    ans$coefficients <- matrix(NA, 0L, 4L)
    dimnames(ans$coefficients) <- list(NULL, c("Estimate", 
                                               "Std. Error", "t value", "Pr(>|t|)"))
    ans$df <- c(0L, n, length(ans$aliased))
    ans$sigma <- sqrt(resvar)
    ans$r.squared <- ans$adj.r.squared <- 0
    return(ans)
  }
  if (is.null(z$terms)) 
    stop("invalid 'plr' object:  no 'terms' component")
  if (!inherits(z, "plr")) 
    warning("calling summary.plr(<fake-plr-object>) ...")
  
  if (is.na(z$df.residual) || n - p != z$df.residual) 
    warning("residual degrees of freedom in z suggest this is not an \"plr\" fit")
  
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
  tval <- z$coefficients / se
  
  ans$coefficients <- cbind(Estimate = z$coefficients, `Std. Error` = se, 
                            `t value` = tval, `Pr(>|t|)` = 2 * pt(abs(tval), rdf, 
                                                                  lower.tail = FALSE))
  ans$sigma <- sqrt(resvar)
  ans$terms <- z$terms
  
  if (p != attr(z$terms, "intercept")) {
    df.int <- if (attr(z$terms, "intercept")) 
      1L
    else 0L
    ans$r.squared <- mss/(mss + rss)
    ans$adj.r.squared <- 1 - (1 - ans$r.squared) * ((n - 
                                                       df.int)/rdf)
    ans$fstatistic <- c(value = (mss/(p - df.int))/resvar, 
                        numdf = p - df.int, dendf = rdf)
  }
  else ans$r.squared <- ans$adj.r.squared <- 0
  class(ans) <- "summary.plr"
  ans
}