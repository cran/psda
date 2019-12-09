## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(psda)

## ----wnba----------------------------------------------------------------
library(psda)
library(ggplot2)
data(wnba2014)
dta <- wnba2014


## ----aggregation---------------------------------------------------------
dta$player_id <- factor(dta$player_id)
head(dta)

## ----representation------------------------------------------------------
center_radius <- paggreg(dta)
head(center_radius$center, 6)
head(center_radius$radius, 6)

## ----polygons------------------------------------------------------------
v <- 5 
polygonal_variables <- psymbolic(center_radius, v)
head(polygonal_variables$team_pts, 3)

## ----descriptivel--------------------------------------------------------
## symbolic polygonal mean
pmean(polygonal_variables$team_pts)
pmean(polygonal_variables$opp_pts)

## symbolic polygonal variance
pvar(polygonal_variables$team_pts)
pvar(polygonal_variables$opp_pts)

## symbolic polygonal covariance
pcov(polygonal_variables$team_pts)
pcov(polygonal_variables$opp_pts)

## symbolic polygonal correlation
pcorr(polygonal_variables$team_pts) 
pcorr(polygonal_variables$opp_pts) 

## ----scatter-------------------------------------------------------------
pplot(polygonal_variables$team_pts) + labs(x = 'Dimension 1', y = 'Dimension 2') +
theme_bw()

## ----modeling------------------------------------------------------------
fit <- plr(team_pts ~ fgatt + minutes + efficiency + opp_pts, data = polygonal_variables)

## ----summary-------------------------------------------------------------
s <- summary(fit)
s

## ----residuals-----------------------------------------------------------
plot(fit$residuals, ylab = 'Residuals')
hist(fit$residuals, xlab = 'Residuals', prob = T, main = '')

## ----fitting-------------------------------------------------------------
fitted_polygons <- fitted(fit, polygon = T, vertices = v)
head(fitted_polygons, 3)

pplot(fitted_polygons) + labs(x = 'Dimension 1', y = 'Dimension 2') +
theme_bw()

## ----rmsea---------------------------------------------------------------
rmsea(fitted_polygons, polygonal_variables$team_pts)

