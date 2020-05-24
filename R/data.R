#' Women National Basketball Association 2014 (WNBA 2014).
#'
#' @description The data set contains information about the season 2014.
#'     The data can be seen in 'Polygonal data analysis: A new framework in symbolic data analysis' paper.
#'
#' @format A data.frame with 4022 rows and 6 variables:
#' \describe{
#'   \item{player_id}{Identification of player.}
#'   \item{team_pts}{Number of points made by team.}
#'   \item{opp_pts}{Number of points made by opponent.}
#'   \item{minutes}{Minutes played.}
#'   \item{fgatt}{Field goal attempts.}
#'   \item{efficiency}{Efficiency.}
#' }
#' @source \url{https://www.sciencedirect.com/science/article/pii/S0950705118304052}
"wnba2014"

#' Airfares data (longair)
#'
#' @description Longair data contains about quarterly average airfare and average weekly passengers
#' for 4177 markets in 2001 of the U.S. Department of Transportation.
#' The data can be seen in 'Polygonal data analysis: A new framework in symbolic data analysis' paper.
#'
#' @format A data.frame with 1000 rows and 11 variables:
#' \describe{
#'   \item{city1}{City of boarding.}
#'   \item{cit2}{City of landing.}
#'   \item{average_fare}{Average fare.}
#'   \item{distance}{Distance between city of boarding and landing.}
#'   \item{average_weekly_passengers}{Average weekly passengers.}
#'   \item{market_leading_airline}{Market leading airline.}
#'   \item{market_share}{Market share.}
#'   \item{avarege_return_fare}{Average return fare}
#'   \item{low_price_airline}{Lower price airline.}
#'   \item{market_share2}{Second market share}
#'   \item{price}{Price of travel.}
#' }
#' @source \url{https://www.sciencedirect.com/science/article/pii/S0950705118304052}
"longair"

#' Basic Education Assessment System 2017 (SAEB 2017)
#' @description The dataset describes information about the Brazilian Basic Education Assessment System (SAEB) 
#' and infrastructure of the schools in 2017.  
#' 
#' @format A data.frame with 4037 observations (rows) and 13 variables, each row represent a 
#' county. One column indicates the county identification, the first six are the center 
#' of the polygons, and the six last are the radius of polygons. In datails:
#' \describe{
#' \item{county}{Identification of the county that participate of the SAEB.}
#' \item{proficiency_lp_center}{Leverage of Portuguese language proficiency score.}
#' \item{proficiency_mt_center}{Leverage of the Mathematics.} 
#' \item{classroom_center}{Leverage number of classroom.}
#' \item{classroom_used_center}{Leverage number of classroom used.}
#' \item{employess}{Leverage number of employees of the schools.}
#' \item{proficiency_lp_radius}{Dispersion of the Portuguese language proficiency score.}
#' \item{proficiency_mt_radius}{Dispersion of the Mathematics proficiency score.}
#' \item{classroom_radius}{Classrooms dispersion.}
#' \item{classroom_used_radius}{Classrooms used dispersion.}
#' \item{computers_radius}{Dispertion of the computer numbers.}
#' \item{employees_radius}{Dispersion of the employess numbers.}
#' }
"saeb2017"