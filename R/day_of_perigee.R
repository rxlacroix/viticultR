#' Compute day of perigee of a year
#'
#' This function returns day of perigee of a year
#' @param year The year in format (2019)
#' @return The day of perigee
#' @export
#' @examples
#' day_of_perigee(2019)

day_of_perigee <- function(year) {

  ian <- year


  rOmega <- 2 * pi / 365.256363004

  rLongitPerig <- -1.374953 + .000300051 * (ian - 1900)
  rDoyEqui <- 80.08 + .2422 * (ian - 1900) - ((ian - 1901) / 4)

  rExentr <- .016751 - .00000042 * (ian - 1900)
  rDoyPerig <- rDoyEqui + (rLongitPerig - 2 * rExentr * sin(rLongitPerig)) / rOmega

  return(rDoyPerig)
}
