#' Compute celestial longitude of a date
#'
#' This function returns celestial longitude from a date
#' @param date The date in format ("2019-05-30")
#' @return The celestial longitude
#' @export
#' @examples
#' celestial_longitude("2019-05-30")

celestial_longitude <- function(date) {

  iNJ <- lubridate::yday(date)
  rNJ <- iNJ + 0.5
  ian <- lubridate::year(date)


  rOmega <- 2 * pi / 365.256363004

  rLongitPerig <- -1.374953 + .000300051 * (ian - 1900)
  rDoyEqui <- 80.08 + .2422 * (ian - 1900) - ((ian - 1901) / 4)

  rExentr <- .016751 - .00000042 * (ian - 1900)
  rDoyPerig <- rDoyEqui + (rLongitPerig - 2 * rExentr * sin(rLongitPerig)) / rOmega
  rLongitCeleste <- rLongitPerig + rOmega * (rNJ - rDoyPerig) + 2 * rExentr * sin(rOmega * (rNJ - rDoyPerig))

  return(rLongitCeleste)
}
