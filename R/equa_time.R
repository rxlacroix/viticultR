#' Compute equation of time of a date
#'
#' This function returns equation of  from a date
#' @param date The date in format ("2019-05-30")
#' @return The equation of time
#' @export
#' @examples
#' equa_time("2019-05-30")

equa_time <- function(date) {

  iNJ <- lubridate::yday(date)
  rNJ <- iNJ + 0.5
  ian <- lubridate::year(date)

  f <- 279.575 + 0.9856 * (rNJ)
  f <- f * pi / 180
  rEquaTemps <- (-104.7 * sin(f) + 596.2 * sin(2 * f) + 4.3 * sin(3 * f) - 12.7 * sin(4 * f) - 429.3 * cos(f) - 2.0 * cos(2 * f) + 19.3 * cos(3 * f)) / 60 # (11.4) Equation of time: ET is a 15-20 minute correction which depends on calendar day

  return(rEquaTemps)
}
