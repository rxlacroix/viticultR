#' Compute absorption rate of diffuse radiation
#'
#' This function returns absorption rate of diffuse radiation
#' @param leaf_albedo The foliage albedo
#' @param ground_albedo The ground albedo
#' @param kdif diffuse radiation interception coefficient
#' @return The absorption rate of diffuse radiation
#' @export
#' @examples
#' cardif(leaf_albedo = 0.22, ground_albedo = 0.18, kdif = 0.6)

cardif <- function(leaf_albedo, ground_albedo, kdif) {

  cardif <- (1 - leaf_albedo) * ((1 - ground_albedo * kdif) * kdif + ground_albedo * kdif)
  return(cardif)
}
