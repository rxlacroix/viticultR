#' Compute reflexion rate of diffuse radiation
#'
#' This function returns reflexion rate of diffuse radiation
#' @param leaf_albedo The foliage albedo
#' @param ground_albedo The ground albedo
#' @param kdif diffuse radiation interception coefficient
#' @return The reflexion rate of diffuse radiation
#' @export
#' @examples
#' redif(leaf_albedo = 0.22, ground_albedo = 0.18, kdif = 0.6)

redif <- function(leaf_albedo, ground_albedo, kdif) {

  redif <-  (leaf_albedo - ground_albedo * (1 - kdif) - leaf_albedo * ground_albedo * kdif / 2) * kdif
  redif <- redif + (ground_albedo * (1 - kdif) + leaf_albedo * ground_albedo * kdif / 2)

  return(redif)
}
