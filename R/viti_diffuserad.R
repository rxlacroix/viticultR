#' Compute diffuse radiation interception coefficient
#'
#' This function returns celestial longitude from a date
#' @param height The foliage height in meters
#' @param thickness The vineyard row thickness in meters
#' @param interrow_width The vineyard interrow width in meters
#' @param porosity The vine porosity (0<x<1)
#' @return The diffuse radiation interception coefficient
#' @export
#' @examples
#' viti_diffuserad(height = 1.4, thickness = 0.7, interrow_width = 2.4, porosity = 0.1)

viti_diffuserad <- function(height, thickness, interrow_width, porosity) {
  
  rKDIF1 <- thickness / interrow_width
  
  rKDIF2 <- 1 - (tan(0.5 * atan((interrow_width - thickness) / height)))
  rKDIF2 <- rKDIF2 * (1 - thickness / interrow_width)
  
  rKDIF3 <- (sin(0.5 * atan(2 * height / thickness)))^2
  rKDIF3 <- rKDIF3 - (sin(0.5 * atan(height / (interrow_width - thickness / 2))))^2
  rKDIF3 <- rKDIF3 * 2 * thickness / interrow_width
  
  rKdif <- rKDIF1 + ((1 - porosity) * rKDIF2) + (porosity * rKDIF3)
  
  return(rKdif)
}
