#' Compute diffuse radiation interception coefficient
#'
#' This function returns celestial longitude from a date
#' @param height The foliage height in meters
#' @param thickness The vineyard row thickness in meters
#' @param interrow_spacing The vineyard interrow width in meters
#' @param porosity The vine porosity (0<x<1)
#' @return The diffuse radiation interception coefficient
#' @export
#' @examples
#' viti_diffuserad(height = 1.4, thickness = 0.7, interrow_spacing = 2.4, porosity = 0.1)

viti_diffuserad <- function(height, thickness, interrow_spacing, porosity) {

  rKDIF1 <- thickness / interrow_spacing

  rKDIF2 <- 1 - (tan(0.5 * atan((interrow_spacing - thickness) / height)))
  rKDIF2 <- rKDIF2 * (1 - thickness / interrow_spacing)

  rKDIF3 <- (sin(0.5 * atan(2 * height / thickness)))^2
  rKDIF3 <- rKDIF3 - (sin(0.5 * atan(height / (interrow_spacing - thickness / 2))))^2
  rKDIF3 <- rKDIF3 * 2 * thickness / interrow_spacing

  rKdif <- rKDIF1 + ((1 - porosity) * rKDIF2) + (porosity * rKDIF3)

  return(rKdif)
}
