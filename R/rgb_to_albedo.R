#' Compute albedo from rgb soil color
#'
#' This function returns albedo
#' @param r The r
#' @param g The b
#' @param b The g
#' @return The albedo
#' @export
#' @examples
#' rgb_to_albedo(r=197,g=146,b=125)

rgb_to_albedo <- function(r,g,b) {
  albedo <- ((r/255)^2.2 + (g/255)^2.2 + (b/255)^2.2) / 3

  return(albedo)
}
