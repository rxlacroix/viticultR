#' rurawls
#' Calcul de la reserve utile selon la formule de Rawls et al. (1982)
#' @param h taille de l'horizon en m
#' @param eg proportion (0<x<1) d'elements grossiers
#' @param s pourcentage de sables (0<x<100)
#' @param a pourcentage d'argiles (0<x<100)
#' @param mo pourcentage de matieres organiques  (0<x<100)
#' @param pierosite TRUE ou FALSE si la formule doit prendre en compte la pierosite (elements grossiers) ou non
#'
#' @return reserve utile en mm
#' @export
#'
#' @examples
#'
#' rurawls(h=0.5,s=43,a=18,mo=1, pierosite=TRUE, eg = 0.4)
#'

rurawls <- function(h,s, a, mo, pierosite,eg ){

  W330 <- (257.6-(2 * s)+(3.6*a)+29.9*mo)
  W15000 <- (26+(5*a)+(15.8*mo))

  if(pierosite == FALSE){
    RU_RAWLS <- (W330 - W15000)*h

  } else {
    RU_RAWLS <- (W330 - W15000)*h
    RU_RAWLS <- RU_RAWLS * (1-eg)

  }

  cat("RU Rawls =", RU_RAWLS)
  return(RU_RAWLS)
}

