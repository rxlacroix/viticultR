#' Compute solar radiation analysis of a vine location at a date
#'
#' This function returns equation of from a date and the plot of the radiations
#' @param lat The latitude in decimal degrees
#' @param lon The longitude in decimal degrees
#' @param date The date in format ("2019-05-30")
#' @param tz The timezone code see https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
#' @param leaf_albedo The foliage albedo
#' @param ground_albedo The ground albedo
#' @param height The foliage height in meters
#' @param thickness The vineyard row thickness in meters
#' @param interrow_width The vineyard interrow width in meters
#' @param porosity The vine porosity (0<x<1)
#' @param row_aspect The row aspect (0 : NS, 90 : EW, -45 : NW-SE)
#' @importFrom magrittr "%>%"
#' @return The data frame of computed radiations
#' @export
#' @examples
#' vitirad(lat = 43.5, lon = 4.7,
#'         date = "2019-05-30", tz = "CET", leaf_albedo = 0.22,
#'         ground_albedo = 0.18, height = 1.4, thickness = 0.7,
#'         interrow_width = 2.4, porosity = 0.1, row_aspect = 0)

vitirad <- function(lat, lon, date, tz, leaf_albedo, ground_albedo, height, thickness, interrow_width, porosity, row_aspect) {

  date <- as.Date(date) # date d'entree

  rEcartRangs <- interrow_width # ecartement entre rangs
  rHautMax <- height # hauteur feuillee
  rLargMax <- thickness # largeur des rangs
  rPorositeMin <- porosity # porosite
  rAlbedoFeuilles <- leaf_albedo # albedo feuille
  rAlbedoSol <- ground_albedo #  albedo sol
  rOrientRangs <- row_aspect # 0 pour NS, 90 pour EW, -45 NW-SE
  rDt <- 1800

  rRgiJour <- 20



  rHrLevSol <- suncalc::getSunlightTimes(date = date, lat = lat, lon = lon, keep = "sunrise", tz = tz)[1, 4]
  rHrLevSol <- lubridate::hour(rHrLevSol) + lubridate::minute(rHrLevSol) / 60 + lubridate::second(rHrLevSol) / 3600
  rHrCouSol <- suncalc::getSunlightTimes(date = date, lat = lat, lon = lon, keep = "sunset", tz = tz)[1, 4]
  rHrCouSol <- lubridate::hour(rHrCouSol) + lubridate::minute(rHrCouSol) / 60 + lubridate::second(rHrCouSol) / 3600

  rDurJour <- rHrCouSol - rHrLevSol
  iNJ <- lubridate::yday(date)
  rNJ <- iNJ + 0.5
  ian <- lubridate::year(date)

  rEquaTemps <- equa_time("2019-05-30")
  rExentr <- .016751 - .00000042 * (ian - 1900)
  rOmega <- 2 * pi / 365.256363004

  rDoyPerig <- day_of_perigee(lubridate::year(date))
  #   DECLINAISON du Soleil           (radians)

  rDelta <- solrad::Declination(iNJ) * pi / 180


  rLatRad <- lat * (pi / 180) # Latitude en radians
  # passage direction des rangs a angle de la normale aux rangs avec la direction NS
  rOrientRangs <- rOrientRangs + 90 # oriente de N a S
  if (rOrientRangs >= 180) {
    rOrientRangs <- rOrientRangs - 180
  }

  rOrientRangsRad <- rOrientRangs * pi / 180 #  rOrientRangs = ANGLE NORMALE AU RANG AVEC DIRECTION NORD-->SUD (DEGRES)

  # Calcul des coefficients d'interception du rayonnement diffus :
  # '       rKdif,  rayonnement venant du ciel et rKdifSol, rayonnement venant du sol


  rKdif <- viti_diffuserad(height = rHautMax, thickness = rLargMax, interrow_width = rEcartRangs, porosity = rPorositeMin)
  rKdifSol <- rKdif

  #       calcul des taux d'absorption (cardif) et de reflexion (crrdif) du rayt.diffus

  rCardif <- cardif(leaf_albedo = rAlbedoFeuilles, ground_albedo = rAlbedoSol, kdif = rKdif)

  rCrrdif <-redif(leaf_albedo = rAlbedoFeuilles, ground_albedo = rAlbedoSol, kdif = rKdif)

  #        Positions Geometriques Limites
  #        ex-subroutine cbetal(rBetaLim, nbetal, imodel)
  #        interception du Rayonnement Solaire Direct
  #        calcul des nbetal valeurs de rBetaLim(),
  #        rapports correspondant a des positions caracteristiques limites
  #        (composante Horizontale du Rayt DIRECT sur composante Verticale)

  rBetaLim <- ""
  rBetaLim[1] <- rLargMax / rHautMax
  rBetaLim[2] <- (rEcartRangs - rLargMax) / rHautMax
  rBetaLim[3] <- rEcartRangs / rHautMax
  rBetaLim[4] <- (rEcartRangs + rLargMax) / rHautMax
  rBetaLim[5] <- (2 * rEcartRangs - rLargMax) / rHautMax
  rBetaLim[6] <- (2 * rEcartRangs) / rHautMax
  rBetaLim[7] <- (3 * rEcartRangs) / rHautMax

  rSomRytGlo <- 0 # intialisation des integrales
  rSomRytGloVigne <- 0
  rSomRytGloSol <- 0
  rSomAlbedoVignoble <- 0
  rSomRgvRel <- 0
  rSomRgsRel <- 0
  rHr <- 0
  rHrTSV <- 0
  rHrRad <- 0
  x <- 0
  rHautSol <- 0
  y <- 0
  rAzimSol <- 0
  a <- 0
  b <- 0
  rAHDeg <- 0
  rAHRad <- 0
  rHautSolRad <- 0
  rAzimSolRad <- 0
  rHeureTSV <- 0
  r <- 0
  rRytGloExtraAtm <- 0
  RG0 <- 0
  rRytGlo <- 0
  GSG0 <- 0
  DSG <- 0
  rRytDif <- 0
  rRytDir <- 0
  rBeta1 <- 0
  rBeta2 <- 0
  rBeta3 <- 0
  rBeta <- 0
  rKdir <- 0
  rCardir <- 0
  rCrrdir <- 0
  rRytGloVigne <- 0
  rRytGloSol <- 0
  rAlbedoVignoble <- 0
  rRgvRel <- 0
  rRgsRel <- 0


  for (j in 0:47) {
    rHr[j + 1] <- ((rDt / 3600) / 2) + ((j * rDt) / 3600)
    # ... Calcul Heure en Temps Solaire Vrai, rHrTSV et Angle Horaire (radians), rHrRad
    rHrTSV[j] <- (rHr[j + 1] - 12) - (lon / 15) - (rEquaTemps / 60)
    rHrRad[j] <- rHrTSV[j] * (pi / 12)

    # ... Calcul de la HAUTEUR du Soleil



    x[j] <- sin(rLatRad) * sin(rDelta) + (cos(rLatRad) * cos(rDelta) * cos(rHrRad[j]))

    if (isTRUE(x[j] < 0)) {
      rHautSol[j] <- 0 #-99.9                    'le Soleil n'est pas leve
    } else if (isTRUE(x[j] == 1)) {
      rHautSol[j] <- 90
    } else {
      rHautSol[j] <- asin(x[j]) * 180 / pi
    }

    #   ' ... Calcul de l'AZIMUT du Soleil
    #   '        codage signe: Nord    -180 deg
    # '                      Est      -90 deg
    #   '                      Sud        0 deg
    # '                      Ouest     90 deg
    #   '                      Nord     180 deg
    # '
    y[j] <- sin(rLatRad) * cos(rHrRad[j]) - cos(rLatRad) * tan(rDelta)


    if (isTRUE(y[j] == 0)) {
      b[j] <- 90
      a[j] <- 0
    } else if (isTRUE(y[j] < 0)) {
      b[j] <- 180
      a[j] <- atan(sin(rHrRad[j]) / y[j]) * 180 / pi
    } else {
      b[j] <- 0
      a[j] <- atan(sin(rHrRad[j]) / y[j]) * 180 / pi
    }

    if (isTRUE(rHrRad[j] == 0)) {
      rAzimSol[j] <- 0
    } else if (isTRUE(rHrRad[j] < 0)) {
      rAzimSol[j] <- a[j] - b[j]
    } else {
      rAzimSol[j] <- a[j] + b[j]
    }

    rHeureTSV[j] <- rHr[j + 1] - lon / 15 - rEquaTemps / 60 # temps solaire vrai (h)

    rAHDeg[j] <- (rHeureTSV[j] - 12) * 15 # angle horaire  (deg)
    rAHRad[j] <- rAHDeg[j] * pi / 180 # angle horaire  (radians)
    rHautSolRad[j] <- rHautSol[j] * pi / 180 # hauteur soleil (radians)
    rAzimSolRad[j] <- rAzimSol[j] * pi / 180 # azimut soleil (radians)

    if (isTRUE(rHautSol[j] > 0)) {
      r <- 1353 * (1 + 2 * rExentr * cos(rOmega * (rNJ - rDoyPerig)))
      RG0[j] <- r * sin(rHautSol[j] * pi / 180)
    } else {
      RG0[j] <- NA
      rRytGloExtraAtm[j] <- NA
    }
  }

  rRytGloExtraAtmJour <- (sum(RG0, na.rm = TRUE) * rDt) / 1000000

  for (j in 0:47) {
    rRytGlo[j] <- RG0[j] * rRgiJour / rRytGloExtraAtmJour


    if (isTRUE(RG0[j] > 0)) {
      GSG0[j] <- rRytGlo[j] / RG0[j]
    } else {
      GSG0[j] <- 0
    }
    DSG[j] <- 1.09 - 2.6896 * GSG0[j] * GSG0[j] + 1.2843 * GSG0[j] * GSG0[j] * GSG0[j] # relation journaliere a Bordeaux (CV)
    if (isTRUE(DSG[j] > 1)) {
      DSG[j] < 1
    }

    rRytDif[j] <- rRytGlo[j] * DSG[j]
    rRytDir[j] <- rRytGlo[j] - rRytDif[j]

    rBeta1[j] <- cos(rOrientRangsRad) * tan(rLatRad)
    rBeta2[j] <- (sin(rDelta) * cos(rOrientRangsRad)) / (sin(rHautSolRad[j]) * cos(rLatRad))
    rBeta3[j] <- (sin(rOrientRangsRad) * cos(rDelta) * sin(rAHRad[j])) / sin(rHautSolRad[j])
    rBeta[j] <- abs(rBeta1[j] - rBeta2[j] + rBeta3[j])

    if(isTRUE(rBeta[j]< as.double(rBetaLim[1]))){
      rKdir[j] <- ((rBeta[j] * rHautMax + rLargMax) / rEcartRangs)
    } else if (isTRUE(rBeta[j] > as.double(rBetaLim[1])) && rBeta[j] < as.double(rBetaLim[2])){
      rKdir[j] <- ((rBeta[j] * rHautMax + rLargMax) - rPorositeMin * (rBeta[j] * rHautMax - rLargMax)) / rEcartRangs
    } else if (isTRUE(rBeta[j] > as.double(rBetaLim[2])) && rBeta[j] < as.double(rBetaLim[3])){
      rKdir[j] <- (rEcartRangs - rPorositeMin * (rBeta[j] * rHautMax - rLargMax)) / rEcartRangs
    } else if (isTRUE(rBeta[j] > as.double(rBetaLim[3])) && rBeta[j] < as.double(rBetaLim[4])){
      rKdir[j] <- (rEcartRangs - rPorositeMin * (2 * rEcartRangs - rLargMax - rBeta[j] * rHautMax)) / rEcartRangs
    } else if (isTRUE(rBeta[j] > as.double(rBetaLim[4])) && rBeta[j] < as.double(rBetaLim[5])){
      rKdir[j] <- (rEcartRangs - rPorositeMin * (2 * rEcartRangs - rLargMax - rBeta[j] * rHautMax) - rPorositeMin * rPorositeMin * (rBeta[j] * rHautMax - rEcartRangs - rLargMax)) / rEcartRangs
    } else if (isTRUE(rBeta[j] > as.double(rBetaLim[5])) && rBeta[j] < as.double(rBetaLim[6])){
      rKdir[j] <- (rEcartRangs - rPorositeMin * rPorositeMin * (rBeta[j] * rHautMax - rEcartRangs - rLargMax)) / rEcartRangs
    } else if (isTRUE(rBeta[j] > as.double(rBetaLim[6])) && rBeta[j] < as.double(rBetaLim[7])){
      rKdir[j] <- (rEcartRangs - rPorositeMin * rPorositeMin * (rEcartRangs - rLargMax) + rPorositeMin * rPorositeMin * (1 - rPorositeMin) * (rBeta[j] * rHautMax - 2 * rEcartRangs - rLargMax)) / rEcartRangs
    } else {
      rKdir[j] <- (rEcartRangs - rPorositeMin * rPorositeMin * rPorositeMin * (rEcartRangs - rLargMax)) / rEcartRangs

    }


    # calcul des taux d'absorption (cardir) et de de reflexion (crrdir) du rayt. direct

    rCardir[j] <- (1 - rAlbedoFeuilles) * ((1 - rAlbedoSol * rKdifSol) * rKdir[j] + rAlbedoSol * rKdifSol)

    rCrrdir[j] <- (rAlbedoFeuilles - rAlbedoSol * (1 - rKdifSol) - rAlbedoFeuilles * rAlbedoSol * rKdifSol / 2) * rKdir[j]
    rCrrdir[j] <- rCrrdir[j] + (rAlbedoSol * (1 - rKdifSol) + rAlbedoFeuilles * rAlbedoSol * rKdifSol / 2)
    rRytGloVigne[j] <- rCardir[j] * rRytDir[j] + rCardif * rRytDif[j]
    rRytGloSol[j] <- (1 - rCardir[j] - rCrrdir[j]) * rRytDir[j] + (1 - rCardif - rCrrdif) * rRytDif[j]

    rAlbedoVignoble[j] <- (rCrrdir[j] * rRytDir[j] + rCrrdif * rRytDif[j]) / rRytGlo[j]
    rRgvRel[j] <- rRytGloVigne[j] / ((1 - rAlbedoVignoble[j]) * rRytGlo[j])
    rRgsRel[j] <- rRytGloSol[j] / ((1 - rAlbedoVignoble[j]) * rRytGlo[j])
  }


  heure <- lubridate::ymd_hms(paste0(date, paste(floor(rHr[1:47]), round((rHr[1:47] - floor(rHr[1:47])) * 60), sep = ":"), ":00"))
  heure <- lubridate::force_tz(heure, tzone = "UTC")
  heure <- lubridate::with_tz(heure, tzone = tz)


  df <- data.frame(
    heure, rHautSol, rAzimSol, RG0, rBeta, rKdir, rCardir, rCrrdir,
    rCardif, rKdif, rCrrdif, rRytGlo, rRytDir, rRytDif, rRytGloVigne,
    rRytGloSol, rAlbedoVignoble, rRgvRel, rRgsRel
  )
  df[is.na(df)] <- 0

  rgv <- sum(df$rRytGloVigne, na.rm = TRUE) * rDt / 1000000
  rgs <- sum(df$rRytGloSol, na.rm = TRUE) * rDt / 1000000
  rgi <- sum(df$rRytGlo, na.rm = TRUE) * rDt / 1000000

  albedo <- 1 - (rgv + rgs) / rgi
  RgvRel <- rgv / ((1 - albedo) * rgi)
  RgsRel <- rgs / ((1 - albedo) * rgi)

  cat(
    " Rgv = ", rgv, "MJ/m2 \n", "Rgs = ", rgs, "MJ/m2 \n", "Albedo = ",
    albedo, "\n", "Rgv / (1-a)Rg => KC vigne = ", RgvRel, "\n",
    "Rgs / (1-a)Rg => KC sol = ", RgsRel
  )



  df2 <- reshape2::melt(df, id.vars="heure")%>%
    dplyr::filter(variable %in% c('RG0','rRytGloVigne','rRytGloSol'))

  print(ggplot2::ggplot(df2, ggplot2::aes(x=heure, y=value, colour=variable)) +
    ggplot2::geom_line(ggplot2::aes(y = value, x = heure), size = 1)+
    ggplot2::scale_color_manual(name = "Radiation", values = c("orange", "green","brown"), labels = c("Global", "Abs. Vine", "Refl. Ground") )+
    ggplot2::geom_area(ggplot2::aes(y = value, fill = variable), alpha = 0.25, position = "identity", show.legend = FALSE) +
    ggplot2::scale_fill_manual(name = "Radiation", values = c("orange", "green","brown"), labels = c("Global", "Abs. Vine", "Refl. Ground") )+

    ggplot2::scale_x_datetime(breaks = scales::date_breaks("1 hour"),
                     labels=scales::date_format("%H:%M", tz = tz))+
    ggplot2::scale_y_continuous(breaks = seq(0, round(max(df2$value)+50,-2), by = 100), labels  = seq(0, round(max(df2$value)+50,-2), by = 100))+
    ggplot2::ylab("Radiation (W/m2)") +
    ggplot2::xlab(paste0(date))+
    ggplot2::theme_bw()+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))+
    ggplot2::labs(title = paste0("Rgv / (1-a)Rg  = ", round(RgvRel,4))))
  return(df)

}
