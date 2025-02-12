#' Calcul_distribution_age
#'
#' Cette fonction calcule la distribution des âges des individus dans un jeu de données
#' en utilisant la date de naissance. Elle retourne les quantiles des âges des individus,
#' allant du minimum au maximum, ainsi que les quantiles à 25%, 50% (médiane) et 75%.
#'
#' @param data Un data.frame contenant une colonne "Date.de.naissance" avec les dates de naissance
#' des individus au format "jour/mois/année".
#'
#' @return Un vecteur numérique contenant les quantiles des âges des individus dans le jeu de données.
#' Les quantiles sont calculés pour les probabilités suivantes : 0, 0.25, 0.5, 0.75, et 1.
#'
#' @export
calcul_distribution_age <- function(data) {
  data$Date.de.naissance <- as.Date(data$Date.de.naissance, format = "%d/%m/%Y")

  data$Age <- as.integer(as.numeric(difftime(Sys.Date(), data$Date.de.naissance, units = "weeks")) / 52.25)

  distribution_age <- as.integer(quantile(data$Age, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))

  return(distribution_age)
}
