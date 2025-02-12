#' Trouver l'élu le plus âgé
#'
#' Cette fonction prend un data.frame contenant des informations sur les élus, y compris leur date de naissance,
#' et retourne les informations de l'élu le plus âgé.
#'
#' @param data Un data.frame contenant les colonnes suivantes :
#'   - "Nom.de.l.élu" : Le nom de l'élu.
#'   - "Prénom.de.l.élu" : Le prénom de l'élu.
#'   - "Date.de.naissance" : La date de naissance de l'élu au format "jour/mois/année".
#'
#' @return Un data.frame contenant les informations de l'élu le plus âgé, incluant :
#'   - "Nom.de.l.élu" : Le nom de l'élu.
#'   - "Prénom.de.l.élu" : Le prénom de l'élu.
#'   - "Age" : L'âge de l'élu en années.
#'
#' @examples
#' # Exemple d'utilisation
#' data <- data.frame(
#'   Nom.de.l.élu = c("Dupont", "Lemoine", "Dufresne"),
#'   Prénom.de.l.élu = c("Pierre", "Sophie", "Marc"),
#'   Date.de.naissance = c("01/01/1970", "15/05/1980", "23/07/1965")
#' )
#' # Appeler la fonction pour trouver l'élu le plus âgé
#' trouver_l_elu_le_plus_age(data)
#' # Retourne : Dupont, Pierre, 55
#'
#' @export
trouver_l_elu_le_plus_age <- function(data) {

  required_cols <- c("Nom.de.l.élu", "Prénom.de.l.élu", "Date.de.naissance")

  # Vérification que toutes les colonnes nécessaires sont présentes
  if (!all(required_cols %in% colnames(data))) {
    stop("Le data.frame doit contenir les colonnes 'Nom.de.l.élu', 'Prénom.de.l.élu' et 'Date.de.naissance' .")
  }

  # Conversion de la colonne Date.de.naissance au format Date
  data$Date.de.naissance <- as.Date(data$Date.de.naissance, format = "%d/%m/%Y")

  # Calculer l'âge en fonction de la date de naissance
  data$Age <- as.integer(as.numeric(difftime(Sys.Date(), data$Date.de.naissance, units = "weeks")) / 52.25)

  # Trier les élus par la date de naissance (du plus vieux au plus jeune)
  data_sorted <- data[order(data$Date.de.naissance), ]

  # Retourner les informations de l'élu le plus âgé
  elu_plus_age <- data_sorted[1, c("Nom.de.l.élu", "Prénom.de.l.élu", "Age")]

  return(elu_plus_age)
}

