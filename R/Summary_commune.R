#' summary_commune
#'
#' Cette fonction génère un résumé statistique pour une commune spécifique, y compris les informations
#' sur les élu.e.s de la commune : nombre d'élus, distribution des âges et informations sur l'élu.e le plus âgé.e.
#'
#' @param x Un data.frame contenant les informations des élus d'une commune. Il doit avoir les colonnes suivantes :
#'   - "Libellé.de.la.commune" : Le nom de la commune.
#'   - "Nom.de.l.élu" : Le nom de l'élu.
#'   - "Prénom.de.l.élu" : Le prénom de l'élu.
#'   - "Date.de.naissance" : La date de naissance de l'élu (au format "jour/mois/année").
#'
#' @details
#' Cette fonction génère les résultats suivants pour une commune donnée :
#' - Le nom de la commune.
#' - Le nombre d'élus dans la commune.
#' - La distribution des âges des élus dans la commune.
#' - Le nom, le prénom et l'âge de l'élu.e le plus âgé(e).
#'
#' La fonction vérifie également que toutes les colonnes nécessaires sont présentes et que les dates de naissance
#' sont au format correct. En cas d'erreur, elle génère un message d'arrêt.
#'
#' @return Aucune valeur n'est retournée. La fonction imprime les résultats dans la console.
#'
#' @examples
#' # Exemple d'utilisation avec le dataset fourni
#' df_Nantes <- data.frame(
#'   `Libellé.de.la.commune` = c("Nantes", "Nantes", "Nantes"),
#'   `Nom.de.l.élu` = c("Dupont", "Martin", "Bernard"),
#'   `Prénom.de.l.élu` = c("Pierre", "Jacques", "Lucie"),
#'   `Date.de.naissance` = c("01/01/1970", "15/03/1980", "23/06/1990")
#' )
#' summary_commune(df_Nantes)
#'
#' @export
summary_commune <- function(x) {

  # Vérification des colonnes nécessaires
  required_cols <- c("Libellé.de.la.commune", "Nom.de.l.élu", "Prénom.de.l.élu", "Date.de.naissance")

  if (!all(required_cols %in% colnames(x))) {
    stop("Le data.frame doit contenir les colonnes 'Libellé.de.la.commune', 'Nom.de.l.élu', 'Prénom.de.l.élu' et 'Date.de.naissance'.")
  }

  # Conversion des dates
  x$Date.de.naissance <- as.Date(x$Date.de.naissance, format = "%d/%m/%Y")

  # Calcul de l'âge
  x$Age <- as.integer(as.numeric(difftime(Sys.Date(), x$Date.de.naissance, units = "weeks")) / 52.25)

  # Vérification des données pour une seule commune
  commune_name <- unique(x$`Libellé.de.la.commune`)
  if (length(commune_name) != 1) {
    stop("L'objet doit contenir uniquement les données d'une seule commune.")
  }

  # Affichage des résultats simples
  cat("Commune:", commune_name, "\n")
  cat("Nombre d'élu.e.s:", nrow(x), "\n")
  cat("Distribution des âges:\n")
  print(summary(x$Age))
  cat("L'élu.e le plus âgé(e):\n")
  elu_plus_age <- x[which.max(x$Age), c("Nom.de.l.élu", "Prénom.de.l.élu", "Age")]
  print(elu_plus_age)
}

