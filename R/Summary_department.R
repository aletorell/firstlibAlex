#' Summary_department
#'
#' Cette fonction génère un résumé statistique des élus d'un département, y compris les informations
#' sur les âges, les élus les plus âgés et les plus jeunes, ainsi que les communes ayant les moyennes
#' d'âge les plus faibles et les plus élevées.
#'
#' @param x Un data.frame contenant les informations des élus. Il doit avoir les colonnes suivantes :
#'   - "Code.du.département" : Le code du département.
#'   - "Libellé.du.département" : Le nom du département.
#'   - "Libellé.de.la.commune" : Le nom de la commune.
#'   - "Nom.de.l.élu" : Le nom de l'élu.
#'   - "Prénom.de.l.élu" : Le prénom de l'élu.
#'   - "Date.de.naissance" : La date de naissance de l'élu (au format "jour/mois/année").
#'
#' @return Aucune valeur n'est retournée, mais la fonction affiche les résultats suivants :
#'   - Le nom du département.
#'   - Le nombre de communes dans le département.
#'   - Le nombre d'élu.e.s dans le département.
#'   - La distribution des âges des élu.e.s.
#'   - Le nom, le prénom, l'âge et la commune de l'élu.e le plus âgé(e).
#'   - Le nom, le prénom, l'âge et la commune de l'élu.e le plus jeune.
#'   - Le nom de la commune ayant la moyenne d'âge la plus faible, ainsi que la distribution des âges des élu.e.s de cette commune.
#'   - Le nom de la commune ayant la moyenne d'âge la plus élevée, ainsi que la distribution des âges des élu.e.s de cette commune.
#'
#' @examples
#' # Exemple d'utilisation
#' # Supposons que df est un dataframe contenant les données nécessaires
#' df <- data.frame(
#'   `Code.du.département` = c("01", "01", "01"),
#'   `Libellé.du.département` = c("Ain", "Ain", "Ain"),
#'   `Libellé.de.la.commune` = c("Bourg-en-Bresse", "Ambérieu-en-Bugey", "Lagnieu"),
#'   `Nom.de.l.élu` = c("Dupont", "Martin", "Bernard"),
#'   `Prénom.de.l.élu` = c("Pierre", "Jacques", "Lucie"),
#'   `Date.de.naissance` = c("01/01/1970", "15/03/1980", "23/06/1990")
#' )
#'
#' # Appel de la fonction avec le dataframe exemple
#' Summary_departement(df)
#'
#' @export
Summary_departement <- function(x) {
  required_cols <- c("Code.du.département", "Libellé.du.département", "Libellé.de.la.commune",
                     "Nom.de.l.élu", "Prénom.de.l.élu", "Date.de.naissance")

  # Vérification des colonnes
  if (!all(required_cols %in% colnames(x))) {
    stop("Le data.frame doit contenir les colonnes nécessaires.")
  }

  # Vérification qu'il n'y a qu'un seul département
  if (length(unique(x$`Libellé.du.département`)) > 1) {
    stop("L'objet doit contenir les données d'un seul département.")
  }

  # Nom du département
  nom_departement <- unique(x$`Libellé.du.département`)

  # Nombre de communes dans le département
  nombre_communes <- length(unique(x$`Libellé.de.la.commune`))

  # Nombre d’élu.e.s dans le département
  nombre_elus <- nrow(x)

  # Conversion des dates
  x$Date.de.naissance <- as.Date(x$Date.de.naissance, format = "%d/%m/%Y")

  # Calcul de l'âge
  x$Age <- as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(x$Date.de.naissance, "%Y"))

  # Distribution des âges
  age_distribution <- summary(x$Age)

  # Élu.e le plus âgé.e
  max_age <- max(x$Age, na.rm = TRUE)
  elu_plus_age <- x[x$Age == max_age, c("Nom.de.l.élu", "Prénom.de.l.élu", "Libellé.de.la.commune", "Age")]

  # Élu.e le plus jeune
  min_age <- min(x$Age, na.rm = TRUE)
  elu_plus_jeune <- x[x$Age == min_age, c("Nom.de.l.élu", "Prénom.de.l.élu", "Libellé.de.la.commune", "Age")]

  # Commune avec l'âge moyen le plus bas
  age_moyen_par_commune <- aggregate(Age ~ Libellé.de.la.commune, data = x, FUN = mean, na.rm = TRUE)
  commune_plus_jeune <- age_moyen_par_commune[which.min(age_moyen_par_commune$Age), "Libellé.de.la.commune"]
  distribution_commune_jeune <- summary(x[x$`Libellé.de.la.commune` == commune_plus_jeune, "Age"])

  # Commune avec l'âge moyen le plus élevé
  commune_plus_agee <- age_moyen_par_commune[which.max(age_moyen_par_commune$Age), "Libellé.de.la.commune"]
  distribution_commune_agee <- summary(x[x$`Libellé.de.la.commune` == commune_plus_agee, "Age"])

  # Affichage des résultats
  cat("Nom du département:", nom_departement, "\n")
  cat("Nombre de communes:", nombre_communes, "\n")
  cat("Nombre d'élu.e.s:", nombre_elus, "\n")
  cat("Distribution des âges des élu.e.s:\n")
  print(age_distribution)

  cat("\nL'élu.e le/la plus âgé(e):", elu_plus_age$Nom.de.l.élu, elu_plus_age$Prénom.de.l.élu, "- Age:", elu_plus_age$Age, "- Commune:", elu_plus_age$`Libellé.de.la.commune`, "\n")
  cat("\nL'élu.e le/la plus jeune:", elu_plus_jeune$Nom.de.l.élu, elu_plus_jeune$Prénom.de.l.élu, "- Age:", elu_plus_jeune$Age, "- Commune:", elu_plus_jeune$`Libellé.de.la.commune`, "\n")

  cat("\nCommune avec la moyenne d'âge la plus faible:", commune_plus_jeune, "\n")
  cat("Distribution des âges des élu.e.s de cette commune:\n")
  print(distribution_commune_jeune)

  cat("\nCommune avec la moyenne d'âge la plus élevée:", commune_plus_agee, "\n")
  cat("Distribution des âges des élu.e.s de cette commune:\n")
  print(distribution_commune_agee)
}
