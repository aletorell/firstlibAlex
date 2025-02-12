#' Plot_professions
#'
#' Cette fonction génère un graphique en barres horizontal représentant la distribution
#' des codes socio-professionnels des élus dans un data.frame. Elle utilise la colonne
#' "Libellé.de.la.catégorie.socio.professionnelle" pour créer un tableau de fréquences et
#' affiche un barplot de ces fréquences.
#'
#' @param data Un data.frame contenant les informations suivantes :
#'   - "Libellé.de.la.catégorie.socio.professionnelle" : La catégorie socio-professionnelle de l'élu.
#'
#'
#' @return Un graphique en barres horizontal affichant la distribution des codes socio-professionnels.
#'
#' @examples
#' # Exemple d'utilisation
#' data <- data.frame(
#'   Libellé.de.la.catégorie.socio.professionnelle = c("Enseignant", "Médecin", "Ingénieur", "Médecin", "Enseignant")
#' )
#' plot_code_professions(data)
#'
#' @export
plot_code_professions <- function(data) {
  # Définition des colonnes nécessaires
  required_cols <- c("Libellé.de.la.catégorie.socio.professionnelle")

  # Vérification que la colonne nécessaire est présente
  if (!all(required_cols %in% colnames(data))) {
    stop("Le data.frame doit contenir la colonne 'Libellé.de.la.catégorie.socio.professionnelle'.")
  }

  # Paramétrage des marges du graphique
  par(mar = c(5, 22, 6, 2))

  # Comptage des catégories socio-professionnelles
  profession_counts <- table(data$Libellé.de.la.catégorie.socio.professionnelle)

  # Création du graphique en barres horizontal
  barplot(profession_counts,
          main = "Distribution des Codes Socio-Professionnels",
          xlab = "Nombre d'Élus",
          col = "skyblue",
          horiz = TRUE,
          las = 1,
          cex.names = 0.7)
}
