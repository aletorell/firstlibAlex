#' Plot_commune
#'
#' Cette fonction génère un graphique en barres pour visualiser la distribution des catégories socio-professionnelles des élu.e.s dans une commune spécifique.
#' Le titre du graphique sera le nom de la commune suivi du nom de son département.
#' L'axe des abscisses s'appelle "Libellés des codes professionnels pour les élus", où X est remplacé par le nombre d'élus de la commune.
#'
#' @param data Un data.frame contenant les informations des élu.e.s. Il doit inclure les colonnes suivantes :
#'   - "Libellé.de.la.commune" : Le nom de la commune.
#'   - "Libellé.du.département" : Le nom du département.
#'   - "Libellé.de.la.catégorie.socio.professionnelle" : La catégorie socio-professionnelle des élu.e.s.
#'
#' @return Aucun retour, mais affiche un graphique en barres horizontal.
#'
#' @examples
#' # Exemple d'utilisation ajusté à la fonction plot_commune
#' # Créons un data.frame de données fictives
#' df <- data.frame(
#'   Code.du.département = c("01", "01", "01", "01", "02", "02"),
#'   Libellé.du.département = c("Ain", "Ain", "Ain", "Ain", "Aisne", "Aisne"),
#'   Libellé.de.la.commune = c("Lyon", "Lyon", "Lyon", "Lyon", "Soissons", "Soissons"),
#'   Nom.de.l.élu = c("Dupont", "Martin", "Lemoine", "Bernard", "Dubois", "Lemoine"),
#'   Prénom.de.l.élu = c("Jean", "Pierre", "Luc", "Sophie", "Marc", "Claire"),
#'   Libellé.de.la.catégorie.socio.professionnelle = c("Médecin", "Ingénieur", "Enseignant", "Médecin", "Médecin", "Ingénieur")
#' )
#'
#' # Filtrons le data.frame pour ne contenir que les données d'une commune spécifique
#' df_lyon <- df[df$Libellé.de.la.commune == "Lyon", ]
#'
#' # Appel de la fonction plot_commune avec le data.frame df_lyon
#' plot_commune(df_lyon)
#'
#' @export
plot_commune <- function(data) {
  # Vérification des colonnes nécessaires
  required_cols <- c("Libellé.de.la.commune", "Libellé.du.département", "Libellé.de.la.catégorie.socio.professionnelle")

  if (!all(required_cols %in% colnames(data))) {
    stop("Le data.frame doit contenir les colonnes nécessaires.")
  }

  # Récupération des communes uniques
  communes <- unique(data$Libellé.de.la.commune)

  # Si plusieurs communes sont présentes, afficher un message d'avertissement
  if (length(communes) > 1) {
    stop("Le data.frame contient plusieurs communes. Veuillez spécifier une seule commune.")
  }

  # Filtrage des données pour la commune actuelle
  commune_data <- data[data$Libellé.de.la.commune == communes[1], ]

  # Récupération du nom du département
  department_name <- unique(commune_data$Libellé.du.département)

  # Comptage des catégories socio-professionnelles pour cette commune
  profession_counts <- table(commune_data$Libellé.de.la.catégorie.socio.professionnelle)

  # Comptage du nombre d'élus dans la commune
  num_elus <- length(unique(commune_data$Nom.de.l.élu))

  # Ajustement des marges du graphique
  par(mar = c(5, 22, 6, 2))

  # Création du graphique en barres horizontal
  barplot(profession_counts,
          main = paste("Distribution des Codes Socio-Professionnels de", communes[1], "(", department_name, ")"),
          xlab = paste("Libellés des codes professionnels pour les élus (", num_elus, "élus)"),
          col = "skyblue",
          horiz = TRUE,
          las = 1,
          cex.names = 0.7)
}
