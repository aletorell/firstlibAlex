#' plot_departement
#'
#' Cette fonction génère des graphiques en barres horizontales pour visualiser les 10 catégories socio-professionnelles les plus représentées dans un département donné.
#' Le titre du graphique sera le nom du département, suivi du nombre de communes dans ce département.
#' L'axe des abscisses s'appelle : "Libellés des 10 codes professionnels les plus représentés pour le département", où X est remplacé par le nom du département.
#'
#' @param data Un data.frame contenant les informations des élu.e.s. Il doit inclure les colonnes suivantes :
#'   - "Libellé.du.département" : Le nom du département.
#'   - "Libellé.de.la.catégorie.socio.professionnelle" : La catégorie socio-professionnelle des élu.e.s.
#'   - "Libellé.de.la.commune" : Le nom de la commune.
#'
#' @return Aucun retour, mais affiche un graphique en barres horizontal pour chaque département dans les données.
#'
#' @examples
#' # Exemple d'utilisation ajusté à la fonction plot_departement
#' # Créons un data.frame de données fictives
#' df <- data.frame(
#'   Code.du.département = c("01", "01", "01", "01", "02", "02"),
#'   Libellé.du.département = c("Ain", "Ain", "Ain", "Ain", "Aisne", "Aisne"),
#'   Libellé.de.la.commune = c("Lyon", "Lyon", "Lyon", "Lyon", "Soissons", "Soissons"),
#'   Nom.de.l.élu = c("Dupont", "Martin", "Lemoine", "Bernard", "Dubois", "Lemoine"),
#'   Libellé.de.la.catégorie.socio.professionnelle = c("Médecin", "Ingénieur", "Enseignant", "Médecin", "Médecin", "Ingénieur")
#' )
#'
#' # Filtrons le data.frame pour qu'il ne contienne que les données d'un département spécifique
#' df_ain <- df[df$Libellé.du.département == "Ain", ]
#'
#' # Appel de la fonction plot_departement avec le data.frame df_ain
#' plot_departement(df_ain)
#'
#' @export
plot_departement <- function(data) {
  # Vérification des colonnes nécessaires
  required_cols <- c("Libellé.du.département", "Libellé.de.la.catégorie.socio.professionnelle")

  if (!all(required_cols %in% colnames(data))) {
    stop("Le data.frame doit contenir les colonnes nécessaires.")
  }

  # Obtenir les départements uniques
  departments <- unique(data$Libellé.du.département)

  # Si plusieurs départements sont présents, afficher un message d'erreur
  if (length(departments) > 1) {
    stop("Le data.frame contient plusieurs départements. Veuillez spécifier un seul département.")
  }

  # Itérer sur les départements et créer un graphique pour chaque département
  for (department in departments) {
    # Filtrage des données pour le département actuel
    department_data <- data[data$Libellé.du.département == department, ]

    # Comptage des catégories socio-professionnelles pour ce département
    profession_counts <- table(department_data$Libellé.de.la.catégorie.socio.professionnelle)

    # Sélectionner les 10 catégories les plus représentées
    top_professions <- sort(profession_counts, decreasing = TRUE)[1:10]

    # Paramétrage des marges du graphique
    par(mar = c(5, 22, 6, 2))

    # Création du graphique
    barplot(top_professions,
            main = paste("Distribution des Codes Socio-Professionnels du département", department, "(", length(unique(department_data$Libellé.de.la.commune)), "communes)"),
            xlab = paste("Libellés des 10 codes professionnels les plus représentés pour le département", department),
            col = "skyblue",
            horiz = TRUE,
            las = 1,
            cex.names = 0.7)
  }
}
