#' Creer_departement
#'
#' Cette fonction prend un `data.frame` en entrée et renvoie un `data.frame`
#' avec la classe 'departement'. Le `data.frame` d'entrée doit inclure la colonne
#' 'Libellé.du.département' et cette fonction va extraire les lignes correspondant
#' à un département spécifique.
#'
#' @param data Un `data.frame` contenant des informations sur les départements.
#'              Il doit inclure la colonne 'Libellé.du.département'.
#' @param departement_name Nom du département que vous voulez extraire du `data.frame`. Rappellez-vous de ne pas ajouter le numéro du département.
#' @return Un `data.frame` de la classe 'departement' créé à partir du `data.frame` d'entrée.
#' @export
#'
#' @examples
#' # Exemple d'utilisation de la fonction creer_departement
#' df <- data.frame(
#'   Code.du.département = c("01", "01", "02", "02"),
#'   Libellé.du.département = c("Ain", "Ain", "Aisne", "Aisne"),
#'   Libellé.de.la.commune = c("Lyon", "Lyon", "Soissons", "Soissons"),
#'   Nom.de.l.élu = c("Dupont", "Martin", "Lemoine", "Dubois")
#' )
#' departement_obj <- creer_departement(df, "Ain")  # Sélectionne les données pour le département "Ain"
#' print(departement_obj)  # Affiche le data.frame du département avec la classe 'departement'
creer_departement <- function(data, departement_name) {
  # Vérification que la colonne 'Libellé.du.département' existe dans le data.frame
  if (!"Libellé.du.département" %in% colnames(data)) {
    stop("Le data.frame doit contenir la colonne 'Libellé.du.département'.")
  }

  # Sélectionner uniquement les lignes pour le département spécifié
  departement_data <- data[data$Libellé.du.département == departement_name, ]

  # Ajouter la classe 'departement' au data.frame
  class(departement_data) <- c("departement", class(departement_data))

  return(departement_data)
}
