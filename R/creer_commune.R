#' Creer_commune
#'
#' Cette fonction prend un `data.frame` en entrée et renvoie un `data.frame`
#' avec la classe 'commune'. Le `data.frame` d'entrée doit inclure la colonne
#' 'Libellé.de.la.commune' et cette fonction va extraire les lignes correspondant
#' à une commune spécifique.
#'
#' @param data Un `data.frame` contenant des informations sur les communes.
#'              Il doit inclure la colonne 'Libellé.de.la.commune'.
#' @param commune_name Nom de la commune que vous voulez extraire du `data.frame`. Rappellez-vous de ne pas ajouter le numéro de la commune.
#' @return Un `data.frame` de la classe 'commune' créé à partir du `data.frame` d'entrée.
#' @export
#'
#' @examples
#' # Exemple d'utilisation de la fonction creer_commune
#' df <- data.frame(
#'   Code.du.département = c("01", "01", "02", "02"),
#'   Libellé.du.département = c("Ain", "Ain", "Aisne", "Aisne"),
#'   Libellé.de.la.commune = c("Lyon", "Lyon", "Soissons", "Soissons"),
#'   Nom.de.l.élu = c("Dupont", "Martin", "Lemoine", "Dubois")
#' )
#' commune_obj <- creer_commune(df, "Lyon")  # Sélectionne les données pour la commune "Lyon"
#' print(commune_obj)  # Affiche le data.frame de la commune avec la classe 'commune'
creer_commune <- function(data, commune_name) {
  # Vérification que la colonne 'Libellé.de.la.commune' existe dans le data.frame
  if (!"Libellé.de.la.commune" %in% colnames(data)) {
    stop("Le data.frame doit contenir la colonne 'Libellé.de.la.commune'.")
  }

  # Sélectionner uniquement les lignes pour la commune spécifiée
  commune_data <- data[data$Libellé.de.la.commune == commune_name, ]

  # Ajouter la classe 'commune' au data.frame
  class(commune_data) <- c("commune", class(commune_data))

  return(commune_data)
}
