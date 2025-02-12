#' Compte_Nombre_d_adjoints
#'
#' Cette fonction compte le nombre d'adjoints
#'
#' @param data un dataframe qui contient les informations des élus.
#'
#' @return un entier correspondant au nombre d'adjoints.
#' @export
#'
compter_nombre_d_adjoints <- function(data) {
  required_cols <- c("Libellé.de.la.fonction")

  # Vérification que toutes les colonnes nécessaires sont présentes
  if (!all(required_cols %in% colnames(data))) {
    stop("Le data.frame doit contenir la colonne 'Libellé.de.la.fonction'.")
  }

  # Comptage en ignorant les NA
  Premier_adjoint <- sum(data$`Libellé.de.la.fonction` == "1er adjoint au Maire", na.rm = TRUE)
  Deuxieme_adjoint <- sum(data$`Libellé.de.la.fonction` == "2ème adjoint au Maire", na.rm = TRUE)
  Troisieme_adjoint <- sum(data$`Libellé.de.la.fonction` == "3ème adjoint au Maire", na.rm = TRUE)

  return(Premier_adjoint + Deuxieme_adjoint + Troisieme_adjoint)
}
