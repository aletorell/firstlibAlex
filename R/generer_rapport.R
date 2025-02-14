#' Générer un rapport basé sur les élections françaises de 2024
#'
#' Cette fonction génère un rapport en utilisant un fichier Quarto (.qmd) pour analyser les caractéristiques des élus lors des élections françaises de 2024, en se basant sur les paramètres 'commune' et 'departement'.
#' Le rapport sera rendu dans le fichier spécifié par l'utilisateur ou choisi via une boîte de dialogue si le chemin de sortie n'est pas précisé.
#'
#' @param commune Un numéro de commune (doit être un nombre à 4 chiffres minimum).
#' @param departement Un numéro de département (doit être un nombre).
#' @param output Le chemin d'enregistrement du rapport généré. Si non spécifié, une boîte de dialogue permettra à l'utilisateur de choisir l'emplacement du fichier.
#'
#' @return Cette fonction génère un fichier rapport en format HTML ou un autre format spécifié dans le fichier Quarto.
#'
#' @examples
#' # Exemple d'utilisation de la fonction
#' # Générer un rapport pour la commune 75056 et le département 75
#' generer_rapport(75056, 75, output = "chemin/vers/le/rapport.html")
#'
#' # Si l'utilisateur ne spécifie pas le chemin de sortie, une boîte de dialogue sera ouverte pour sélectionner l'emplacement du fichier
#' generer_rapport(75056, 75)
#'
#'  @export
generer_rapport <- function(commune, departement, output = NULL) {

  # 1. Valider que 'commune' et 'departement' ne soient pas vides
  if (missing(commune) || is.null(commune) || commune == "") {
    stop("Erreur : 'commune' doit être spécifié.")
  }

  if (missing(departement) || is.null(departement) || departement == "") {
    stop("Erreur : 'departement' doit être spécifié.")
  }

  # 2. Valider que 'commune' et 'departement' sont des nombres
  if (!is.numeric(commune) || !is.numeric(departement)) {
    stop("Erreur : 'commune' et 'departement' doivent être des nombres.")
  }

  # 3. Valider que 'commune' contient au moins 4 chiffres
  if (nchar(as.character(commune)) < 4) {
    stop("Erreur : 'commune' doit contenir au moins 4 chiffres.")
  }

  # 4. Si le paramètre 'output' n'est pas passé, demander à l'utilisateur de choisir l'emplacement et le nom du fichier
  if (is.null(output)) {
    output <- file.choose()  # Ouvrira une boîte de dialogue pour choisir le fichier de sortie
  }

  # 5. Localiser le fichier rapport.qmd dans le package
  rapport_path <- system.file("rapport.qmd", package = "firstlibAlex")

  # Vérifier si le fichier existe
  if (rapport_path == "") {
    stop("Le fichier rapport.qmd est introuvable dans le package.")
  }

  # 6. Compiler le rapport avec Quarto
  quarto::quarto_render(
    input = rapport_path,
    output_file = output,
    execute_params = list(
      commune = commune,
      departement = departement
    )
  )

  message("Rapport généré avec succès à: ", output)
}


