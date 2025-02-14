#' Générer un rapport basé sur les élections françaises de 2024
#'
#' Cette fonction génère un rapport en utilisant un fichier Quarto (.qmd) pour analyser les caractéristiques des élus lors des élections françaises de 2024, en se basant sur les paramètres 'code_commune' et 'code_departement'.
#' Le rapport sera rendu dans le fichier spécifié par l'utilisateur ou choisi via une boîte de dialogue si le chemin de sortie n'est pas précisé.
#'
#' @param code_commune Un numéro de commune (doit être un nombre à 4 chiffres minimum).
#' @param code_departement Un numéro de département (doit être un nombre).
#' @param output Le chemin d'enregistrement du rapport généré. Si non spécifié, une boîte de dialogue permettra à l'utilisateur de choisir l'emplacement du fichier.
#'
#' @import quarto
#' @import utils
#'
#' @return Cette fonction génère un fichier rapport en format HTML ou un autre format spécifié dans le fichier Quarto.
#' @export
#'
#' @examples
#' # Exemple d'utilisation de la fonction
#' # Générer un rapport pour la commune 75056 et le département 75
#' generer_rapport(1001, 1, output = "chemin/vers/le/rapport.html")
#'
#' # Si l'utilisateur ne spécifie pas le chemin de sortie, une boîte de dialogue sera ouverte pour sélectionner l'emplacement du fichier
#' generer_rapport(1001, 1)
generer_rapport <- function(code_commune, code_departement, output = NULL) {

  # Validación de los parámetros
  if (missing(code_commune) || is.null(code_commune) || code_commune == "") {
    stop("Erreur : 'code_commune' doit être spécifié.")
  }

  if (missing(code_departement) || is.null(code_departement) || code_departement == "") {
    stop("Erreur : 'code_departement' doit être spécifié.")
  }

  if (!is.numeric(code_commune) || !is.numeric(code_departement)) {
    stop("Erreur : 'code_commune' et 'code_departement' doivent être des nombres.")
  }

  if (nchar(as.character(code_commune)) < 4) {
    stop("Erreur : 'code_commune' doit contenir au moins 4 chiffres.")
  }

  # Si 'output' no está especificado, se pedirá al usuario que elija la ruta y nombre del archivo
  if (is.null(output)) {
    output <- file.choose()  # Selección de archivo a través de un diálogo
  } else {
    # Comprobación de que la extensión es '.html'
    if (!grepl("\\.html$", output)) {
      stop("Erreur : Le fichier de sortie doit avoir l'extension '.html'.")
    }
  }

  # Localizar el archivo 'rapport.qmd' en el paquete
  rapport_path <- system.file("rapport.qmd", package = "firstlibAlex")

  # Verificación de la existencia del archivo
  if (rapport_path == "") {
    stop("Le fichier rapport.qmd est introuvable dans le package.")
  }

  # Variables para el código de la comuna y del departamento
  numero_commune <- code_commune
  numero_departement <- code_departement

  # Mensajes de información
  message("Vous avez choisi le numéro de commune: ", numero_commune)
  message("Vous avez choisi le numéro de département: ", numero_departement)

  # Cambiar directorio de trabajo al directorio del archivo de salida (para que todo se guarde allí)
  output_dir <- dirname(output)
  setwd(output_dir)

  # Compilación del informe con Quarto
  quarto::quarto_render(
    input = rapport_path,
    output_file = basename(output),  # Guardar solo el nombre de archivo, no la ruta completa
    execute_params = list(
      code_commune = numero_commune,
      code_departement = numero_departement
    )
  )

  # Abrir el archivo generado en el navegador por defecto
  browseURL(output)

  # Confirmación de éxito
  message("Rapport généré avec succès à: ", output)
}
