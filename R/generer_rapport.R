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

  # 1. Valider que 'code_commune' et 'code_departement' ne soient pas vides
  if (missing(code_commune) || is.null(code_commune) || code_commune == "") {
    stop("Erreur : 'code_commune' doit être spécifié.")
  }

  if (missing(code_departement) || is.null(code_departement) || code_departement == "") {
    stop("Erreur : 'code_departement' doit être spécifié.")
  }

  # 2. Valider que 'code_commune' et 'code_departement' sont des nombres
  if (!is.numeric(code_commune) || !is.numeric(code_departement)) {
    stop("Erreur : 'code_commune' et 'code_departement' doivent être des nombres.")
  }

  # 3. Valider que 'code_commune' contient au moins 4 chiffres
  if (nchar(as.character(code_commune)) < 4) {
    stop("Erreur : 'code_commune' doit contenir au moins 4 chiffres.")
  }

  # 4. Si el parámetro 'output' no es pasado, pedir al usuario que elija la ruta y nombre del archivo
  if (is.null(output)) {
    output <- file.choose()  # Abrir un cuadro de diálogo para seleccionar la ruta del archivo de salida
  } else {
    # Asegurarse de que la ruta tenga la extensión '.html'
    if (!grepl("\\.html$", output)) {
      stop("Erreur : le fichier de sortie doit avoir une extension '.html'.")
    }
  }

  # 5. Asegurarse de que el directorio de salida existe
  output_dir <- dirname(output)  # Obtener el directorio de la ruta de salida
  if (!dir.exists(output_dir)) {
    stop("Erreur : Le dossier spécifié n'existe pas.")
  }

  # 6. Localizar el archivo 'rapport.qmd' dentro del paquete
  rapport_path <- system.file("rapport.qmd", package = "firstlibAlex")

  # Verificar si el archivo existe
  if (rapport_path == "") {
    stop("Le fichier rapport.qmd est introuvable dans le package.")
  }

  # Crear variables para los números de la comuna y el departamento
  numero_commune <- code_commune
  numero_departement <- code_departement

  # Mostrar los números de comuna y departamento elegidos
  message("Vous avez choisi le numéro de commune: ", numero_commune)
  message("Vous avez choisi le numéro de département: ", numero_departement)

  # 7. Compilar el informe con Quarto
  quarto::quarto_render(
    input = rapport_path,  # El archivo de entrada que está parametrizado
    output_file = basename(output),  # Usar solo el nombre del archivo sin la ruta completa
    output_dir = output_dir,  # Establecer el directorio de salida
    execute_params = list(
      code_commune = numero_commune,  # Usar las variables de número de comuna y departamento
      code_departement = numero_departement
    )
  )

  # Abrir el archivo generado en el navegador por defecto
  browseURL(output)

  # Mostrar un mensaje confirmando el éxito de la generación
  message("Rapport généré avec succès à: ", output)
}
