library(quarto)

# Écrivez le numéro de la commune et le département dont vous souhaitez générer un rapport

quarto_render(
  input = "rapport.qmd", # Ne pas modifier l'input car il est lié à notre fichier paramétré
  output_file = "Nom_du_fichier_a_generer.html", # Vous pouvez changer le nom du fichier et le format si besoin
  execute_params = list(code_departement="", code_commune="")
)
