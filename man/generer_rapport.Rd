% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generer_rapport.R
\name{generer_rapport}
\alias{generer_rapport}
\title{Générer un rapport basé sur les élections françaises de 2024}
\usage{
generer_rapport(code_commune, code_departement, output = NULL)
}
\arguments{
\item{code_commune}{Un numéro de commune (doit être un nombre à 4 chiffres minimum).}

\item{code_departement}{Un numéro de département (doit être un nombre).}

\item{output}{Le chemin d'enregistrement du rapport généré. Si non spécifié, une boîte de dialogue permettra à l'utilisateur de choisir l'emplacement du fichier.}
}
\value{
Cette fonction génère un fichier rapport en format HTML ou un autre format spécifié dans le fichier Quarto.
}
\description{
Cette fonction génère un rapport en utilisant un fichier Quarto (.qmd) pour analyser les caractéristiques des élus lors des élections françaises de 2024, en se basant sur les paramètres 'code_commune' et 'code_departement'.
Le rapport sera rendu dans le fichier spécifié par l'utilisateur ou choisi via une boîte de dialogue si le chemin de sortie n'est pas précisé.
}
\examples{
# Exemple d'utilisation de la fonction
# Générer un rapport pour la commune 75056 et le département 75
generer_rapport(1001, 1, output = "chemin/vers/le/rapport.html")

# Si l'utilisateur ne spécifie pas le chemin de sortie, une boîte de dialogue sera ouverte pour sélectionner l'emplacement du fichier
generer_rapport(1001, 1)
}
