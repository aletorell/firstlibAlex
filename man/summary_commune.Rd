% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Summary_commune.R
\name{summary_commune}
\alias{summary_commune}
\title{summary_commune}
\usage{
summary_commune(x)
}
\arguments{
\item{x}{Un data.frame contenant les informations des élus d'une commune. Il doit avoir les colonnes suivantes :
\itemize{
\item "Libellé.de.la.commune" : Le nom de la commune.
\item "Nom.de.l.élu" : Le nom de l'élu.
\item "Prénom.de.l.élu" : Le prénom de l'élu.
\item "Date.de.naissance" : La date de naissance de l'élu (au format "jour/mois/année").
}}
}
\value{
Aucune valeur n'est retournée. La fonction imprime les résultats dans la console.
}
\description{
Cette fonction génère un résumé statistique pour une commune spécifique, y compris les informations
sur les élu.e.s de la commune : nombre d'élus, distribution des âges et informations sur l'élu.e le plus âgé.e.
}
\details{
Cette fonction génère les résultats suivants pour une commune donnée :
\itemize{
\item Le nom de la commune.
\item Le nombre d'élus dans la commune.
\item La distribution des âges des élus dans la commune.
\item Le nom, le prénom et l'âge de l'élu.e le plus âgé(e).
}

La fonction vérifie également que toutes les colonnes nécessaires sont présentes et que les dates de naissance
sont au format correct. En cas d'erreur, elle génère un message d'arrêt.
}
\examples{
# Exemple d'utilisation avec le dataset fourni
df_Nantes <- data.frame(
  `Libellé.de.la.commune` = c("Nantes", "Nantes", "Nantes"),
  `Nom.de.l.élu` = c("Dupont", "Martin", "Bernard"),
  `Prénom.de.l.élu` = c("Pierre", "Jacques", "Lucie"),
  `Date.de.naissance` = c("01/01/1970", "15/03/1980", "23/06/1990")
)
summary_commune(df_Nantes)

}
