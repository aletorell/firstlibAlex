library(testthat)

test_that("Summary_departement fonctionne avec un data.frame valide", {
  # Création d'un data.frame valide pour un département
  df_valid <- data.frame(
    `Code.du.département` = c("01", "01", "01"),
    `Libellé.du.département` = c("Ain", "Ain", "Ain"),
    `Libellé.de.la.commune` = c("Bourg-en-Bresse", "Ambérieu-en-Bugey", "Lagnieu"),
    `Nom.de.l.élu` = c("Dupont", "Martin", "Bernard"),
    `Prénom.de.l.élu` = c("Pierre", "Jacques", "Lucie"),
    `Date.de.naissance` = c("01/01/1970", "15/03/1980", "23/06/1990")
  )

  # Test de la fonction avec ce data.frame
  expect_output(Summary_departement(df_valid), "Nom du département: Ain")
  expect_output(Summary_departement(df_valid), "Nombre de communes: 3")
  expect_output(Summary_departement(df_valid), "Nombre d'élu.e.s: 3")
})

test_that("Summary_departement lance une erreur pour des données invalides", {
  # Création d'un data.frame manquant une colonne obligatoire
  df_invalid <- data.frame(
    `Code.du.département` = c("01", "01", "01"),
    `Libellé.du.département` = c("Ain", "Ain", "Ain"),
    `Libellé.de.la.commune` = c("Bourg-en-Bresse", "Ambérieu-en-Bugey", "Lagnieu"),
    `Nom.de.l.élu` = c("Dupont", "Martin", "Bernard"),
    `Prénom.de.l.élu` = c("Pierre", "Jacques", "Lucie")
    # Il manque la colonne 'Date.de.naissance'
  )

  # Test de l'erreur pour colonnes manquantes
  expect_error(Summary_departement(df_invalid), "Le data.frame doit contenir les colonnes nécessaires.")

  # Création d'un data.frame avec plus d'un département
  df_multiple_departments <- data.frame(
    `Code.du.département` = c("01", "02", "01"),
    `Libellé.du.département` = c("Ain", "Aude", "Ain"),
    `Libellé.de.la.commune` = c("Bourg-en-Bresse", "Narbonne", "Lagnieu"),
    `Nom.de.l.élu` = c("Dupont", "Martin", "Bernard"),
    `Prénom.de.l.élu` = c("Pierre", "Jacques", "Lucie"),
    `Date.de.naissance` = c("01/01/1970", "15/03/1980", "23/06/1990")
  )

  # Test de l'erreur pour plusieurs départements
  expect_error(Summary_departement(df_multiple_departments), "L'objet doit contenir les données d'un seul département.")
})
