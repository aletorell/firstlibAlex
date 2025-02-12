library(testthat)

# Test simple pour la fonction summary_commune
test_that("summary_commune fonctionne avec une commune valide", {

  # Création d'un jeu de données pour Nantes
  df_Nantes <- data.frame(
    `Libellé.de.la.commune` = c("Nantes", "Nantes", "Nantes"),
    `Nom.de.l.élu` = c("Dupont", "Martin", "Bernard"),
    `Prénom.de.l.élu` = c("Pierre", "Jacques", "Lucie"),
    `Date.de.naissance` = c("01/01/1970", "15/03/1980", "23/06/1990")
  )

  # On capture la sortie de la fonction pour vérifier si le texte attendue est imprimé
  output <- capture.output(summary_commune(df_Nantes))

  # Vérification que l'affichage contient les informations attendues
  expect_true(any(grepl("Commune:", output)))
  expect_true(any(grepl("Nombre d'élu.e.s:", output)))
  expect_true(any(grepl("Distribution des âges:", output)))
})
test_that("summary_commune fonctionne avec un jeu de données minimal", {

  # Création d'un jeu de données minimal pour une commune avec un seul élu
  df_Single <- data.frame(
    `Libellé.de.la.commune` = c("SingleCommune"),
    `Nom.de.l.élu` = c("Martin"),
    `Prénom.de.l.élu` = c("Jacques"),
    `Date.de.naissance` = c("15/03/1980")
  )

  # On capture la sortie de la fonction pour vérifier si le texte attendu est imprimé
  output <- capture.output(summary_commune(df_Single))

  # Vérification que l'affichage contient les informations attendues
  expect_true(any(grepl("Commune:", output)))
  expect_true(any(grepl("Nombre d'élu.e.s:", output)))
  expect_true(any(grepl("Distribution des âges:", output)))

  # Vérification que le nombre d'élus est correct (1 dans ce cas)
  expect_true(any(grepl("Nombre d'élu.e.s: 1", output)))
})

