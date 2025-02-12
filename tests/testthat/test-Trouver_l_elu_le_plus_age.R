test_that("trouver_l_elu_le_plus_age fonctionne avec un data.frame valide", {
  # Création du data.frame d'exemple
  data <- data.frame(
    Nom.de.l.élu = c("Dupont", "Lemoine", "Dufresne"),
    Prénom.de.l.élu = c("Pierre", "Sophie", "Marc"),
    Date.de.naissance = c("01/01/1970", "15/05/1980", "23/07/1965")
  )

  # Appliquer la fonction
  result <- trouver_l_elu_le_plus_age(data)

  # Vérifier que l'élu le plus âgé est bien Dufresne (né en 1965)
  expect_equal(result$Nom.de.l.élu, "Dufresne")
  expect_equal(result$Prénom.de.l.élu, "Marc")
  expect_equal(result$Age, 59)  # En supposant que l'année actuelle soit 2024
})

test_that("trouver_l_elu_le_plus_age échoue si le data.frame n'a pas les bonnes colonnes", {
  # Création du data.frame sans la colonne Date.de.naissance
  data <- data.frame(
    Nom.de.l.élu = c("Dupont", "Lemoine", "Dufresne"),
    Prénom.de.l.élu = c("Pierre", "Sophie", "Marc")
  )

  # Tester que la fonction lance une erreur
  expect_error(trouver_l_elu_le_plus_age(data),
               "Le data.frame doit contenir les colonnes 'Nom.de.l.élu', 'Prénom.de.l.élu' et 'Date.de.naissance'")
})
