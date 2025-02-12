test_that("calcul_distribution_age renvoie une distribution correcte", {
  # Création d'un jeu de données fictif
  df_test <- data.frame(
    Date.de.naissance = c("01/01/1980", "15/05/1995", "10/10/2000", "22/08/1975", "30/12/1960")
  )

  # Exécution de la fonction
  result <- calcul_distribution_age(df_test)

  # Vérifications
  expect_type(result, "integer")  # Vérifie que le résultat est bien un entier
  expect_length(result, 5)  # Vérifie que la distribution contient bien 5 valeurs (min, Q1, médiane, Q3, max)
})

test_that("calcul_distribution_age gère les valeurs manquantes", {
  df_test <- data.frame(
    Date.de.naissance = c("01/01/1980", NA, "10/10/2000", "22/08/1975", "30/12/1960")
  )

  result <- calcul_distribution_age(df_test)

  expect_true(all(!is.na(result)))  # Vérifie qu'il n'y a pas de valeurs NA dans le résultat
})
