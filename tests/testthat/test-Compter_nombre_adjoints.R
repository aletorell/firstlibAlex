# tests/testthat/test_compter_nombre_d_adjoints.R

test_that("compter_nombre_d_adjoints retourne un entier positif", {
  data_test <- data.frame(Libellé.de.la.fonction = c(
    "1er adjoint au Maire", "2ème adjoint au Maire", "3ème adjoint au Maire",
    "Maire", "Conseiller municipal", "1er adjoint au Maire"
  ))

  result <- compter_nombre_d_adjoints(data_test)

  expect_type(result, "integer")
  expect_gte(result, 0)  # Vérifie que le résultat est positif
})

test_that("compter_nombre_d_adjoints gère les valeurs manquantes correctement", {
  data_test <- data.frame(Libellé.de.la.fonction = c(NA, NA, "Maire"))

  result <- compter_nombre_d_adjoints(data_test)

  expect_equal(result, 0)  # Aucune correspondance ne doit donner 0
})

test_that("compter_nombre_d_adjoints renvoie une erreur si colonne manquante", {
  data_test <- data.frame(Autre_colonne = c("1er adjoint au Maire", "Maire"))

  expect_error(compter_nombre_d_adjoints(data_test),
               "Le data.frame doit contenir la colonne 'Libellé.de.la.fonction'.")
})
