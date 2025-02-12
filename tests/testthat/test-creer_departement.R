test_that("creer_departement filtre correctement les données", {
  df <- data.frame(
    Code.du.département = c("01", "01", "02", "02"),
    Libellé.du.département = c("Ain", "Ain", "Aisne", "Aisne"),
    Libellé.de.la.commune = c("Lyon", "Lyon", "Soissons", "Soissons"),
    Nom.de.l.élu = c("Dupont", "Martin", "Lemoine", "Dubois")
  )

  result <- creer_departement(df, "Ain")

  expect_s3_class(result, "departement")  # Verifica que la clase es 'departement'
  expect_equal(nrow(result), 2)  # Solo debe haber 2 filas para "Ain"
  expect_true(all(result$Libellé.du.département == "Ain"))  # Todas las filas deben ser de "Ain"
})

test_that("creer_departement retourne un data.frame vide si le département n'existe pas", {
  df <- data.frame(
    Code.du.département = c("01", "01", "02", "02"),
    Libellé.du.département = c("Ain", "Ain", "Aisne", "Aisne")
  )

  result <- creer_departement(df, "Paris")

  expect_s3_class(result, "departement")  # Debe seguir siendo un 'departement'
  expect_equal(nrow(result), 0)  # Debe estar vacío
})

test_that("creer_departement génère une erreur si la colonne 'Libellé.du.département' est absente", {
  df <- data.frame(
    Code.du.département = c("01", "02"),
    Nom.de.l.élu = c("Dupont", "Lemoine")
  )

  expect_error(creer_departement(df, "Ain"), "Le data.frame doit contenir la colonne 'Libellé.du.département'")
})
