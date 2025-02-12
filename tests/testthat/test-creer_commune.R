test_that("creer_commune filtre correctement les données", {
  df <- data.frame(
    Code.du.département = c("01", "01", "02", "02"),
    Libellé.du.département = c("Ain", "Ain", "Aisne", "Aisne"),
    Libellé.de.la.commune = c("Lyon", "Lyon", "Soissons", "Soissons"),
    Nom.de.l.élu = c("Dupont", "Martin", "Lemoine", "Dubois")
  )

  result <- creer_commune(df, "Lyon")

  expect_s3_class(result, "commune")  # Verifica que la clase es 'commune'
  expect_equal(nrow(result), 2)  # Solo debe haber 2 filas para "Lyon"
  expect_true(all(result$Libellé.de.la.commune == "Lyon"))  # Todas las filas deben ser de "Lyon"
})

test_that("creer_commune retourne un data.frame vide si la commune n'existe pas", {
  df <- data.frame(
    Code.du.département = c("01", "01", "02", "02"),
    Libellé.de.la.commune = c("Lyon", "Lyon", "Soissons", "Soissons")
  )

  result <- creer_commune(df, "Paris")

  expect_s3_class(result, "commune")  # Sigue siendo de clase 'commune'
  expect_equal(nrow(result), 0)  # Debe estar vacío
})

test_that("creer_commune génère une erreur si la colonne 'Libellé.de.la.commune' est absente", {
  df <- data.frame(
    Code.du.département = c("01", "02"),
    Nom.de.l.élu = c("Dupont", "Lemoine")
  )

  expect_error(creer_commune(df, "Lyon"), "Le data.frame doit contenir la colonne 'Libellé.de.la.commune'")
})
