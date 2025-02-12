test_that("plot_departement génère un graphique sans erreur pour un département unique", {
  df <- data.frame(
    Code.du.département = c("01", "01", "01", "01"),
    Libellé.du.département = c("Ain", "Ain", "Ain", "Ain"),
    Libellé.de.la.commune = c("Lyon", "Lyon", "Lyon", "Lyon"),
    Nom.de.l.élu = c("Dupont", "Martin", "Lemoine", "Bernard"),
    Libellé.de.la.catégorie.socio.professionnelle = c("Médecin", "Ingénieur", "Enseignant", "Médecin")
  )

  expect_silent(plot_departement(df))  # Verifica que no hay errores al ejecutar
})

test_that("plot_departement génère une erreur si des colonnes sont manquantes", {
  df_incomplet <- data.frame(
    Libellé.du.département = c("Ain", "Ain"),
    Libellé.de.la.commune = c("Lyon", "Lyon"),
    Nom.de.l.élu = c("Dupont", "Martin")
  )

  expect_error(plot_departement(df_incomplet), "Le data.frame doit contenir les colonnes nécessaires.")
})

test_that("plot_departement génère une erreur si plusieurs départements sont présents", {
  df_multiple_departements <- data.frame(
    Libellé.du.département = c("Ain", "Ain", "Aisne"),
    Libellé.de.la.commune = c("Lyon", "Lyon", "Soissons"),
    Libellé.de.la.catégorie.socio.professionnelle = c("Médecin", "Ingénieur", "Enseignant")
  )

  expect_error(plot_departement(df_multiple_departements), "Le data.frame contient plusieurs départements. Veuillez spécifier un seul département.")
})
