test_that("plot_commune génère un graphique sans erreur pour une commune unique", {
  df <- data.frame(
    Code.du.département = c("01", "01", "01", "01"),
    Libellé.du.département = c("Ain", "Ain", "Ain", "Ain"),
    Libellé.de.la.commune = c("Lyon", "Lyon", "Lyon", "Lyon"),
    Nom.de.l.élu = c("Dupont", "Martin", "Lemoine", "Bernard"),
    Libellé.de.la.catégorie.socio.professionnelle = c("Médecin", "Ingénieur", "Enseignant", "Médecin")
  )

  expect_silent(plot_commune(df))  # Verifica que no hay errores al ejecutar
})

test_that("plot_commune génère une erreur si des colonnes sont manquantes", {
  df_incomplet <- data.frame(
    Libellé.de.la.commune = c("Lyon", "Lyon"),
    Nom.de.l.élu = c("Dupont", "Martin")
  )

  expect_error(plot_commune(df_incomplet), "Le data.frame doit contenir les colonnes nécessaires.")
})

test_that("plot_commune génère une erreur si plusieurs communes sont présentes", {
  df_multiple_communes <- data.frame(
    Libellé.de.la.commune = c("Lyon", "Lyon", "Soissons"),
    Libellé.du.département = c("Ain", "Ain", "Aisne"),
    Libellé.de.la.catégorie.socio.professionnelle = c("Médecin", "Ingénieur", "Enseignant")
  )

  expect_error(plot_commune(df_multiple_communes), "Le data.frame contient plusieurs communes. Veuillez spécifier une seule commune.")
})
