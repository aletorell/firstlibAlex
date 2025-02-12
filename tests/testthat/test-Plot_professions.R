test_that("plot_code_professions génère un graphique sans erreur pour un data.frame valide", {
  df <- data.frame(
    Libellé.de.la.catégorie.socio.professionnelle = c("Enseignant", "Médecin", "Ingénieur", "Médecin", "Enseignant")
  )

  expect_silent(plot_code_professions(df))  # Vérifie que la fonction ne génère pas d'erreur
})

test_that("plot_code_professions génère une erreur si la colonne est manquante", {
  df_incomplet <- data.frame(
    Libellé.de.la.commune = c("Lyon", "Paris", "Marseille")
  )

  expect_error(plot_code_professions(df_incomplet), "Le data.frame doit contenir la colonne 'Libellé.de.la.catégorie.socio.professionnelle'.")
})

test_that("plot_code_professions gère correctement un seul type de profession", {
  df_single_profession <- data.frame(
    Libellé.de.la.catégorie.socio.professionnelle = c("Médecin", "Médecin", "Médecin")
  )

  expect_silent(plot_code_professions(df_single_profession))  # Vérifie que la fonction ne génère pas d'erreur
})

test_that("plot_code_professions gère un data.frame avec plusieurs professions", {
  df_multiple_professions <- data.frame(
    Libellé.de.la.catégorie.socio.professionnelle = c("Médecin", "Ingénieur", "Médecin", "Enseignant", "Médecin", "Ingénieur")
  )

  expect_silent(plot_code_professions(df_multiple_professions))  # Vérifie que la fonction ne génère pas d'erreur
})
