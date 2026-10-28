
# ==============================================================
# ANALYSE EPIDEMIOLOGIQUE D'EBOLA - NORD-KIVU ET ITURI
# Analyse par zone de sante, groupe d'age et sexe
# DONNEES SIMULEES : NE PAS UTILISER COMME STATISTIQUES REELLES
# ==============================================================

# 1. Installer et charger les bibliotheques necessaires
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# 2. Creer le jeu de donnees agrege et fictif
# Aucun nom, telephone ou identifiant individuel n'est utilise.
# Les effectifs servent uniquement a apprendre l'analyse sous R.

donnees <- data.frame(
  Province = c(
    "Ituri", "Ituri", "Ituri", "Ituri",
    "Ituri", "Ituri", "Ituri", "Ituri",
    "Nord-Kivu", "Nord-Kivu", "Nord-Kivu", "Nord-Kivu",
    "Nord-Kivu", "Nord-Kivu", "Nord-Kivu", "Nord-Kivu"
  ),

  Zone_sante = c(
    "Zone A", "Zone A", "Zone A", "Zone A",
    "Zone B", "Zone B", "Zone B", "Zone B",
    "Zone C", "Zone C", "Zone C", "Zone C",
    "Zone D", "Zone D", "Zone D", "Zone D"
  ),

  Groupe_age = c(
    "0-4 ans", "0-4 ans", "15 ans et +", "15 ans et +",
    "5-14 ans", "5-14 ans", "15 ans et +", "15 ans et +",
    "0-4 ans", "0-4 ans", "15 ans et +", "15 ans et +",
    "5-14 ans", "5-14 ans", "15 ans et +", "15 ans et +"
  ),

  Sexe = c(
    "Femme", "Homme", "Femme", "Homme",
    "Femme", "Homme", "Femme", "Homme",
    "Femme", "Homme", "Femme", "Homme",
    "Femme", "Homme", "Femme", "Homme"
  ),

  Cas = c(4, 5, 12, 18, 6, 7, 15, 20,
          3, 4, 8, 12, 4, 5, 10, 14),

  Deces = c(1, 2, 5, 8, 2, 3, 6, 9,
            1, 2, 3, 5, 2, 2, 4, 7)
)

# 3. Calculer le taux de letalite pour chaque groupe
# Formule : (deces / cas) x 100
donnees$Letalite <- round(
  donnees$Deces / donnees$Cas * 100, 1
)

# 4. Afficher le tableau des donnees
cat("\n===== DONNEES EPIDEMIOLOGIQUES =====\n")
print(donnees)

# 5. ANALYSE PAR PROVINCE
# Total des cas et des deces dans chaque province
par_province <- aggregate(
  cbind(Cas, Deces) ~ Province,
  data = donnees,
  FUN = sum
)

par_province$Letalite <- round(
  par_province$Deces / par_province$Cas * 100, 1
)

cat("\n===== RESULTATS PAR PROVINCE =====\n")
print(par_province)

# 6. ANALYSE PAR ZONE DE SANTE
# Comparer le nombre de cas dans chaque zone
par_zone <- aggregate(
  Cas ~ Province + Zone_sante,
  data = donnees,
  FUN = sum
)

graphique_zone <- ggplot(
  par_zone,
  aes(x = Zone_sante, y = Cas, fill = Province)
) +
  geom_col(position = "dodge") +
  labs(
    title = "Repartition des cas par zone de sante",
    subtitle = "Donnees simulees",
    x = "Zone de sante",
    y = "Nombre de cas",
    fill = "Province"
  ) +
  theme_minimal()

print(graphique_zone)

# 7. ANALYSE PAR GROUPE D'AGE
# Comparer les cas chez les enfants et les adultes
par_age <- aggregate(
  Cas ~ Groupe_age,
  data = donnees,
  FUN = sum
)

graphique_age <- ggplot(
  par_age,
  aes(x = Groupe_age, y = Cas, fill = Groupe_age)
) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = Cas), vjust = -0.4) +
  labs(
    title = "Repartition des cas par groupe d'age",
    subtitle = "Donnees simulees",
    x = "Groupe d'age",
    y = "Nombre de cas"
  ) +
  theme_minimal()

print(graphique_age)

# 8. ANALYSE PAR SEXE
# Comparer les effectifs masculins et feminins
par_sexe <- aggregate(
  Cas ~ Sexe,
  data = donnees,
  FUN = sum
)

par_sexe$Pourcentage <- round(
  par_sexe$Cas / sum(par_sexe$Cas) * 100, 1
)

graphique_sexe <- ggplot(
  par_sexe,
  aes(x = Sexe, y = Cas, fill = Sexe)
) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = paste0(Cas, " cas (", Pourcentage, "%)")),
    vjust = -0.4
  ) +
  labs(
    title = "Repartition des cas selon le sexe",
    subtitle = "Donnees simulees",
    x = "Sexe",
    y = "Nombre de cas"
  ) +
  theme_minimal()

print(graphique_sexe)

# 9. EXPORTER LES TABLEAUX POUR EXCEL OU POWER BI
write.csv(
  donnees,
  "donnees_epidemiologiques.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  par_province,
  "synthese_par_province.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\nAnalyse terminee avec succes !\n")
cat("Les tableaux CSV sont enregistres dans le repertoire de travail.\n")
