# ============================================================
# ANALYSE DE SURVIE COMPLÈTE EN R
# Auteur : Khadidiatou NDIAYE
# Environnement : R 
# Packages : survival, survminer
# ============================================================

# ------------------------------------------------------------
# 0. INSTALLATION ET CHARGEMENT DES PACKAGES
# ------------------------------------------------------------
# À exécuter une seule fois si les packages ne sont pas installés :
# install.packages("survival")
# install.packages("survminer")

library(survival)
library(survminer)

# ------------------------------------------------------------
# 1. CHOIX DU JEU DE DONNÉES
# ------------------------------------------------------------
# Option A : jeu d'exemple intégré (pédagogique)
data(lung)
dat <- lung

# Option B : vos propres données (décommentez et adaptez)
# dat <- read.csv("chemin/vers/vos_donnees.csv", header = TRUE, sep = ";")
# Vérifiez que vos colonnes contiennent :
#   - une variable de TEMPS (numérique, en jours/mois)
#   - une variable de STATUT (0 = censuré, 1 = événement)
#   - vos variables explicatives (sexe, âge, groupe de traitement...)

# Aperçu et structure
head(dat)
str(dat)

# ------------------------------------------------------------
# 2. DÉFINITION DE L'OBJET DE SURVIE
# ------------------------------------------------------------
# ATTENTION au codage du statut selon le jeu de données :
#   - lung : status == 2 correspond au décès (1 = censuré)
#   - vos données : souvent 1 = événement, 0 = censuré
# Adaptez la ligne ci-dessous en conséquence.

dat$event <- ifelse(dat$status == 2, 1, 0)   # pour le jeu lung
# dat$event <- dat$statut                      # pour vos propres données (0/1)

# ------------------------------------------------------------
# 3. KAPLAN-MEIER + TEST DU LOG-RANK
# ------------------------------------------------------------
# Courbe de survie par sexe (adaptez la variable de groupe)
fit_km <- survfit(Surv(time, event) ~ sex, data = dat)

# Résumé statistique (médianes de survie, IC 95 %)
summary(fit_km)

# Test du Log-Rank (comparaison des courbes)
logrank <- survdiff(Surv(time, event) ~ sex, data = dat)
print(logrank)
# p-value :
p_logrank <- 1 - pchisq(logrank$chisq, df = length(logrank$n) - 1)
cat("p-value du test du Log-Rank :", format.pval(p_logrank), "\n")

# Graphique Kaplan-Meier soigné
g_km <- ggsurvplot(
  fit_km,
  data = dat,
  pval = TRUE,                 # affiche la p-value du Log-Rank
  conf.int = TRUE,             # intervalle de confiance à 95 %
  risk.table = TRUE,           # tableau des sujets à risque
  legend.labs = c("Hommes", "Femmes"),   # adaptez aux modalités
  palette = c("#E7B800", "#2E9FDF"),
  xlab = "Temps (jours)",
  ylab = "Probabilité de survie",
  title = "Courbe de survie de Kaplan-Meier",
  ggtheme = theme_minimal()
)
print(g_km)

# Export du graphique en haute résolution (PNG 300 dpi)
png("courbe_kaplan_meier.png", width = 2400, height = 1800, res = 300)
print(g_km)
dev.off()

# ------------------------------------------------------------
# 4. MODÈLE DE COX (ANALYSE MULTIVARIÉE)
# ------------------------------------------------------------
# Adaptez les covariables à votre problématique
fit_cox <- coxph(Surv(time, event) ~ sex + age + ph.ecog, data = dat)
summary(fit_cox)

# Vérification de l'hypothèse des risques proportionnels
# (important pour la validité du modèle)
test_ph <- cox.zph(fit_cox)
print(test_ph)
# Si p > 0,05 pour chaque variable : hypothèse respectée

# Forest plot des Hazard Ratios
g_forest <- ggforest(fit_cox, data = dat,
                     main = "Hazard Ratios — Modèle de Cox")
print(g_forest)

# Export du forest plot en haute résolution
png("forest_cox.png", width = 2400, height = 1800, res = 300)
print(g_forest)
dev.off()

# ------------------------------------------------------------
# 5. SAUVEGARDE DES RÉSULTATS TEXTUELS
# ------------------------------------------------------------
sink("resultats_analyse_survie.txt")
cat("=== RÉSUMÉ KAPLAN-MEIER ===\n")
print(summary(fit_km))
cat("\n=== TEST DU LOG-RANK ===\n")
print(logrank)
cat("\n=== MODÈLE DE COX ===\n")
print(summary(fit_cox))
cat("\n=== TEST DES RISQUES PROPORTIONNELS ===\n")
print(test_ph)
sink()

cat("\n Analyse terminée. Fichiers générés :\n")
cat(" - courbe_kaplan_meier.png\n")
cat(" - forest_cox.png\n")
cat(" - resultats_analyse_survie.txt\n")