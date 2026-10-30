# ============================================================
# COMPARAISON DES MODÈLES DE SURVIE :
# COX (semi-paramétrique) vs WEIBULL (paramétrique)
# Auteur : Khadija Ndiaye
# Date : Octobre 2026
# ============================================================

# --- 0. Chargement des packages ---
library(survival)
library(survminer)
library(ggplot2)

# --- 1. Chargement du jeu de données ---
data(lung)
dat <- lung

# Recodage du statut (dans 'lung', status == 2 signifie décès)
dat$event <- ifelse(dat$status == 2, 1, 0)

# --- 2. Ajustement des deux modèles ---

# 2a. Modèle de Cox (semi-paramétrique)
fit_cox <- coxph(Surv(time, event) ~ sex + age, data = dat)

# 2b. Modèle de Weibull (paramétrique)
fit_weibull <- survreg(Surv(time, event) ~ sex + age, 
                       data = dat, dist = "weibull")

# --- 3. Comparaison par AIC ---
AIC_cox <- AIC(fit_cox)
AIC_weibull <- AIC(fit_weibull)

cat("\n=====================================\n")
cat("COMPARAISON DES MODÈLES (AIC)\n")
cat("=====================================\n")
cat("AIC du modèle de Cox    :", round(AIC_cox, 2), "\n")
cat("AIC du modèle de Weibull:", round(AIC_weibull, 2), "\n")
cat("(Le plus petit AIC = meilleur ajustement)\n\n")

# --- 4. Prédiction pour un profil type ---
# Homme (sex = 1) de 65 ans
nouvel_individu <- data.frame(sex = 1, age = 65)

# Grille de temps pour tracer
times <- seq(0, max(dat$time), length.out = 200)

# Courbe de survie Cox (inchangée)
surv_cox <- survfit(fit_cox, newdata = nouvel_individu)
surv_cox_df <- data.frame(
  time = surv_cox$time,
  surv = surv_cox$surv,
  modele = "Cox (semi-paramétrique)"
)

# Courbe de survie Weibull
# On calcule le prédicteur linéaire, puis l'échelle (scale) avec exp()
lp <- predict(fit_weibull, newdata = nouvel_individu, type = "lp")
scale_param <- exp(lp)
shape_param <- 1 / fit_weibull$scale

surv_weibull_prob <- pweibull(
  times,
  shape = shape_param,
  scale = scale_param,
  lower.tail = FALSE
)
surv_weibull_df <- data.frame(
  time = times,
  surv = surv_weibull_prob,
  modele = "Weibull (paramétrique)"
)

# --- 5. Graphique de comparaison avec ggplot2 ---
df_plot <- rbind(surv_cox_df, surv_weibull_df)

g_compare <- ggplot(df_plot, aes(x = time, y = surv, color = modele, linetype = modele)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Cox (semi-paramétrique)" = "#2E9FDF", 
                                "Weibull (paramétrique)" = "#E7B800")) +
  scale_linetype_manual(values = c("Cox (semi-paramétrique)" = "solid",
                                   "Weibull (paramétrique)" = "dashed")) +
  labs(
    title = "Comparaison Cox vs Weibull",
    subtitle = "Profil type : Homme de 65 ans (jeu de données 'lung')",
    x = "Temps (jours)",
    y = "Probabilité de survie",
    color = "Modèle",
    linetype = "Modèle"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom"
  )

print(g_compare)

# Sauvegarde de la version corrigée
ggsave("comparaison_cox_weibull.png", 
       plot = g_compare, 
       width = 10, height = 7, dpi = 300)

