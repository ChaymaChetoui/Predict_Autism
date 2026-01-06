# ============================================================================
# VISUALISATIONS AMÉLIORÉES - PROJET TSA
# ============================================================================

# Packages supplémentaires pour de meilleures visualisations
packages <- c("caret", "xgboost", "pROC", "randomForest", "tidyverse",
              "corrplot", "RColorBrewer", "gridExtra", "glmnet",
              "ggplot2", "scales", "viridis", "patchwork", "ggthemes",
              "plotly", "GGally", "ggsci", "ggridges")

installed <- packages %in% rownames(installed.packages())
if(any(!installed)) {
  install.packages(packages[!installed])
}

library(caret)
library(xgboost)
library(pROC)
library(randomForest)
library(tidyverse)
library(corrplot)
library(RColorBrewer)
library(gridExtra)
library(glmnet)
library(scales)
library(viridis)
library(patchwork)
library(ggthemes)
library(ggsci)
library(ggridges)

# Thème personnalisé pour tous les graphiques
theme_custom <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#2c3e50"),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#7f8c8d", 
                                   margin = ggplot2::margin(b = 15)),  # Fix conflit
      plot.caption = element_text(size = 10, color = "#95a5a6", hjust = 1),
      axis.title = element_text(face = "bold", size = 12, color = "#34495e"),
      axis.text = element_text(size = 11, color = "#2c3e50"),
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 11),
      legend.position = "right",
      panel.grid.major = element_line(color = "#ecf0f1", size = 0.5),
      panel.grid.minor = element_line(color = "#ecf0f1", size = 0.25),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      strip.text = element_text(face = "bold", size = 12)
    )
}

# Palette de couleurs personnalisée
colors_custom <- c(
  "NO" = "#3498db",   # Bleu
  "YES" = "#e74c3c",  # Rouge
  "primary" = "#2ecc71",
  "secondary" = "#f39c12",
  "accent" = "#9b59b6"
)

# ============================================================================
# CHARGEMENT ET PRÉPARATION DES DONNÉES (votre code existant)
# ============================================================================

data <- read.csv("data/results.csv", stringsAsFactors = FALSE)
data_clean <- data

# Corrections
names(data_clean)[names(data_clean) == "jundice"] <- "jaundice"
names(data_clean)[names(data_clean) == "austim"] <- "autism"
names(data_clean)[names(data_clean) == "contry_of_res"] <- "country_of_res"

data_clean <- data_clean[!duplicated(data_clean), ]
data_clean$age <- as.numeric(data_clean$age)
if(sum(is.na(data_clean$age)) > 0) {
  data_clean$age[is.na(data_clean$age)] <- median(data_clean$age, na.rm = TRUE)
}
data_clean[data_clean == "?"] <- NA

score_cols <- paste0("A", 1:10, "_Score")
data_clean$total_score <- rowSums(data_clean[, score_cols])
data_clean <- data_clean %>%
  mutate(across(where(is.character), ~str_replace_all(., "'", "")))

# Feature Engineering
data_clean <- data_clean %>%
  mutate(
    social_score = A1_Score + A2_Score + A3_Score,
    attention_score = A4_Score + A5_Score + A6_Score,
    communication_score = A7_Score + A8_Score,
    imagination_score = A9_Score + A10_Score,
    high_risk = ifelse(total_score >= 6, 1, 0),
    has_family_history = ifelse(autism == "yes", 1, 0),
    had_jaundice = ifelse(jaundice == "yes", 1, 0)
  )

# Créer répertoire
if(!dir.exists("outputs/figures")) {
  dir.create("outputs/figures", recursive = TRUE)
}

cat("=== GÉNÉRATION DE VISUALISATIONS AMÉLIORÉES ===\n\n")

# ============================================================================
# VIZ 1: DISTRIBUTION DES SCORES - VERSION AMÉLIORÉE
# ============================================================================

cat("📊 Viz 1: Distribution des scores...\n")

# Statistiques pour annotations
stats_summary <- data_clean %>%
  group_by(Class.ASD) %>%
  summarise(
    mean = mean(total_score),
    median = median(total_score),
    sd = sd(total_score),
    n = n()
  )

p1 <- ggplot(data_clean, aes(x = Class.ASD, y = total_score, fill = Class.ASD)) +
  # Violin plot pour voir la distribution
  geom_violin(alpha = 0.3, trim = FALSE) +
  # Boxplot par-dessus
  geom_boxplot(width = 0.2, alpha = 0.7, outlier.shape = 21, 
               outlier.size = 2, outlier.color = "black") +
  # Points individuels avec jitter
  geom_jitter(width = 0.15, alpha = 0.4, size = 2, shape = 16) +
  # Ligne de seuil clinique
  geom_hline(yintercept = 6, linetype = "dashed", color = "#e74c3c", 
             size = 1, alpha = 0.7) +
  annotate("text", x = 1.5, y = 6.3, label = "Seuil clinique = 6", 
           color = "#e74c3c", fontface = "bold", size = 4) +
  # Moyennes
  stat_summary(fun = mean, geom = "point", shape = 23, size = 5, 
               fill = "#f39c12", color = "black", stroke = 1.5) +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 1)),
               vjust = -1.5, size = 4.5, fontface = "bold", color = "#f39c12") +
  # Annotations avec stats
  annotate("text", x = 1, y = 10, 
           label = sprintf("n = %d\nµ = %.1f\nσ = %.1f", 
                           stats_summary$n[1], 
                           stats_summary$mean[1], 
                           stats_summary$sd[1]),
           size = 3.5, hjust = 0.5, color = "#3498db", fontface = "bold") +
  annotate("text", x = 2, y = 10, 
           label = sprintf("n = %d\nµ = %.1f\nσ = %.1f", 
                           stats_summary$n[2], 
                           stats_summary$mean[2], 
                           stats_summary$sd[2]),
           size = 3.5, hjust = 0.5, color = "#e74c3c", fontface = "bold") +
  # Échelles et labels
  scale_fill_manual(values = colors_custom, name = "Diagnostic") +
  scale_y_continuous(breaks = seq(0, 10, 1), limits = c(-0.5, 10.5)) +
  labs(
    title = "Distribution des Scores AQ-10 par Diagnostic",
    subtitle = sprintf("Analyse de %d enfants | Losange jaune = moyenne", nrow(data_clean)),
    x = "Diagnostic TSA",
    y = "Score Total AQ-10 (0-10)",
    caption = "Chaque point représente un enfant | Ligne rouge = seuil clinique"
  ) +
  theme_custom() +
  theme(legend.position = "none")

ggsave("outputs/figures/scores_distribution_enhanced.png", p1, 
       width = 12, height = 8, dpi = 300, bg = "white")
print(p1)

# ============================================================================
# VIZ 2: HEATMAP DES QUESTIONS - VERSION AMÉLIORÉE
# ============================================================================

cat("🔥 Viz 2: Heatmap des questions...\n")

score_by_question <- data_clean %>%
  group_by(Class.ASD) %>%
  summarise(across(starts_with("A") & ends_with("Score"), mean)) %>%
  pivot_longer(-Class.ASD, names_to = "Question", values_to = "Moyenne") %>%
  mutate(
    Question_Clean = str_extract(Question, "A\\d+"),
    Domaine = case_when(
      Question_Clean %in% c("A1", "A2", "A3") ~ "Social",
      Question_Clean %in% c("A4", "A5", "A6") ~ "Attention",
      Question_Clean %in% c("A7", "A8") ~ "Communication",
      Question_Clean %in% c("A9", "A10") ~ "Imagination"
    )
  )

# Calculer les différences
differences <- score_by_question %>%
  select(Question_Clean, Class.ASD, Moyenne) %>%
  pivot_wider(names_from = Class.ASD, values_from = Moyenne) %>%
  mutate(Difference = YES - NO) %>%
  select(Question_Clean, Difference)

score_by_question <- score_by_question %>%
  left_join(differences, by = "Question_Clean")

p2 <- ggplot(score_by_question, aes(x = Question_Clean, y = Class.ASD, fill = Moyenne)) +
  geom_tile(color = "white", size = 1.5) +
  geom_text(aes(label = sprintf("%.2f", Moyenne)), 
            color = "white", fontface = "bold", size = 5) +
  # Ajouter les différences sous forme de petite annotation
  geom_text(aes(y = 2.5, label = ifelse(Class.ASD == "YES", 
                                        sprintf("Δ=%.2f", Difference), "")),
            color = "#2c3e50", size = 3, fontface = "italic") +
  # Facette par domaine
  facet_grid(. ~ Domaine, scales = "free_x", space = "free_x") +
  scale_fill_gradient2(
    low = "#3498db", 
    mid = "#ffffff", 
    high = "#e74c3c",
    midpoint = 0.5, 
    limits = c(0, 1),
    name = "Score\nMoyen",
    breaks = seq(0, 1, 0.25),
    labels = percent
  ) +
  labs(
    title = "Score Moyen par Question AQ-10 et Diagnostic",
    subtitle = "Plus le rouge est foncé, plus le comportement autistique est fréquent | Δ = différence YES-NO",
    x = "Questions AQ-10 (par Domaine Cognitif)",
    y = "Diagnostic",
    caption = "Les valeurs indiquent la proportion d'enfants avec comportement autistique"
  ) +
  theme_custom() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.background = element_rect(fill = "#34495e", color = NA),
    strip.text = element_text(color = "white", face = "bold", size = 12),
    panel.spacing = unit(0.5, "lines")
  )

ggsave("outputs/figures/heatmap_questions_enhanced.png", p2, 
       width = 14, height = 6, dpi = 300, bg = "white")
print(p2)

# ============================================================================
# VIZ 3: CORRÉLATION - VERSION AMÉLIORÉE
# ============================================================================

cat("🔗 Viz 3: Matrice de corrélation...\n")

cor_matrix <- cor(data_clean[, score_cols], use = "complete.obs")

png("outputs/figures/correlation_matrix_enhanced.png", 
    width = 1000, height = 1000, res = 120)
par(bg = "white")
corrplot(cor_matrix, 
         method = "circle",
         type = "upper",
         order = "hclust",
         tl.col = "#2c3e50", 
         tl.srt = 45,
         tl.cex = 1.2,
         addCoef.col = "black",
         number.cex = 0.9,
         col = colorRampPalette(c("#3498db", "white", "#e74c3c"))(200),
         title = "Corrélations entre Questions AQ-10",
         mar = c(0, 0, 2, 0),
         cl.cex = 1.1,
         addgrid.col = "white")
dev.off()

# ============================================================================
# VIZ 4 (NOUVELLE): DISTRIBUTION PAR DOMAINE COGNITIF
# ============================================================================

cat("🧠 Viz 4: Distribution par domaine cognitif...\n")

domain_scores <- data_clean %>%
  select(Class.ASD, social_score, attention_score, 
         communication_score, imagination_score) %>%
  pivot_longer(-Class.ASD, names_to = "Domaine", values_to = "Score") %>%
  mutate(
    Domaine = factor(Domaine, 
                     levels = c("social_score", "attention_score", 
                                "communication_score", "imagination_score"),
                     labels = c("Social\n(A1-A3)", "Attention\n(A4-A6)", 
                                "Communication\n(A7-A8)", "Imagination\n(A9-A10)"))
  )

p4 <- ggplot(domain_scores, aes(x = Score, y = Domaine, fill = Class.ASD)) +
  geom_density_ridges(
    alpha = 0.7, 
    scale = 0.9,
    rel_min_height = 0.01,
    panel_scaling = TRUE
  ) +
  scale_fill_manual(values = colors_custom, name = "Diagnostic") +
  scale_x_continuous(breaks = 0:10) +
  labs(
    title = "Distribution des Scores par Domaine Cognitif",
    subtitle = "Comparaison des profils TSA vs. Sans TSA",
    x = "Score (0 à max par domaine)",
    y = "",
    caption = "Les courbes montrent la densité de distribution des scores"
  ) +
  theme_custom() +
  theme(
    legend.position = c(0.85, 0.15),
    legend.background = element_rect(fill = "white", color = "#2c3e50", size = 0.5)
  )

ggsave("outputs/figures/domain_distribution.png", p4, 
       width = 12, height = 8, dpi = 300, bg = "white")
print(p4)

# ============================================================================
# VIZ 5 (NOUVELLE): ANALYSE PAR ÂGE
# ============================================================================

cat("👶 Viz 5: Analyse par âge...\n")

age_analysis <- data_clean %>%
  group_by(age, Class.ASD) %>%
  summarise(
    n = n(),
    mean_score = mean(total_score),
    .groups = "drop"
  )

p5 <- ggplot(data_clean, aes(x = age, y = total_score, color = Class.ASD)) +
  # Points avec transparence
  geom_jitter(alpha = 0.4, size = 2, width = 0.2, height = 0.1) +
  # Lignes de tendance
  geom_smooth(method = "loess", se = TRUE, size = 1.5, alpha = 0.2) +
  # Seuil clinique
  geom_hline(yintercept = 6, linetype = "dashed", color = "#e74c3c", size = 1) +
  scale_color_manual(values = colors_custom, name = "Diagnostic") +
  scale_x_continuous(breaks = 4:12) +
  scale_y_continuous(breaks = 0:10) +
  labs(
    title = "Score AQ-10 en Fonction de l'Âge",
    subtitle = "Évolution du score total selon l'âge de l'enfant",
    x = "Âge (années)",
    y = "Score Total AQ-10",
    caption = "Ligne pointillée rouge = seuil clinique (6) | Bande = intervalle de confiance"
  ) +
  theme_custom() +
  theme(legend.position = c(0.15, 0.85))

ggsave("outputs/figures/age_analysis.png", p5, 
       width = 12, height = 8, dpi = 300, bg = "white")
print(p5)

# ============================================================================
# VIZ 6 (NOUVELLE): IMPACT DES FACTEURS DE RISQUE
# ============================================================================

cat("⚠️ Viz 6: Facteurs de risque...\n")

risk_factors <- data_clean %>%
  mutate(
    Antécédents = ifelse(has_family_history == 1, "Oui", "Non"),
    Jaunisse = ifelse(had_jaundice == 1, "Oui", "Non")
  ) %>%
  group_by(Antécédents, Jaunisse, Class.ASD) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Antécédents, Jaunisse) %>%
  mutate(
    total = sum(n),
    percentage = n / total * 100
  )

p6 <- ggplot(risk_factors, aes(x = Antécédents, y = percentage, fill = Class.ASD)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7) +
  geom_text(aes(label = sprintf("%d\n(%.1f%%)", n, percentage)),
            position = position_fill(vjust = 0.5),
            size = 4, fontface = "bold", color = "white") +
  facet_wrap(~ Jaunisse, labeller = labeller(Jaunisse = c("Non" = "Sans Jaunisse", 
                                                          "Oui" = "Avec Jaunisse"))) +
  scale_fill_manual(values = colors_custom, name = "Diagnostic") +
  scale_y_continuous(labels = percent, breaks = seq(0, 1, 0.25)) +
  labs(
    title = "Impact des Facteurs de Risque sur le Diagnostic TSA",
    subtitle = "Proportion de TSA selon antécédents familiaux et jaunisse néonatale",
    x = "Antécédents Familiaux de TSA",
    y = "Proportion (%)",
    caption = "Effectifs et pourcentages indiqués dans les barres"
  ) +
  theme_custom() +
  theme(
    strip.background = element_rect(fill = "#34495e", color = NA),
    strip.text = element_text(color = "white", face = "bold")
  )

ggsave("outputs/figures/risk_factors.png", p6, 
       width = 12, height = 7, dpi = 300, bg = "white")
print(p6)

# ============================================================================
# VIZ 7 (NOUVELLE): RADAR CHART - PROFIL MOYEN
# ============================================================================

cat("📡 Viz 7: Profil radar...\n")

# Préparer les données pour le radar
radar_data <- data_clean %>%
  group_by(Class.ASD) %>%
  summarise(across(all_of(score_cols), mean)) %>%
  pivot_longer(-Class.ASD, names_to = "Question", values_to = "Score") %>%
  mutate(Question = str_extract(Question, "A\\d+"))

p7 <- ggplot(radar_data, aes(x = Question, y = Score, group = Class.ASD, 
                             color = Class.ASD, fill = Class.ASD)) +
  geom_polygon(alpha = 0.3, size = 1.5) +
  geom_point(size = 4) +
  coord_polar() +
  scale_color_manual(values = colors_custom, name = "Diagnostic") +
  scale_fill_manual(values = colors_custom, name = "Diagnostic") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), 
                     labels = percent) +
  labs(
    title = "Profil Comportemental Moyen par Question AQ-10",
    subtitle = "Vue radar des scores moyens pour chaque diagnostic",
    caption = "0% = aucun comportement autistique | 100% = comportement autistique systématique"
  ) +
  theme_custom() +
  theme(
    axis.text.y = element_text(size = 10),
    legend.position = "bottom"
  )

ggsave("outputs/figures/radar_profile.png", p7, 
       width = 10, height = 10, dpi = 300, bg = "white")
print(p7)

# ============================================================================
# VIZ 8 (NOUVELLE): DASHBOARD RÉSUMÉ
# ============================================================================

cat("📊 Viz 8: Dashboard résumé...\n")

# Créer 4 petits graphiques
# 1. Distribution globale
p8_1 <- ggplot(data_clean, aes(x = total_score, fill = Class.ASD)) +
  geom_histogram(binwidth = 1, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = colors_custom) +
  labs(title = "Distribution des Scores", x = "Score", y = "Effectif") +
  theme_custom() +
  theme(legend.position = "none", 
        plot.title = element_text(size = 12),
        axis.title = element_text(size = 10))

# 2. Proportions par diagnostic
prop_data <- data_clean %>%
  count(Class.ASD) %>%
  mutate(percentage = n / sum(n) * 100)

p8_2 <- ggplot(prop_data, aes(x = "", y = n, fill = Class.ASD)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = sprintf("%s\n%d (%.1f%%)", Class.ASD, n, percentage)),
            position = position_stack(vjust = 0.5),
            fontface = "bold", size = 4) +
  scale_fill_manual(values = colors_custom) +
  labs(title = "Répartition des Diagnostics") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

# 3. Top 5 questions discriminantes
importance_data <- data_clean %>%
  pivot_longer(all_of(score_cols), names_to = "Question", values_to = "Score") %>%
  group_by(Question, Class.ASD) %>%
  summarise(mean = mean(Score), .groups = "drop") %>%
  pivot_wider(names_from = Class.ASD, values_from = mean) %>%
  mutate(
    Difference = abs(YES - NO),
    Question = str_extract(Question, "A\\d+")
  ) %>%
  arrange(desc(Difference)) %>%
  head(5)

p8_3 <- ggplot(importance_data, aes(x = reorder(Question, Difference), y = Difference)) +
  geom_col(fill = "#9b59b6", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.2f", Difference)), 
            hjust = -0.2, fontface = "bold", size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Top 5 Questions Discriminantes", 
       x = "Question", y = "Différence |YES - NO|") +
  theme_custom() +
  theme(plot.title = element_text(size = 12),
        axis.title = element_text(size = 10))

# 4. Score par âge (simplifié)
p8_4 <- ggplot(data_clean, aes(x = factor(age), y = total_score, fill = Class.ASD)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
  scale_fill_manual(values = colors_custom) +
  labs(title = "Score par Âge", x = "Âge", y = "Score") +
  theme_custom() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(size = 12),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(angle = 0))

# Combiner avec patchwork
dashboard <- (p8_1 + p8_2) / (p8_3 + p8_4) +
  plot_annotation(
    title = "Dashboard Exploratoire - Diagnostic TSA",
    subtitle = sprintf("Analyse de %d enfants | Dataset AQ-10", nrow(data_clean)),
    theme = theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#7f8c8d")
    )
  )

ggsave("outputs/figures/dashboard_summary.png", dashboard, 
       width = 16, height = 12, dpi = 300, bg = "white")
print(dashboard)

# ============================================================================
# RÉSUMÉ DES VISUALISATIONS CRÉÉES
# ============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("✅ VISUALISATIONS CRÉÉES AVEC SUCCÈS\n")
cat(strrep("=", 70), "\n\n", sep = "")

viz_list <- data.frame(
  Numéro = 1:8,
  Fichier = c(
    "scores_distribution_enhanced.png",
    "heatmap_questions_enhanced.png",
    "correlation_matrix_enhanced.png",
    "domain_distribution.png",
    "age_analysis.png",
    "risk_factors.png",
    "radar_profile.png",
    "dashboard_summary.png"
  ),
  Description = c(
    "Distribution scores + statistiques",
    "Heatmap par domaine cognitif",
    "Matrice corrélation optimisée",
    "Distribution par domaine",
    "Évolution selon l'âge",
    "Impact facteurs de risque",
    "Profil radar moyen",
    "Dashboard 4 graphiques"
  )
)

print(viz_list)

cat("\n📁 Tous les graphiques sont sauvegardés dans: outputs/figures/\n")
cat("📐 Résolution: 300 DPI (publication-ready)\n")
cat("🎨 Thème: Personnalisé et professionnel\n")
cat(strrep("=", 70), "\n", sep = "")