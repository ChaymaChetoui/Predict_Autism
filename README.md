# 🧠 Prédiction du Trouble du Spectre Autistique chez l'Enfant

[![Quarto](https://img.shields.io/badge/Quarto-1.4+-blue)](https://quarto.org)
[![WebR](https://img.shields.io/badge/WebR-Enabled-green)](https://docs.r-wasm.org/webr/latest/)
[![License](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

> Analyse prédictive du TSA basée sur le questionnaire AQ-10 avec machine learning

**🌐 Site web :** (https://chaymachetoui.github.io/Predict_Autism/)

---

## 📖 Description

Ce projet utilise des techniques de **machine learning** pour prédire le Trouble du Spectre Autistique (TSA) chez l'enfant à partir du questionnaire **AQ-10** (Autism Spectrum Quotient).

### 🎯 Objectifs

- Développer des modèles prédictifs performants (Random Forest, Régression Logistique, XGBoost)
- Identifier les variables les plus discriminantes du questionnaire AQ-10
- Fournir un outil interactif de dépistage via WebR
- Créer un rapport complet et reproductible avec Quarto

### 📊 Résultats Clés

| Modèle | Accuracy | AUC | Sensibilité | Spécificité |
|--------|----------|-----|-------------|-------------|
| **Random Forest** | **97.1%** | **0.984** | 95.7% | 98.2% |
| Régression Logistique | 95.2% | 0.962 | 92.3% | 96.9% |
| XGBoost | 96.3% | 0.974 | 94.2% | 97.6% |

---

## 🚀 Démarrage Rapide

### Prérequis

- R ≥ 4.3
- Quarto ≥ 1.4
- Git

### Installation

```bash
# 1. Cloner le projet
git clone https://github.com/votre-username/projet-tsa.git
cd projet-tsa

# 2. Installer l'extension WebR
quarto add coatless/quarto-webr

# 3. Installer les packages R nécessaires
Rscript -e "install.packages(c('caret', 'randomForest', 'xgboost', 'pROC', 'ggplot2', 'dplyr'))"

# 4. Lancer le site localement
quarto preview
```

Le site s'ouvrira automatiquement dans votre navigateur à `http://localhost:XXXX`

---

## 📁 Structure du Projet

```
projet-tsa/
├── _quarto.yml              # Configuration Quarto
├── index.qmd                # Page d'accueil
├── analyse.qmd              # Analyse interactive (WebR)
├── rapport.qmd              # Rapport détaillé
├── about.qmd                # À propos
├── custom.scss              # Styles personnalisés
│
├── data/
│   └── results.csv          # Dataset AQ-10
│
├── outputs/
│   ├── figures/             # Graphiques générés
│   │   ├── scores_distribution.png
│   │   ├── heatmap_questions.png
│   │   ├── correlation_matrix.png
│   │   ├── rf_importance.png
│   │   ├── model_comparison.png
│   │   └── roc_curves.png
│   │
│   └── models/              # Modèles sauvegardés
│       ├── rf_model.rds
│       ├── logit_model.rds
│       └── test_data.rds
│
├── scripts/
│   └── analyse.R            # Script R complet
│
├── docs/                    # Site web généré (après render)
├── README.md
└── LICENSE
```

---

## 🔬 Méthodologie

### 1️⃣ Prétraitement des Données

- Nettoyage des doublons et valeurs manquantes
- Correction des noms de colonnes
- Feature engineering (scores par domaine)
- Gestion des valeurs aberrantes

### 2️⃣ Modélisation

**Algorithmes utilisés :**
- Random Forest (ntree=500, mtry optimisé)
- Régression Logistique (famille binomiale)
- XGBoost (gradient boosting)

**Validation :**
- Cross-validation 10-fold
- Split Train/Test (70/30)
- Optimisation des hyperparamètres

### 3️⃣ Évaluation

**Métriques :**
- Accuracy, AUC-ROC
- Sensibilité, Spécificité
- Precision, Recall, F1-Score
- Matrice de confusion

---

## 💻 Utilisation

### Option 1 : Interface Web

1. Allez sur [le site web](https://votre-username.github.io/projet-tsa)
2. Naviguez vers **Analyse Interactive**
3. Exécutez les blocs de code WebR directement dans le navigateur
4. Modifiez les paramètres pour tester différents scénarios

### Option 2 : Script R Local

```r
# Lancer l'analyse complète
source("scripts/analyse.R")

# Les sorties seront générées dans outputs/
```

### Option 3 : Utiliser les Modèles Sauvegardés

```r
# Charger un modèle entraîné
rf_model <- readRDS("outputs/models/rf_model.rds")

# Faire une prédiction
new_case <- data.frame(
  A1_Score = 1, A2_Score = 1, A3_Score = 1,
  A4_Score = 0, A5_Score = 1, A6_Score = 1,
  A7_Score = 0, A8_Score = 1, A9_Score = 0, A10_Score = 1,
  age = 7, has_family_history = 1, had_jaundice = 0,
  social_score = 3, attention_score = 2,
  communication_score = 1, imagination_score = 1
)

prediction <- predict(rf_model, new_case)
probability <- predict(rf_model, new_case, type = "prob")
```

---

## 📊 Variables du Dataset

| Variable | Type | Description |
|----------|------|-------------|
| `A1-A10_Score` | Binaire (0/1) | Réponses aux 10 questions AQ-10 |
| `age` | Numérique | Âge de l'enfant (4-12 ans) |
| `jaundice` | Binaire | Jaunisse néonatale (yes/no) |
| `autism` | Binaire | Antécédents familiaux de TSA (yes/no) |
| `total_score` | Numérique | Score total AQ-10 (0-10) |
| `social_score` | Numérique | Score questions sociales (A1-A3) |
| `attention_score` | Numérique | Score attention (A4-A6) |
| `communication_score` | Numérique | Score communication (A7-A8) |
| `imagination_score` | Numérique | Score imagination (A9-A10) |
| `Class.ASD` | Catégorique | Diagnostic (YES/NO) |

---

## 🌐 Déploiement

### GitHub Pages

```bash
# Build le site
quarto render

# Commit et push
git add docs/
git commit -m "Deploy site"
git push origin main

# Activer GitHub Pages dans Settings > Pages
# Source: Deploy from branch main/docs
```

### Netlify

```bash
# Via CLI
netlify deploy --prod --dir=docs

# Ou connectez votre repo GitHub sur netlify.com
```

### Quarto Pub

```bash
quarto publish quarto-pub
```

---

## 🔧 Développement

### Ajouter une Nouvelle Page

1. Créer `nouvelle-page.qmd`
2. Ajouter dans `_quarto.yml` :
   ```yaml
   navbar:
     left:
       - text: "Nouvelle Page"
         href: nouvelle-page.qmd
   ```
3. Render : `quarto render`

### Modifier les Styles

Éditez `custom.scss` puis :
```bash
quarto render
```

### Ajouter des Packages WebR

Dans vos blocs `{webr-r}` :
```r
webr::install("nom-package", quiet = TRUE)
library(nom-package)
```

---

## 📚 Références

1. Baron-Cohen, S., et al. (2001). The Autism-Spectrum Quotient (AQ). *Journal of Autism and Developmental Disorders*.

2. Allison, C., et al. (2012). The Q-CHAT: A quantitative measure of autistic traits. *Journal of Autism*.

3. Breiman, L. (2001). Random Forests. *Machine Learning*, 45(1), 5-32.

4. Chen, T., & Guestrin, C. (2016). XGBoost: A Scalable Tree Boosting System. *KDD*.

---

## ⚠️ Avertissement

**CE MODÈLE EST À BUT ÉDUCATIF UNIQUEMENT.**

Il ne remplace en aucun cas :
- Un diagnostic clinique professionnel
- Une évaluation complète par un spécialiste
- Des tests neuropsychologiques approfondis

Pour toute question concernant le TSA, consultez un professionnel de santé qualifié.

---



## 👥 Contribution

Les contributions sont les bienvenues ! 

1. Fork le projet
2. Créez une branche (`git checkout -b feature/amelioration`)
3. Commit vos changements (`git commit -m 'Ajout amelioration'`)
4. Push vers la branche (`git push origin feature/amelioration`)
5. Ouvrez une Pull Request

---

## 📧 Contact

**Auteur :** Chayma Chetoui

- 📧 Email : chaymacheoui2821@gmail.com



**⭐ Si ce projet vous a aidé, n'oubliez pas de lui donner une étoile !**
