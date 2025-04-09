# Prévision de la Production Éolienne chez RTE

## Description

Ce projet a pour objectif de prédire la production éolienne en France en utilisant les données météorologiques horaires des dix dernières années agrégées sur plusieurs zones. Le modèle vise à découper la France en 10 zones géographiques et à prédire la production d'énergie olienne pour chaque zone en fonction de facteurs météorologiques, géographiques et autres paramètres liés à la production éolienne. Ce projet utilise d'une part des packages sophistiqués du logiciel R pour la manipulation de gros volumes de données et d'autre part des techniques d'analyse statistique avancée et des modèles de régression, notamment les modèles linéaires généralisés (GLM) pour effectuer les prévisions.

### Objectifs du projet :
- Analyser les données chronologiques de production éolienne de RTE.
- Découper la France en 10 zones géographiques pour mieux prédire la production.
- Appliquer des modèles statistiques (GLM et GAM) pour estimer la production éolienne en fonction des variables explicatives.
- Manipuler des bases de données massives dans R .
- Effectuer des analyses exploratoires et des tests statistiques
- Réaliser des visualisations et des rapports d'analyse.

## Prérequis

- **R** version 4.x ou plus
- **Packages R** :
  - `ggplot2` pour la visualisation des données
  - `dplyr` pour la manipulation des données
  - `tidyr` pour le nettoyage et la transformation des données
  - `lubridate` pour la gestion des dates
  - `caret` pour les modèles de machine learning
  - `glmnet` pour les modèles linéaires généralisés (GLM)

## Installation

```R
install.packages(c("ggplot2", "dplyr", "tidyr", "lubridate", "caret", "glmnet"))
