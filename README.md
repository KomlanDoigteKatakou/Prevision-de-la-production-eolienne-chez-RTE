# Prévision de la Production Éolienne chez RTE

## Contexte et Description

L'énergie éolienne est un pilier de la transition énergétique. Mais, derrière les pales qui tournent, se cachent une mécanique complexe et de gros volumes de données dont la bonne compréhension permet une bonne anticipation de la production.

Ce projet a pour objectif d'expliquer la production éolienne française et de prédire le facteur de charge (indice de rendement) du parc éolien français à partir des données météorologiques et géographiques relevées sur tout le territoire français découpé au préalable en une grille de dix ou plusieurs zones. Nous disposons aussi d'un modèle initial proposé par RTE, commettant une erreur de prédiction moyenne (RMSE) de 4.7 % sur le rendement éolien et notre objectif est de proposer d'autres modèles pour réduire cette erreur de manière afin d'avoir des prédictions plus précises.

Ce projet utilise d'une part des packages sophistiqués du logiciel R pour la manipulation de gros volumes de données et d'autre part des techniques d'analyse et de modélisation statistique avancée, notamment les modèles additifs généralisés.

### Etapes :
- Compréhension de la problématique de RTE : principe de fonctionnement des installations éoliennes, indicateurs techniques (puissance installée, facteur de charge)
- Analyse cartographique et sectorisation climatique :
  - Manipulation des coordonnées géographiques (longitude et latitude) des sites d'implantation d'éoliennes et visualisation cartographique
  - Découpage du territoire français en dix zones, en tenant compte des disparités climatiques régionales.

- Traitement des données massives de RTE : Extraction, Lecture, Nettoyage et Agrégation des séries chronologiques de données :
  - Données climatiques par site : Vitesses (composantes U et V) du vent à 100 mètres d'altitude, enregistrées chaque heure (2017 - 2022)
  - Données de production éolienne (mesurées chaque demi-heure sur la même période) à l'échelle nationale

- Manipulation avancée du logiciel R :
  - Utilisation intensive de packages variés : tidyverse, data.table,  lubridate, forecast, maps, ggplot2, ...
  - Gestion de divers formats de fichiers (`csv, xls, nc, RDS, dat, txt`, ...)

- Modélisation prédictive :
  - Modélisation GAM : un premier modèle pour expliquer la puissance installée du parc éolien en fonction des variables climatiques et de la saisonnalité et un deuxième pour prédire le facteur de charge (indice de rendement) éolien à partir de la vitesse du vent sur les dix zones
  - Détection de multicolinéarité et amélioration des performances par ACP et des modèles de régularisation
  - Redéfinition du découpage géographique en 20 zones et reprise de la modélisation initiale pour capturer les effets très locaux de la vitesse du vent

## Packages R :
 - `tidyverse` : collection de packages pour la science des données (inclut `ggplot2`, `dplyr`, `tidyr`, `purrr`, etc.)
- `ggplot2` : création de visualisations élégantes et personnalisables
- `dplyr` : manipulation efficace des données (filtrage, regroupement, etc.)
- `tidyr` : réorganisation des données (pivotements, reshaping)
- `janitor` : nettoyage rapide des jeux de données (`clean_names()`, détection des doublons, etc.)
- `purrr` : programmation fonctionnelle et manipulation de listes ou de colonnes imbriquées
- `lubridate` : gestion simple des dates et heures
- `data.table` pour la manipulation des fichiers concernés
- `ncdf4` pour la manipulation des fichiers de l'extension `.nc` (format de brique multidimensionnelle)
- `glmnet` : ajustement de modèles linéaires généralisés régularisés (Lasso, Ridge, Elastic-Net)
- `mgcv` : ajustement de modèles additifs généralisés (GAM)
- `maps` : cartes de base pour visualiser des données géographiques
- `sf` : gestion et manipulation de données spatiales (format *Simple Features*)

## Impacts
- Résultat obtenu : une erreur de prédiction moyenne (RMSE) de 0.04% sur le rendement, soit 100 fois mieux que le modèle initial de RTE

- Autre impact :
    - Mise en évidence des variables les plus influentes sur le rendement éolien
    - Identification des zones à grand rendement et proposition de délocalisation des installations peu rentables.
