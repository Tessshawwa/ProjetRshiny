# Rapport Technique : Analyse de l'Architecture de Données DPE

Ce document analyse l'architecture technique de la solution de visualisation des Données de Performance Énergétique (DPE), composé de deux scripts R distincts : 
API code.R qui est un script de collecte de données (ETL) qui interroge une API externe (ADEME) pour agréger et sauvegarder un jeu de données.
app.R qui est une application web interactive (Shiny) qui charge et visualise les données collectées par le premier script.
L'architecture est découplée : le script de collecte n'est pas exécuté par l'application web. L'application lit un fichier statique, ce qui garantit sa rapidité et sa stabilité.

**Partie 1 : API code.R (Script de Collecte de Données)**

Ce script n'est pas une API en soi, mais un client qui consomme l'API publique de l'ADEME.
L'objectif de ce script est de construire un jeu de données CSV unique (df_nord_sud_COMBINE_COMPLET.csv) en interrogeant l'API de l'ADEME pour des codes postaux spécifiques (Nord 59 et Sud 66) et pour deux types de DPE (neufs et existants).
Librairie importée :
-	httr pour exécuter les requêtes HTTP (GET).
-	jsonlite pour convertir les réponses JSON en dataframes R.
-	dplyr pour la manipulation et l'agrégation des données.
-	readr pour écrire le fichier CSV final.

Processus Technique

Le script suit une logique d'ETL (Extract, Transform, Load) séquentielle :
Configuration (Extract) définit les codes postaux cibles et les datasets (dpe03existant, dpe02neuf). Il présélectionne les colonnes à récupérer pour optimiser les requêtes.
Boucle de Collecte (Extract & Transform) est une double boucle itère sur chaque code postal et dataset, exécute une requête GET et parse la réponse JSON.
Agrégation et Harmonisation (Transform) ajoute les résultats au dataframe principal via bind_rows(). Une colonne type_source est ajoutée pour identifier l'origine ("Existant" ou "Neuf").
Harmonisation est une colonne annee_construction (remplie de NA) est ajoutée au dataset "Neuf" pour permettre la fusion.
Finalisation (Load) pour que le script nettoie les données, crée une colonne typologie_logement, dédoublonne l'ensemble avec distinct() et sauvegarde le fichier CSV.

**Partie 2 : app.R (Application Web Shiny)**

Cette application shinydashboard fournit une interface utilisateur pour explorer les données collectées.
Librairie importée  

-	shiny & shinydashboard qui est le framewAork de base pour l'interface.
-	shinyauthr pour la gestion de l'authentification.
-	shinyjs pour les interactions JavaScript (ex: changer le thème).
-	readr pour lire le fichier CSV au démarrage.
-	dplyr & tidyr pour le filtrage et la transformation en temps réel.
-	lubridate pour la manipulation des dates.
-	plotly pour les graphiques interactifs.
-	leaflet pour la carte interactive.
-	sf (Simple Features) qui est la bibliothèque critique pour le géotraitement.
-	DT pour les tables de données interactives.

Architecture de l'Application

I. Initialisation (Logique "Global")
Chargement des Données :
-	Le script lit le CSV directement depuis une URL GitHub. L'application est donc autonome.
-	Une gestion d'erreur (tryCatch) empêche l'application de démarrer si le fichier est inaccessible.
Pré-traitement des Données :
-	Géotraitement (Étape cruciale) : Les coordonnées Lambert-93 (CRS: 2154) de l'ADEME sont converties en coordonnées GPS WGS84 (CRS: 4326) à l'aide de sf. C'est ce qui permet leur affichage sur leaflet.
-	Les dates sont converties et les facteurs sont ordonnés (ex: "A" à "G").
- Gestion d'État : Les données sont stockées dans une reactiveVal(df) nommée app_data, permettant des mises à jour réactives.

II. Interface Utilisateur (UI)

L'interface gère l'authentification (loginUI) avant d'afficher la structure dashboardPage. Celle-ci organise la navigation (via dashboardSidebar et tabItems) et contient les emplacements pour les graphiques (plotlyOutput), la carte (leafletOutput), et la table (DTOutput).
C. Logique Serveur (Server)
La logique serveur gère la réactivité de l'application. Elle vérifie d'abord l'authentification (credentials()$user_auth) pour afficher ou cacher le contenu (shinyjs::show/hide).
Ensuite, elle utilise un reactive central (common_filters_data) pour appliquer efficacement les filtres (date, département) en fonction de l'onglet actif. Les sorties visuelles (renderPlotly, renderLeaflet, renderDT) utilisent ces données filtrées pour se mettre à jour.
Enfin, le bouton "Rafraîchir API" est une simulation (Sys.sleep(2)) et ne ré-exécute pas le script de collecte API code.R.

