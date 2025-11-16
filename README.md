#Projet d'Analyse de Données DPE (R Shiny + R Markdown)

Projet réalisé par : V. GROSJEAN & T. ALSHAWWA

Nous: GreenTech Solutions
Notre client: Enedis

1. Description du Projet

Ce projet analyse les données de Diagnostics de Performance Énergétique (DPE) pour les départements 59 (Nord) et 66 (Pyrénées-Orientales). Il vise à répondre à la problématique suivante :

Comment les performances énergétiques des logements dans le Nord (Dép. 59) se comparent-elles à celles du Sud (Dép. 66), et quels facteurs (type de logement, type de chauffage, isolation) expliquent les différences observées ?

Ce dépôt contient les deux livrables finaux du projet :

I. Une application d'exploration interactive (Shiny).

II. Un rapport d'analyse statique (R Markdown) présentant les conclusions clés.

2. Contenu du Dépôt

Ce dépôt est structuré comme suit :

app.R : Le script complet de l'application web interactive Shiny.

rapport_statistique_final.Rmd : Le script R Markdown source utilisé pour générer le rapport d'analyse statique.

rapport_statistique_final.html : (Livrable) Le rapport d'analyse final, pré-généré et consultable directement.

doc_fonctionnelle.md : Le guide utilisateur destiné au client Enedis expliquant comment utiliser l'application Shiny.

doc_technique.md : La documentation technique expliquant l'architecture du projet.

API code.R : Le script R original utilisé pour l'extraction initiale des données depuis l'API de l'ADEME.

Fichiers de support

df_nord_sud_COMBINE_COMPLET.csv : Le jeu de données complet (68k+ lignes) utilisé par les deux scripts.

www/ (dossier) : Contient les assets pour l'application Shiny (CSS pour les thèmes, logos).

greentech_logo.png & Logo_enedis.png : Logos utilisés pour la page de garde du rapport R Markdown.

3. Instructions d'Utilisation

Vous pouvez évaluer ce projet de deux manières.

A. Lancer l'Application Shiny (Interactive)

L'application est configurée pour lire les données directement depuis ce dépôt GitHub public. Vous n'avez pas besoin de gérer le fichier CSV localement pour cette partie.

Cloner ou Télécharger ce Dépôt : Assurez-vous d'avoir tous les fichiers!

Ouvrir app.R dans RStudio : Ouvrez le script de l'application.

Installer les Packages : Assurez-vous que tous les packages R requis sont installés (voir la liste library(...) en haut du script app.R).

Lancer l'Application : Cliquez sur le bouton "Run App" dans RStudio.

Connexion : L'application chargera les données depuis GitHub et vous présentera un écran de connexion. Utilisez les identifiants suivants :

Utilisateur : greentech

Mot de passe : pas les meilleurs mais pas les pires non plus

B. Consulter le Rapport R Markdown (Statique)

Télécharger ce dépôt.

Dans le dossier de votre ordinateur, trouvez le fichier rapport_statistique_final.html.

Double-cliquez sur ce fichier pour l'ouvrir dans votre navigateur (ex: Chrome).

Vous verrez le rapport final, mis en forme, avec tous les graphiques et analyses.


