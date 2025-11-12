# APPLICATION SHINY POUR LA COMPARAISON DPE NORD (59) vs SUD (66)

#Chargement des bibliothèques
# On charge tous les packages dont on aura besoin pour l'appli
library(readr)
library(shiny)
library(dplyr)       # Pour la manipulation de données (filter, mutate...)
library(ggplot2)     # Pour les graphiques statiques
library(plotly)      # Pour rendre les graphiques interactifs
library(bslib)
library(lubridate)   # Pour gérer les dates (ex: extraire l'année)
library(leaflet)     # Pour la carte
library(sf)          # Pour la conversion des coordonnées GPS
library(DT)          # Pour afficher des tableaux de données interactifs
library(shinydashboard) # La structure principale de notre interface
library(scales)      # Pour les labels en pourcentage (%)
library(fontawesome)
library(shinythemes)
library(shinyjs)     # Pour des actions JavaScript (cacher/montrer des trucs)
library(rsconnect)
library(curl)
library(tibble)
library(shinyauthr)  # Pour l'écran de connexion (login)
library(sodium)      # Pour le "hachage" (cryptage) des mots de passe
library(tidyr)
library(httr)
library(jsonlite)

#Chargement des Données
#On définit le nom du fichier.
#Ce fichier doit être dans le MÊME dossier que app.R
data_file <- "df_nord_sud_COMBINE_COMPLET.csv"

# On définit les années min/max pour le slider (au cas où le chargement échoue)
min_year_data <- 2021
max_year_data <- 2025
df <- NULL
show_load_error <- FALSE
error_message <- ""

# On vérifie d'abord si le fichier de données existe
if (file.exists(data_file)) {
  print(paste("Chargement du fichier CSV:", data_file)) 
  # On utilise tryCatch pour gérer les erreurs si le fichier est corrompu
  df <- tryCatch({
    readr::read_csv(data_file, show_col_types = FALSE) 
  }, error = function(e){
    error_message <<- paste("ERREUR lors de la lecture du CSV:", e$message) 
    print(error_message);
    NULL
  })
  
  # Si le chargement a réussi (df n'est pas NULL et n'est pas vide), on nettoie les données
  if (!is.null(df) && is.data.frame(df) && nrow(df) > 0) {
    print("Pré-traitement des données...")
    
    # S'assurer que les dates sont bien au format Date
    if ("date_reception_dpe" %in% names(df) && !inherits(df$date_reception_dpe, "Date")) {
      df$date_reception_dpe <- tryCatch(as.Date(df$date_reception_dpe), error = function(e) NA)
    }
    # S'assurer que le code postal est bien du texte (pour éviter les problèmes)
    if ("code_postal_ban" %in% names(df)) df$code_postal_ban <- as.character(df$code_postal_ban)
    
    # --- Renommer les départements ---
    # C'est plus joli dans les graphiques que "59" et "66" tout seuls
    if("code_departement_ban" %in% names(df)) {
      print("Renommage des codes départements...")
      df <- df %>%
        mutate(code_departement_ban = as.character(code_departement_ban), 
               code_departement_ban = case_when(
                 code_departement_ban == "59" ~ "59 - Nord",
                 code_departement_ban == "66" ~ "66 - Pyrénées-Orientales",
                 TRUE ~ code_departement_ban
               ))
    }
    
    # Nettoyage des NA (valeurs manquantes)
    # On enlève les lignes où les colonnes importantes sont vides
    # Sinon, on a une catégorie "NA" non utile dans nos graphiques
    print(paste("Nombre de lignes avant suppression des NA:", nrow(df)))
    df <- df %>%
      filter(
        !is.na(etiquette_dpe), 
        !is.na(code_departement_ban),
        !is.na(type_source)
      )
    print(paste("Nombre de lignes après suppression des NA:", nrow(df)))
    
    # On vérifie qu'on a bien toutes les colonnes dont on a besoin
    required_cols <- c("date_reception_dpe", "etiquette_dpe", "code_departement_ban",
                       "type_source", "coordonnee_cartographique_x_ban", "coordonnee_cartographique_y_ban", "conso_5_usages_par_m2_ep", "surface_habitable_logement")
    missing_cols <- required_cols[!required_cols %in% names(df)]
    
    if(length(missing_cols) > 0){
      # S'il manque une colonne, on arrête tout et on prépare un message d'erreur
      error_message <- paste(error_message, " Colonnes essentielles manquantes:", paste(missing_cols, collapse=", "))
      print(error_message); show_load_error <- TRUE; df <- NULL
    } else {
      # Tout va bien, on continue le formatage
      
      # Mettre les étiquettes DPE dans le bon ordre (A, B, C...)
      dpe_levels <- c("A", "B", "C", "D", "E", "F", "G")
      df$etiquette_dpe <- factor(df$etiquette_dpe, levels = dpe_levels, ordered = TRUE)
      if("etiquette_ges" %in% names(df)) df$etiquette_ges <- factor(df$etiquette_ges, levels = dpe_levels, ordered = TRUE)
      
      # Convertir les autres colonnes de texte en facteurs (mieux pour les filtres)
      df$code_departement_ban <- as.factor(df$code_departement_ban)
      df$type_source <- as.factor(df$type_source)
      
      # Conversion des coordonnées pour la carte (de Lambert 93 vers GPS WGS84)
      print("Conversion des coordonnées Lambert-93 (X/Y) en WGS84 (Lat/Lon)...")
      df$latitude <- NA_real_
      df$longitude <- NA_real_
      # On ne garde que les lignes qui ont des coordonnées valides
      valid_coords <- !is.na(df$coordonnee_cartographique_x_ban) & !is.na(df$coordonnee_cartographique_y_ban) &
        is.finite(df$coordonnee_cartographique_x_ban) & is.finite(df$coordonnee_cartographique_y_ban)
      
      if(sum(valid_coords) > 0) {
        tryCatch({
          # On utilise le package 'sf' pour la conversion
          sf_data <- st_as_sf(df[valid_coords, ], coords = c("coordonnee_cartographique_x_ban", "coordonnee_cartographique_y_ban"), crs = 2154, remove = FALSE)
          sf_data_wgs84 <- st_transform(sf_data, crs = 4326)
          coords_wgs84 <- st_coordinates(sf_data_wgs84)
          # On ré-insère les bonnes lat/lon dans notre dataframe
          df$latitude[valid_coords] <- coords_wgs84[, "Y"]
          df$longitude[valid_coords] <- coords_wgs84[, "X"]
          print(paste("Conversion réussie pour", sum(is.finite(df$latitude), na.rm=TRUE), "points."))
          # On enlève les lignes qui ont échoué la conversion
          df <- df %>% filter(is.finite(latitude) & is.finite(longitude)) 
          print(paste("Nombre de lignes après filtrage coordonnées valides:", nrow(df)))
        }, error = function(e) {
          # Si 'sf' plante, on continue sans données GPS
          print(paste("Erreur lors de la conversion SF:", e$message))
          df$latitude <- NA_real_; df$longitude <- NA_real_
        })
      } else {
        print("Aucune coordonnée X/Y valide trouvée.")
      }
      
      # On trouve les vraies dates min/max des données pour régler le slider
      valid_dates <- df$date_reception_dpe[!is.na(df$date_reception_dpe)]
      if(length(valid_dates) > 0) {
        min_year_data <- min(year(valid_dates), na.rm=TRUE)
        max_year_data <- max(year(valid_dates), na.rm=TRUE)
      }
      print(paste("Années:", min_year_data, "-", max_year_data))
      print("Pré-traitement terminé.")
      show_load_error <- FALSE
    }
  } else {
    # Cas où le fichier a été lu mais était vide
    if(!show_load_error) {
      error_message <- paste("Le fichier CSV", data_file, "est vide ou n'a pas pu être lu correctement.") 
      print(error_message)
      show_load_error <- TRUE
    }
    df <- NULL
  }
} else {
  # Cas où le fichier .csv n'a pas été trouvé
  error_message <- paste("ERREUR: Fichier", data_file, "introuvable. Assurez-vous qu'il est DANS LE MÊME DOSSIER que app.R") 
  print(error_message)
  show_load_error <- TRUE
  df <- NULL
}

# Création d'un Dataframe Vide (si le chargement a échoué) 
# Si show_load_error est TRUE, on crée un dataframe vide avec les bonnes colonnes
# pour que l'application puisse se lancer (en mode erreur) sans planter.
if (show_load_error || is.null(df) || nrow(df) == 0) {
  if(!show_load_error && !is.null(df)) print("Dataframe vide après pré-traitement.")
  show_load_error <- TRUE
  # On définit la structure vide pour que l'UI ne plante pas
  df <- data.frame(code_departement_ban=factor(),
                   etiquette_dpe=factor(levels=c("A", "B", "C", "D", "E", "F", "G"), ordered=TRUE),
                   date_reception_dpe=as.Date(character()),
                   latitude=numeric(), longitude=numeric(),
                   type_source = factor(),
                   coordonnee_cartographique_x_ban=numeric(),
                   coordonnee_cartographique_y_ban=numeric(),
                   type_energie_principale_chauffage=character(),
                   qualite_isolation_murs=character(),
                   surface_habitable_logement=numeric(),
                   conso_5_usages_par_m2_ep=numeric(),
                   annee_construction=numeric(),
                   nom_commune_ban=character(),
                   type_batiment=character(),
                   stringsAsFactors = FALSE, check.names = FALSE)
  min_year_data <- 2021
  max_year_data <- 2025
}

# --- 3. Variables globales pour l'UI ---
# On prépare les listes de choix pour les filtres (menus déroulants)
type_source_choices <- if(nrow(df) > 0 && "type_source" %in% names(df) && is.factor(df$type_source) && length(levels(droplevels(df$type_source))) > 0) {
  unique_sources <- levels(droplevels(df$type_source))
  stats::setNames(unique_sources, unique_sources)
} else { c("Existant (dpe03)" = "Existant (dpe03)", "Neuf (dpe02)" = "Neuf (dpe02)") }

# On utilise les noms corrigés (ex: "59 - Nord")
departement_choices <- if(nrow(df) > 0 && "code_departement_ban" %in% names(df) && is.factor(df$code_departement_ban) && length(levels(droplevels(df$code_departement_ban))) > 0) {
  unique_depts <- levels(droplevels(df$code_departement_ban))
  stats::setNames(unique_depts, paste0(unique_depts))
} else { c("59 - Nord"="59 - Nord", "66 - Pyrénées-Orientales"="66 - Pyrénées-Orientales") } 

etiquette_choices <- if(nrow(df) > 0 && "etiquette_dpe" %in% names(df) && is.factor(df$etiquette_dpe)) {
  levels(df$etiquette_dpe)
} else { c("A", "B", "C", "D", "E", "F", "G") }

# On récupère les colonnes numériques pour le filtre du nuage de points
numeric_cols <- names(df)[sapply(df, is.numeric) & !(names(df) %in% c("latitude", "longitude", "coordonnee_cartographique_x_ban", "coordonnee_cartographique_y_ban"))]

default_var_1 <- intersect("surface_habitable_logement", numeric_cols)[1]
default_var_2 <- intersect("conso_5_usages_par_m2_ep", numeric_cols)[1]

if (is.na(default_var_1) && length(numeric_cols) > 0) default_var_1 <- numeric_cols[1]
if (is.na(default_var_2) && length(numeric_cols) > 1) default_var_2 <- numeric_cols[2]
if (is.na(default_var_2) && length(numeric_cols) == 1) default_var_2 <- numeric_cols[1]

# Stockage des données dans une variable réactive
# C'est plus efficace de charger les données une fois et de les mettre ici
app_data <- reactiveVal(df)
rm(df) # On supprime le dataframe original pour libérer de la mémoire

# 4. Authentification
# On définit les utilisateurs. Le mot de passe est "hashé" (crypté) automatiquement
user_base <- tibble::tibble(
  user = c("greentech", "user2"),
  password = sapply(c("pas les meilleurs mais pas les pires non plus", "pass2"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("Admin User", "User Two")
)

# 5. Thèmes
# Les choix pour le sélecteur de thème (Clair vs Sombre)
themes_disponibles <- list(
  "Thème Clair" = "skin-blue",
  "Thème Sombre" = "skin-black"
)

# INTERFACE UTILISATEUR (UI)
# Définit à quoi ressemble l'application

ui <- fluidPage(
  shinyjs::useShinyjs(), # Active shinyjs
  
  # JavaScript personnalisé pour le changement de thème
  # On a dû corriger le 'removeClass' pour qu'il enlève l'ancien skin
  # avant d'ajouter le nouveau (sinon les thèmes se superposent)
  shinyjs::extendShinyjs(text = "
        shinyjs.changeSkin = function(skin) {
            $('body').removeClass('skin-blue skin-black').addClass('skin-' + skin);
        };
        
        shinyjs.reloadCSS = function(href) {
            var link = document.getElementById('theme_css');
            if (link) {
                var newHref = href + '?t=' + new Date().getTime();
                link.setAttribute('href', newHref);
            }
        };
    ", functions = c("changeSkin", "reloadCSS")),
  
  # CSS personnalisé pour le thème
  tags$head(
    # Ce tag est la cible de notre JS. Il doit exister pour que reloadCSS("") fonctionne.
    tags$link(id="theme_css", rel = "stylesheet", type = "text/css", href = ""), 
    # On charge le patch "dark_mode_patch" (style.css) mais on le cache.
    # On l'affichera seulement si l'utilisateur choisit le thème sombre.
    shinyjs::hidden(
      tags$link(id = "dark_mode_patch", rel = "stylesheet", type = "text/css", href = "style.css")
    )
  ),
  
  # UI de Connexion 
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  if(!show_load_error) {
    # Si les données sont OK, on affiche l'écran de connexion
    shinyauthr::loginUI(id = "login", title = "Connexion - Analyse DPE", 
                        user_title = "Utilisateur", pass_title = "Mot de passe", 
                        login_title = "Se connecter", error_message = "Identifiants incorrects")
  } else {
    # Si le CSV est introuvable, on affiche un message d'erreur clair
    h3(paste("Erreur critique:", error_message), style="color:red; text-align:center; padding-top: 50px;")
  },
  
  # Contenu principal de l'appli (caché par défaut)
  shinyjs::hidden(
    div(
      id = "show-page-content",
      dashboardPage(
        skin = "blue", # Thème par défaut
        dashboardHeader(
          title = tags$span(
            style = "display: block; width: 100%;",
            tags$img(src = "https://www.soignolles14.fr/wp-content/uploads/2019/03/Logo-ENEDIS.png", 
                     height = "40px", style="margin-top: 5px;")
          ),
          # Notre sélecteur de thème personnalisé
          tags$li(
            class = "dropdown",
            style = "float: right; margin-right: 15px; padding-top: 10px;",
            selectInput(
              inputId = "theme_selector", 
              label = tags$div("Thème:", style = "font-weight: bold; color: white;"),
              choices = names(themes_disponibles),
              selected = "Thème Clair",
              width = "200px"
            )
          )
        ),
        dashboardSidebar(
          # Le menu de navigation à gauche
          sidebarMenu(
            id = "tabs", 
            menuItem("Comparaison Étiquettes", tabName = "etiquettes_dpe", icon = icon("chart-bar")),
            menuItem("Variables Explicatives", tabName = "var_explicatives", icon = icon("lightbulb")),
            menuItem("Carte DPE", tabName = "carte_dpe", icon = icon("map-marked-alt")),
            menuItem("Données Brutes", tabName = "donnees", icon = icon("table")),
            menuItem("Contexte", tabName = "contexte", icon = icon("info-circle"))
          )
        ),
        dashboardBody(
          tabItems(
            # Onglet 1: Étiquettes DPE
            tabItem(tabName = "etiquettes_dpe",
                    fluidRow(
                      # Boîte des filtres
                      box(width = 3, solidHeader = TRUE, status = "primary", title = "Filtres",
                          selectInput("type_source_tab1", "Type DPE :",
                                      choices = c("Tous" = "Tous", type_source_choices),
                                      selected = "Tous"),
                          selectInput("departement_tab1", "Département :",
                                      choices = c("Tous" = "Tous", departement_choices),
                                      selected = "Tous"),
                          sliderInput("plage_annee_tab1", "Année(s) :",
                                      min = min_year_data, max = max_year_data, 
                                      value = c(min_year_data, max_year_data),
                                      step = 1, sep = ""),
                          hr(),
                          h4("Statistiques Clés"),
                          # Les 3 indicateurs (KPIs)
                          valueBoxOutput("kpi_total_logements", width = 12),
                          valueBoxOutput("kpi_passoires", width = 12),
                          valueBoxOutput("kpi_conso_moyenne", width = 12)
                      ),
                      # Boîte du graphique principal (barres)
                      box(width = 9, solidHeader = TRUE, status = "primary", 
                          title = "Distribution des Étiquettes DPE", 
                          plotlyOutput("etiquette_dpe_plot"))
                    ),
                    fluidRow(
                      # Boîte du 2e graphique (lignes)
                      box(width = 3, solidHeader = TRUE, status = "info", title = "Filtre Évolution",
                          selectInput("choix_etiquette_evol", "Étiquette DPE :",
                                      choices = etiquette_choices, selected="D")
                      ),
                      box(width = 9, solidHeader = TRUE, status = "info", 
                          title = "Évolution Mensuelle", 
                          plotlyOutput("evolution_dpe_plot"))
                    )
            ),
            # Onglet 2: Variables Explicatives
            tabItem(tabName = "var_explicatives",
                    fluidRow(
                      # Filtres
                      box(width = 3, solidHeader = TRUE, status = "primary", title = "Filtres",
                          selectInput("type_source_tab2", "Type DPE :",
                                      choices = c("Tous" = "Tous", type_source_choices)),
                          selectInput("departement_tab2", "Département :",
                                      choices = c("Tous" = "Tous", departement_choices)),
                          sliderInput("plage_annee_tab2", "Année(s) :",
                                      min = min_year_data, max = max_year_data,
                                      value = c(min_year_data, max_year_data), step = 1, sep = "")
                      ),
                      # Graphiques (Énergie et Isolation)
                      box(width = 9,
                          fluidRow(box(width = 12, solidHeader = TRUE, status = "warning", 
                                       title = "Top 3 Énergies de Chauffage", 
                                       plotlyOutput("diag_type_energie_plot"))),
                          fluidRow(box(width = 12, solidHeader = TRUE, status = "success", 
                                       title = "Isolation des Murs", 
                                       plotlyOutput("diag_empile_100_murs_plot")))
                      )
                    ),
                    fluidRow(
                      # Nuage de points (Corrélation)
                      box(width = 12, solidHeader = TRUE, status = "danger", 
                          title = "Corrélation Variables",
                          fluidRow(
                            column(6, selectInput("variable1", "Variable X :",
                                                  choices = numeric_cols, selected = default_var_1)),
                            column(6, selectInput("variable2", "Variable Y :",
                                                  choices = numeric_cols, selected = default_var_2))
                          ),
                          fluidRow(
                            column(6, radioButtons("show_regression", "Régression :",
                                                   choices = c("Afficher", "Masquer"), inline = TRUE)),
                            column(6, checkboxInput("filter_outliers", "Exclure extrêmes", value = FALSE))
                          ),
                          hr(), textOutput("coeff_cor_text"), plotlyOutput("nuage_plot")
                      )
                    )
            ),
            #Onglet 3: Carte
            tabItem(tabName = "carte_dpe",
                    fluidRow(
                      box(width = 3, solidHeader = TRUE, status = "primary", title = "Filtres",
                          selectInput("type_source_tab3", "Type DPE :",
                                      choices = c("Tous" = "Tous", type_source_choices)),
                          selectInput("departement_tab3", "Département :",
                                      choices = c("Tous" = "Tous", departement_choices)),
                          sliderInput("plage_annee_tab3", "Année(s) :",
                                      min = min_year_data, max = max_year_data,
                                      value = c(min_year_data, max_year_data), step = 1, sep = ""),
                          selectInput("etiquette_carte", "Étiquette :",
                                      choices = c("Toutes", etiquette_choices))
                      ),
                      box(width = 9, solidHeader = TRUE, status = "primary", 
                          title = "Carte des Logements", 
                          leafletOutput("carte", height = "600px"))
                    )
            ),
            # Onglet 4: Données Brutes
            tabItem(tabName = "donnees",
                    fluidRow(
                      box(width = 3, solidHeader = TRUE, status = "primary", title = "Filtres",
                          selectInput("type_source_tab4", "Type DPE :",
                                      choices = c("Tous" = "Tous", type_source_choices)),
                          selectInput("departement_tab4", "Département :",
                                      choices = c("Tous" = "Tous", departement_choices)),
                          sliderInput("plage_annee_tab4", "Année(s) :",
                                      min = min_year_data, max = max_year_data,
                                      value = c(min_year_data, max_year_data), step = 1, sep = ""),
                          actionButton("refresh_data_api", "Rafraîchir API (Simulation)",
                                       icon = icon("sync"), class = "btn-warning"),
                          hr(),
                          downloadButton("download_data", "Exporter CSV")
                      ),
                      box(width = 9, solidHeader = TRUE, status = "primary", 
                          title = "Tableau des Données", 
                          DTOutput("tableau"))
                    )
            ),
            # Onglet 5: Contexte
            tabItem(tabName = "contexte",
                    fluidRow(
                      box(width = 12, solidHeader = TRUE, status = "info", title = "Contexte",
                          fluidRow(
                            column(8,
                                   h4("Source des Données"),
                                   p("Données ADEME - DPE France métropolitaine"),
                                   tags$ul(
                                     tags$li(tags$b("Existants:"), 
                                             tags$a(href="https://data.ademe.fr/datasets/dpe-france", 
                                                    target="_blank", "dpe03existant")),
                                     tags$li(tags$b("Neufs:"), 
                                             tags$a(href="https://data.ademe.fr/datasets/dpe-logements-neufs-2", 
                                                    target="_blank", "dpe02neuf"))
                                   ),
                                   p("Période:", min_year_data, "-", max_year_data, "| Départements: 59 et 66")
                            ),
                            # L'image du logo ADEME doit être dans le dossier /www
                            column(4, tags$img(src="logo-ademe.jpg", width="80%", style="margin-top:20px;"))
                          )
                      )
                    )
            )
          )
        )
      )
    )
  )
)


# SERVEUR (Toute la logique de l'application)

server <- function(input, output, session) {
  
  # Logique d'authentification
  # 'credentials' est une variable réactive qui contiendra les infos de l'utilisateur
  credentials <- shinyauthr::loginServer(
    id = "login", data = user_base, user_col = user, pwd_col = password, 
    sodium_hashed = TRUE, log_out = reactive(logout_init())
  )
  logout_init <- shinyauthr::logoutServer(
    id = "logout", active = reactive(credentials()$user_auth)
  )
  
  # Cet 'observe' vérifie si l'utilisateur est connecté
  # Si oui (user_auth == TRUE), il cache le login et montre l'application
  observe({
    req(exists("app_data"), shiny::is.reactive(app_data))
    if (credentials()$user_auth && !show_load_error) {
      shinyjs::show(id = "show-page-content")
      shinyjs::hide(id = "login")
    } else if (!credentials()$user_auth && !show_load_error) {
      shinyjs::hide(id = "show-page-content")
      shinyjs::show(id = "login")
    } else {
      shinyjs::hide(id = "show-page-content") # Cache tout si les données n'ont pas chargé
    }
  })
  
  # Logique du sélecteur de thème
  observeEvent(input$theme_selector, {
    req(input$theme_selector)
    
    selected_skin_name <- themes_disponibles[[input$theme_selector]] # ex: "skin-blue"
    selected_skin_js <- gsub("skin-", "", selected_skin_name)       # ex: "blue"
    
    # 1. On appelle notre JS perso pour changer le skin (header/sidebar)
    shinyjs::js$changeSkin(selected_skin_js)
    
    # 2. On affiche ou on cache notre patch CSS (style.css) pour le fond
    if (selected_skin_name == "skin-black") {
      shinyjs::show(id = "dark_mode_patch") # Affiche le CSS sombre
    } else {
      shinyjs::hide(id = "dark_mode_patch") # Cache le CSS sombre
    }
    
    showNotification(
      paste("Thème changé:", input$theme_selector),
      type = "message",
      duration = 2
    )
  }, ignoreInit = TRUE)
  
  # Données Réactives (le coeur de l'appli)
  
  # 'filtered_data_base' contient toutes nos données (68k lignes)
  filtered_data_base <- reactive({
    data_base <- app_data()
    req(data_base, nrow(data_base) > 0)
    data_base
  })
  
  # 'common_filters_data' est la base pour TOUS nos graphiques.
  # Il réagit à l'onglet actif (input$tabs) et applique les filtres de cet onglet.
  common_filters_data <- reactive({
    req(input$tabs) 
    
    # D'abord, on vérifie quel onglet est ouvert pour savoir quels filtres utiliser
    if (input$tabs == "etiquettes_dpe") {
      dept <- input$departement_tab1
      type_src <- input$type_source_tab1
      annees <- input$plage_annee_tab1
    } else if (input$tabs == "var_explicatives") {
      dept <- input$departement_tab2
      type_src <- input$type_source_tab2
      annees <- input$plage_annee_tab2
    } else if (input$tabs == "carte_dpe") {
      dept <- input$departement_tab3
      type_src <- input$type_source_tab3
      annees <- input$plage_annee_tab3
    } else if (input$tabs == "donnees") {
      dept <- input$departement_tab4
      type_src <- input$type_source_tab4
      annees <- input$plage_annee_tab4
    } else {
      return(filtered_data_base()) # Pas de filtre pour l'onglet "Contexte"
    }
    
    # Ensuite, on applique les filtres
    data_filtered <- filtered_data_base()
    start_year <- annees[1]
    end_year <- annees[2]
    
    df_filtered <- data_filtered %>% 
      filter(!is.na(date_reception_dpe) & 
               year(date_reception_dpe) >= start_year & 
               year(date_reception_dpe) <= end_year)
    
    if(dept != "Tous") 
      df_filtered <- df_filtered %>% filter(code_departement_ban == dept)
    if(type_src != "Tous") 
      df_filtered <- df_filtered %>% filter(type_source == type_src)
    
    df_filtered # On retourne le dataframe filtré
  })
  
  # On crée des dataframes réactifs spécifiques pour chaque onglet
  # (même s'ils sont identiques, c'est plus propre)
  filtered_data_tab1 <- reactive({
    common_filters_data()
  })
  
  filtered_data_tab2 <- reactive({
    common_filters_data()
  })
  
  filtered_data_tab3 <- reactive({ 
    data_filtered <- common_filters_data()
    
    data_filtered <- data_filtered %>%
      filter(is.finite(latitude) & is.finite(longitude))
    
    # On ajoute le filtre d'étiquette spécifique à la carte
    if(input$etiquette_carte != "Toutes") 
      data_filtered <- data_filtered %>% filter(etiquette_dpe == input$etiquette_carte)
    
    data_filtered
  })
  
  filtered_data_tab4 <- reactive({ 
    common_filters_data()
  })
  
  # --- OUTPUTS (Onglet 1: Étiquettes) ---
  
  # Graphique 1: Distribution DPE (barres côte à côte)
  output$etiquette_dpe_plot <- renderPlotly({
    data_plot <- filtered_data_tab1()
    req(data_plot, nrow(data_plot) > 0)
    
    # On doit d'abord calculer les proportions par département
    data_summary <- data_plot %>%
      count(code_departement_ban, etiquette_dpe) %>%
      group_by(code_departement_ban) %>%
      mutate(Proportion = n / sum(n))
    
    dpt_colors <- c("59 - Nord" = "#3FA2F6", "66 - Pyrénées-Orientales" = "#FF7F50")
    
    p <- ggplot(data_summary, aes(x = etiquette_dpe, y = Proportion, fill = code_departement_ban)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = dpt_colors, name = "Département") +
      labs(title = "Distribution DPE", x = "Étiquette", y = "Pourcentage")
    
    ggplotly(p) # On rend le graphique interactif
  })
  
  # KPI 1: Total Logements
  output$kpi_total_logements <- renderValueBox({
    data_kpi <- filtered_data_tab1()
    req(data_kpi) 
    total <- format(nrow(data_kpi), big.mark = " ")
    valueBox(total, "Logements", icon = icon("home"), color = "blue")
  })
  
  # KPI 2: Passoires (F+G)
  output$kpi_passoires <- renderValueBox({
    data_kpi <- filtered_data_tab1()
    req(data_kpi, nrow(data_kpi) > 0)
    passoires <- data_kpi %>% filter(etiquette_dpe %in% c("F", "G")) %>% nrow()
    perc <- if(nrow(data_kpi) > 0) round((passoires / nrow(data_kpi)) * 100, 1) else 0
    valueBox(paste0(perc, "%"), "Passoires", icon = icon("fire"), color = "red")
  })
  
  # KPI 3: Conso Moyenne
  output$kpi_conso_moyenne <- renderValueBox({
    data_kpi <- filtered_data_tab1()
    req(data_kpi, nrow(data_kpi) > 0, "conso_5_usages_par_m2_ep" %in% names(data_kpi))
    conso <- mean(data_kpi$conso_5_usages_par_m2_ep, na.rm = TRUE)
    valueBox(paste(round(conso, 0), "kWh/m²"), "Conso moyenne", icon = icon("bolt"), color = "yellow")
  })
  
  # Graphique 2: Évolution DPE (lignes)
  output$evolution_dpe_plot <- renderPlotly({
    req(input$choix_etiquette_evol)
    data_plot_base <- filtered_data_tab1()
    req(data_plot_base, nrow(data_plot_base) > 0)
    
    # Petite sécurité si l'étiquette choisie n'a aucune donnée
    if (!input$choix_etiquette_evol %in% unique(data_plot_base$etiquette_dpe)) {
      return(plotly_empty() %>% layout(title = paste("Aucune donnée pour", input$choix_etiquette_evol)))
    }
    
    # On agrège les données par Mois
    data_plot <- data_plot_base %>% 
      filter(etiquette_dpe == input$choix_etiquette_evol) %>% 
      mutate(Mois = floor_date(date_reception_dpe, "month")) %>%
      count(code_departement_ban, type_source, Mois, name = "Effectif") %>% 
      ungroup()
    
    if(nrow(data_plot) == 0) {
      return(plotly_empty() %>% layout(title = "Aucune donnée"))
    }
    
    # On s'assure d'avoir tous les mois (même avec 0 DPE) pour éviter les "trous"
    all_months <- seq.Date(
      min(floor_date(data_plot_base$date_reception_dpe, "month"), na.rm=TRUE),
      max(floor_date(data_plot_base$date_reception_dpe, "month"), na.rm=TRUE), 
      by="month"
    )
    
    # On complète les données avec des 0 pour les mois manquants
    data_plot <- data_plot %>% 
      tidyr::complete(Mois = all_months, nesting(code_departement_ban, type_source), 
                      fill = list(Effectif = 0)) %>%
      mutate(Groupe = paste0(code_departement_ban, " - ", gsub(" \\(.*", "", type_source)))
    
    p <- ggplot(data_plot, aes(x = Mois, y = Effectif, color = Groupe, group = Groupe,
                               text = paste("Groupe:", Groupe, "<br>Mois:", format(Mois, "%Y-%m"), 
                                            "<br>N:", Effectif))) +
      geom_line(size = 1) + geom_point(size = 1.5) +
      
      # On utilise une palette de 4 couleurs bien distinctes
      scale_color_manual(values = c("59 - Nord - Existant" = "#1f77b4",           # Bleu
                                    "66 - Pyrénées-Orientales - Existant" = "#ff7f0e", # Orange
                                    "59 - Nord - Neuf" = "#2ca02c",               # Vert
                                    "66 - Pyrénées-Orientales - Neuf" = "#d62728"),  # Rouge
                         name = "Groupe") +
      # Fin de la correction des couleurs
      
      labs(title = paste("Évolution", input$choix_etiquette_evol), x = "Date", y = "Nb DPE") +
      theme_minimal(base_size = 10) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0.1, y = -0.2))
  })
  
  # OUTPUTS (Onglet 2: Variables)
  
  # Graphique 3: Top 3 Énergies
  output$diag_type_energie_plot <- renderPlotly({
    data_plot <- filtered_data_tab2()
    req(data_plot, nrow(data_plot) > 0, "type_energie_principale_chauffage" %in% names(data_plot))
    
    # On calcule le top 3 pour chaque groupe (DPE, Dept, Type)
    data_plot <- data_plot %>% 
      filter(!is.na(type_energie_principale_chauffage) & type_energie_principale_chauffage != "") %>%
      count(code_departement_ban, type_source, etiquette_dpe, type_energie_principale_chauffage, name = "Effectif") %>%
      group_by(code_departement_ban, type_source, etiquette_dpe) %>%
      mutate(Total_Groupe = sum(Effectif), 
             Proportion = ifelse(Total_Groupe > 0, Effectif / Total_Groupe, 0)) %>%
      arrange(desc(Effectif)) %>% 
      slice_head(n = 3) %>%
      ungroup() %>%
      mutate(Proportion_text = scales::percent(Proportion, accuracy=1))
    
    if(nrow(data_plot) == 0) {
      return(plotly_empty() %>% layout(title = "Aucune donnée disponible"))
    }
    
    # Graphique en barres empilées 100%
    p <- ggplot(data_plot, aes(x = etiquette_dpe, y = Proportion, fill = type_energie_principale_chauffage,
                               text = paste("Dpt:", code_departement_ban, "<br>Énergie:", 
                                            type_energie_principale_chauffage, "<br>%:", Proportion_text))) +
      geom_bar(stat = "identity", position = "fill") +
      scale_y_continuous(labels = scales::percent) + 
      labs(x = "Étiquette DPE", y = "%", fill = "Énergie") +
      theme_minimal(base_size = 10) + 
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face = "bold"))
    
    ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0.1, y = -0.2))
  })
  
  # Graphique 4: Isolation Murs
  output$diag_empile_100_murs_plot <- renderPlotly({
    data_plot <- filtered_data_tab2()
    req(data_plot, nrow(data_plot) > 0, "qualite_isolation_murs" %in% names(data_plot))
    
    data_plot <- data_plot %>% 
      filter(!is.na(qualite_isolation_murs) & qualite_isolation_murs != "") %>%
      mutate(qualite_isolation_murs = factor(qualite_isolation_murs, 
                                             levels = c("insuffisante", "moyenne", "bonne", "très bonne"), 
                                             ordered = TRUE)) %>%
      count(code_departement_ban, type_source, etiquette_dpe, qualite_isolation_murs, .drop = FALSE) %>%
      group_by(code_departement_ban, type_source, etiquette_dpe) %>%
      mutate(Total = sum(n), Proportion = ifelse(Total > 0, n / Total, 0)) %>% 
      ungroup() %>%
      mutate(Proportion_text = scales::percent(Proportion, accuracy=1))
    
    if(nrow(data_plot) == 0) {
      return(plotly_empty() %>% layout(title = "Données non disponibles"))
    }
    
    p <- ggplot(data_plot, aes(x = etiquette_dpe, y = Proportion, fill = qualite_isolation_murs,
                               text = paste("Qualité:", qualite_isolation_murs, "<br>%:", Proportion_text))) +
      geom_bar(stat = "identity", position = "fill") +
      # On choisit des couleurs logiques (rouge -> vert)
      scale_fill_manual(values = c("insuffisante"="#FF5722", "moyenne"="#FFC107", 
                                   "bonne"="#8BC34A", "très bonne"="#009688"), 
                        name = "Qualité Murs", drop=FALSE) +
      scale_y_continuous(labels = scales::percent) + 
      labs(x = "Étiquette DPE", y = "%") +
      theme_minimal(base_size = 10) + 
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face = "bold"))
    
    ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0.1, y = -0.2))
  })
  
  # Graphique 5: Nuage de points (Corrélation)
  
  # Calcul du coefficient de corrélation
  output$coeff_cor_text <- renderText({
    data_cor <- filtered_data_tab2()
    req(data_cor, nrow(data_cor) > 1, input$variable1, input$variable2)
    req(input$variable1 %in% names(data_cor), input$variable2 %in% names(data_cor))
    
    data_cor_clean <- data_cor %>% 
      select(all_of(c(input$variable1, input$variable2))) %>% 
      na.omit()
    
    # Option pour filtrer les outliers (z-score > 3)
    if (input$filter_outliers && nrow(data_cor_clean) > 2) {
      data_cor_clean <- data_cor_clean %>%
        filter(abs(scale(.data[[input$variable1]])) < 3 & 
                 abs(scale(.data[[input$variable2]])) < 3)
    }
    
    if(nrow(data_cor_clean) < 2) return("Non calculable")
    
    correlation <- tryCatch(
      cor(data_cor_clean[[input$variable1]], data_cor_clean[[input$variable2]]), 
      error = function(e) NA
    )
    
    if (is.na(correlation)) "Non calculable" else paste("Corrélation:", round(correlation, 2))
  })
  
  # Affichage du nuage de points
  output$nuage_plot <- renderPlotly({
    data_plot <- filtered_data_tab2()
    req(data_plot, nrow(data_plot) > 0, input$variable1, input$variable2)
    req(input$variable1 %in% names(data_plot), input$variable2 %in% names(data_plot))
    
    data_plot_clean <- data_plot %>%
      filter(is.finite(.data[[input$variable1]]) & is.finite(.data[[input$variable2]]))
    
    # Option pour filtrer les outliers
    if (input$filter_outliers && nrow(data_plot_clean) > 2) {
      data_plot_clean <- data_plot_clean %>%
        filter(abs(scale(.data[[input$variable1]])) < 3 & 
                 abs(scale(.data[[input$variable2]])) < 3)
    }
    
    if(nrow(data_plot_clean) == 0) {
      return(plotly_empty() %>% layout(title = "Aucune donnée"))
    }
    
    p <- ggplot(data_plot_clean, aes(x = .data[[input$variable1]], y = .data[[input$variable2]])) +
      geom_point(aes(color = etiquette_dpe, shape = type_source), alpha=0.6, size=1.5) +
      scale_color_manual(values = c("A"="#008000", "B"="#50A000", "C"="#A0D000", 
                                    "D"="#FFFF00", "E"="#FFC000", "F"="#FF8000", "G"="#FF0000"), 
                         name="Étiquette", drop=FALSE) +
      scale_shape_manual(values = c("Existant (dpe03)" = 16, "Neuf (dpe02)" = 17), name="Type") +
      labs(title = paste(input$variable1, "vs", input$variable2)) +
      theme_minimal() + 
      theme(legend.position="bottom") +
      facet_wrap(~ code_departement_ban)
    
    if (input$show_regression == "Afficher" && nrow(data_plot_clean) > 1) {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "#555555", formula = y ~ x)
    }
    
    ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.1, y = -0.2))
  })
  
  # OUTPUTS (Onglet 3: Carte)
  
  output$carte <- renderLeaflet({
    data_map <- filtered_data_tab3()
    req(data_map, nrow(data_map) > 0)
    
    pal <- colorFactor(palette = c("#008000", "#50A000", "#A0D000", "#FFFF00", 
                                   "#FFC000", "#FF8000", "#FF0000"), 
                       domain = etiquette_choices, ordered = TRUE)
    
    center_lat <- mean(data_map$latitude, na.rm = TRUE)
    center_lon <- mean(data_map$longitude, na.rm = TRUE)
    
    leaflet(data_map) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = center_lon, lat = center_lat, zoom = 8) %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude, radius = 4,
        color = ~pal(etiquette_dpe), stroke = FALSE, fillOpacity = 0.7,
        popup = ~paste("<b>Commune:</b>", nom_commune_ban, 
                       "<br><b>Étiquette:</b>", etiquette_dpe),
        clusterOptions = markerClusterOptions(maxClusterRadius = 40)
      ) %>%
      addLegend(position = "bottomright", pal = pal, values = etiquette_choices, 
                title = "Étiquette", opacity = 1)
  })
  
  # OUTPUTS (Onglet 4: Données)
  
  output$tableau <- renderDT({
    data_table <- filtered_data_tab4()
    req(data_table, nrow(data_table) > 0)
    
    cols_to_select <- c("type_source", "nom_commune_ban", "code_departement_ban", "etiquette_dpe", 
                        "conso_5_usages_par_m2_ep", "surface_habitable_logement")
    cols_exist <- intersect(cols_to_select, names(data_table))
    
    data_table <- data_table %>% 
      select(all_of(cols_exist), everything(), 
             -any_of(c("latitude", "longitude", "coordonnee_cartographique_x_ban", 
                       "coordonnee_cartographique_y_ban")))
    
    datatable(data_table, options = list(scrollX = TRUE, pageLength = 10), 
              rownames = FALSE, filter = 'top')
  })
  
  output$download_data <- downloadHandler(
    filename = function() paste("dpe_filtre_", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(filtered_data_tab4(), file, row.names = FALSE)
  )
  
  # --- Action API (Simulation) ---
  observeEvent(input$refresh_data_api, {
    showModal(modalDialog("Rafraîchissement en cours (simulation)...", title = "API ADEME", footer = NULL))
    
    Sys.sleep(2) 
    
    removeModal()
    showNotification(
      "Données mises à jour (simulation terminée). L'application utilise maintenant les nouvelles données.",
      type = "message",
      duration = 5
    )
  })
}


# LANCEMENT DE L'APPLICATION


if (!show_load_error) {
  shinyApp(ui = ui, server = server)
} else {
  # Si le chargement des données a échoué, on n'affiche que le message d'erreur
  error_display <- if(error_message != "") error_message else "Impossible de charger les données"
  shinyApp(
    ui = fluidPage(h3(paste("Erreur:", error_display), 
                      style="color:red; text-align:center; padding-top: 50px;")),
    server = function(input, output, session){}
  )
}