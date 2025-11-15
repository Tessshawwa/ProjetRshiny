
# RÉCUPÉRATION de données DPE


# CHARGEMENT DES PACKAGES 
install.packages("httr")
library(httr)
install.packages("jsonlite")
library(jsonlite)
install.packages("dplyr")
library(dplyr)
install.packages("readr")
library(readr)

# --- 2. DÉFINITION DU RÉPERTOIRE DE TRAVAIL ---
# !!! ATTENTION: Changez ceci pour VOTRE dossier (pas celui de 'viole' ou le dossier 'Temp') !!!
# Par exemple: "C:/Users/alshawwa/Desktop/ExtractionDPENordSud"
setwd("C:/Users/alshawwa/Desktop/Projet R shiny/ExtractionDPENordSud")


# --- 3. DÉFINITION DES CIBLES ---

# DÉFINITION MANUELLE DES CODES POSTAUX (Nord vs Sud)
codes_nord <- c(59000, 59800, 59100, 59200, 59140)
codes_sud <- c(66000, 66100, 66140, 66240, 66700)
Code_postal <- c(codes_nord, codes_sud)

# CREATION DU DATAFRAME FINAL COMBINÉ
new_df=data.frame()

# SPÉCIFICATION DE L'URL DE BASE DE L'API
base_api_url = "https://data.ademe.fr/data-fair/api/v1/datasets/"

# Liste des datasets à interroger
dataset_ids <- c("dpe03existant", "dpe02neuf")

# Définition des colonnes COMMUNES (20)
select_fields_communs = paste(
  "code_postal_ban", "nom_commune_ban", "code_departement_ban", # Localisation (3)
  "etiquette_dpe", "etiquette_ges",                           # Performance Globale (2)
  "conso_5_usages_par_m2_ep", "emission_ges_5_usages_par_m2",   # Performance Clé (2)
  "conso_chauffage_ep", "conso_ecs_ep",                         # Consommations spécifiques (2)
  "besoin_chauffage",                                           # Besoin (1)
  "type_batiment", "surface_habitable_logement", "zone_climatique", # Caractéristiques Bâti (3)
  "type_energie_principale_chauffage", "type_energie_principale_ecs", # Système (2)
  "qualite_isolation_murs", "qualite_isolation_menuiseries",     # Isolation (2)
  "coordonnee_cartographique_x_ban", "coordonnee_cartographique_y_ban", # Coordonnées Géo (2)
  "date_reception_dpe",                                       # Date (1)
  sep=","
)

# Définition des listes de sélection spécifiques
select_fields_existant <- paste(select_fields_communs, "annee_construction", sep=",")
select_fields_neuf <- select_fields_communs


# --- 4. BOUCLE PRINCIPALE (POUR TOUS LES CODES POSTAUX) ---
for (cp in Code_postal){
  
  print(paste("--- Début CP : ", cp, " ---"))
  
  # Boucle secondaire sur les datasets (Existant et Neuf)
  for (dataset_id in dataset_ids) {
    
    current_url <- paste0(base_api_url, dataset_id, "/lines")
    source_label <- ifelse(dataset_id == "dpe03existant", "Existant (dpe03)", "Neuf (dpe02)")
    print(paste("  -> Interrogation Dataset:", dataset_id, "(", source_label, ")"))
    
    # CHOIX DE LA BONNE LISTE DE COLONNES
    current_select_fields <- ifelse(dataset_id == "dpe03existant", 
                                    select_fields_existant, 
                                    select_fields_neuf)
    
    # Paramètres de la requête
    params = list(
      select = current_select_fields, 
      size = 10000, 
      q = cp,
      q_fields = "code_postal_ban"
    )
    
    # Encodage des paramètres
    url_encoded = modify_url(current_url, query = params)
    
    # Effectuer la requête
    response = GET(url_encoded)
    
    # Afficher le statut de la réponse
    print(paste("  Statut réponse:", status_code(response)))
    
    # Gestion d'erreur
    if (status_code(response) != 200) {
      print(paste("  Erreur API pour", dataset_id, "CP", cp, ". Statut:", status_code(response)))
      next 
    }
    
    # Tentative de conversion JSON
    content = tryCatch({
      fromJSON(rawToChar(response$content), flatten = TRUE) 
    }, error = function(e) {
      print(paste("  Erreur parsing JSON pour", dataset_id, "CP", cp, ":", e$message))
      return(NULL) 
    })
    
    if (is.null(content)) { next }
    
    # Afficher le nombre total de lignes disponibles
    taille_totale = ifelse(!is.null(content$total), content$total, 0)
    print(paste("  Taille totale dispo. pour", dataset_id, ":", taille_totale))
    
    # Ajout des résultats
    if(!is.null(content$results) && (is.data.frame(content$results) || length(content$results) > 0)) {
      
      results_df <- content$results
      if(!is.data.frame(results_df) && length(results_df) > 0) {
        results_df <- tryCatch(bind_rows(results_df), error = function(e) { NULL })
      } else if (length(results_df) == 0 && !is.data.frame(results_df)) {
        results_df <- data.frame() 
      }
      
      if (!is.null(results_df) && nrow(results_df) > 0) {
        print(paste("  Nombre de lignes ajoutées:", nrow(results_df)))
        results_df$type_source <- source_label
        
        # HARMONISATION DES COLONNES
        if (dataset_id == "dpe02neuf") {
          results_df$annee_construction <- NA_integer_ 
        }
        
        tryCatch({
          new_df = bind_rows(new_df, results_df) 
        }, error = function(e_bind) {
          print(paste("  Erreur lors de bind_rows pour", dataset_id, "CP", cp, ":", e_bind$message))
        })
      } else {
        print("  Aucune ligne de résultat valide trouvée.")
      }
    } else {
      print("  Aucun résultat ($results) dans la réponse.")
    }
    
    print(paste("  --- Fin Dataset : ", dataset_id, " ---"))
    # Pause
    Sys.sleep(0.5) 
    
  } # Fin boucle datasets
  
  print(paste("--- Fin CP : ", cp, " ---"))
  # Pause
  Sys.sleep(1.0) 
  
} # Fin boucle CPs

# --- 5. Nettoyage final avant sauvegarde ---
print("Nettoyage final du dataframe combiné...")
if (!is.null(new_df) && nrow(new_df) > 0) { 
  list_cols <- sapply(new_df, is.list)
  if(any(list_cols)) {
    print("Colonnes liste détectées. Conversion en char:")
    list_col_names <- names(new_df)[list_cols]
    for(col_name in list_col_names) {
      new_df[[col_name]] <- sapply(new_df[[col_name]], function(x) {
        if (is.null(x) || length(x) == 0) return(NA_character_)
        if (is.list(x) || length(x) > 1) return(paste(unlist(x), collapse = "; ")) 
        return(as.character(x))
      })
    }
  } else { print("Pas de colonnes liste détectées.") }
} else { print("Dataframe combiné vide.") }


# Afficher dimensions
print(paste("Dimension finale COMBINÉE (avant distinct):", ifelse(!is.null(new_df) && nrow(new_df)>0, nrow(new_df), 0), "lignes et", ifelse(!is.null(new_df) && ncol(new_df)>0, ncol(new_df), 0), "colonnes"))

# --- 6. Déduplication et Sauvegarde ---
if (!is.null(new_df) && nrow(new_df) > 0) {
  
  # ---!!! NOUVELLE LIGNE AJOUTÉE ICI !!!---
  new_df$typologie_logement <- ifelse(
    new_df$type_source == "Neuf (dpe02)", 
    "neuf", 
    "existant"
  )
  print("Nouvelle colonne 'typologie_logement' ajoutée.")
  # ---!!! FIN DE L'AJOUT !!!---
  
  tryCatch({ 
    new_df = distinct(new_df) 
    print(paste("Dimension COMBINÉE (après distinct):", nrow(new_df))) 
  }, error = function(e){print(paste(" Erreur distinct:", e$message))})
  
  print("Noms colonnes COMBINÉES (toutes):"); print(names(new_df)) 
  
  # ---!!! NOM DE FICHIER MODIFIÉ ICI !!!---
  file_name_csv <- "df_nord_sud_COMBINE_COMPLET.csv"
  
  tryCatch({
    readr::write_csv(new_df, file = file_name_csv, na = "") 
    print(paste("Données COMBINÉES COMPLÈTES sauvegardées dans", file_name_csv))
  }, error = function(e_write) { print(paste("Erreur écriture CSV combiné:", e_write$message)) })
} else { 
  print("Aucune donnée COMBINÉE collectée.") 
}