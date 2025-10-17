#Librairies
library(httr)
library(jsonlite)

#URL de base de l’API ADEME
base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe03existant/lines"


#Paramètres de la requête
params <- list(
  page = 1,
  size = 1000,
  select = paste(
    "numero_dpe",
    "date_etablissement_dpe",
    "date_reception_dpe",
    "type_batiment",
    "typologie_logement",
    "surface_habitable_logement",
    "code_postal_ban",
    "code_departement_ban",
    "nom_commune_ban",
    "coordonnee_cartographique_x_ban",
    "coordonnee_cartographique_y_ban",
    "etiquette_dpe",
    "etiquette_ges",
    "conso_5_usages_ep",
    "conso_5_usages_par_m2_ep",
    "emission_ges_5_usages",
    "emission_ges_5_usages_par_m2",
    "type_energie_principale_chauffage",
    "type_installation_chauffage",
    "type_energie_principale_ecs",
    "presence_production_pv",
    "production_electricite_pv_kwhep_par_an",
    "version_dpe",
    "zone_climatique",
    sep = ","
  ),
    
    
   
  qs = 'code_departement_ban:"69" AND date_reception_dpe:[2023-06-01 TO 2023-08-30]'
)


#Boucle pour télécharger plusieurs pages
all_data <- data.frame()  # data frame vide pour accumuler les pages

for (page_num in 1:5) {
  params$page <- page_num
  url_encoded <- modify_url(base_url, query = params)
  cat("Page", page_num, "->", url_encoded, "\n")
  
  response <- GET(url_encoded)
  
  if (status_code(response) == 200) {
    content_json <- fromJSON(rawToChar(response$content), flatten = TRUE)
    
    if (!is.null(content_json$results)) {
      df <- content_json$results
    } else {
      df <- content_json$result
    }
    
    if (nrow(df) > 0) {
      all_data <- rbind(all_data, df)
    } else {
      cat("Page vide, arrêt.\n")
      break
    }
    
  } else {
    cat("Erreur HTTP :", status_code(response), "\n")
    break
  }
}

# Vérifications
cat("Total de lignes téléchargées :", nrow(all_data), "\n")
head(all_data)

all_data$typelog="Ancien"

# Librairies
library(httr)
library(jsonlite)

# URL de base de l’API (logements neufs)
base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/g3cgx7jb3cmys5voxz1mrm22/lines"

# Paramètres de la requête
params <- list(
  page = 1,
  size = 10000,  # 10 000 lignes par page
  select = "numero_dpe,code_postal_ban,code_departement_ban,nom_commune_ban,etiquette_dpe,etiquette_ges,date_reception_dpe,date_etablissement_dpe,type_batiment,typologie_logement,surface_habitable_logement,conso_5_usages_ep,conso_5_usages_par_m2_ep,emission_ges_5_usages,emission_ges_5_usages_par_m2,type_energie_principale_chauffage,type_installation_chauffage,type_energie_principale_ecs,presence_production_pv,production_electricite_pv_kwhep_par_an,version_dpe,zone_climatique",
  qs = 'code_departement_ban:"69" AND date_reception_dpe:[2024-01-01 TO 2024-03-31]'
)

# Boucle pour télécharger plusieurs pages
all_data_neuf <- data.frame()  # data frame vide

for (page_num in 1:5) {  # 5 pages = ~50 000 lignes
  params$page <- page_num
  url_encoded <- modify_url(base_url, query = params)
  cat("Page", page_num, "->", url_encoded, "\n")
  
  response <- GET(url_encoded)
  
  if (status_code(response) == 200) {
    content_json <- fromJSON(rawToChar(response$content), flatten = TRUE)
    
    if (!is.null(content_json$results)) {
      df <- content_json$results
    } else {
      df <- content_json$result
    }
    
    if (nrow(df) > 0) {
      all_data_neuf <- rbind(all_data_neuf, df)
    } else {
      cat("Page vide, arrêt.\n")
      break
    }
    
  } else {
    cat("Erreur HTTP :", status_code(response), "\n")
    break
  }
}

# Vérifications
cat("Total de lignes téléchargées :", nrow(all_data_neuf), "\n")
head(all_data_neuf)

