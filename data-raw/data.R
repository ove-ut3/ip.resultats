donnees <- insertion.pro::diplomes %>% 
  dplyr::filter(a_enqueter == "Oui",
                annee <= 2015,
                type_diplome %in% c("DUT", "LP", "Master")) %>% 
  tidyr::unnest_legacy(code_composante) %>% 
  dplyr::mutate(code_etape = apogee::histo_etape_succ_2(code_etape)) %>% 
  tidyr::unnest_legacy(code_etape) %>% 
  dplyr::mutate(mention = apogee::hier_etape_mention(code_etape)) %>% 
  tidyr::unnest_legacy(mention) %>% 
  dplyr::mutate_at("mention", apogee::histo_mention_diplome_succ) %>% 
  dplyr::mutate(annee = apogee::annee_u(annee, fichier = TRUE) %>% 
                  factor(levels = rev(unique(.))),
                type_diplome = apogee::hier_type_diplome_parent(type_diplome) #%>% 
                  #dplyr::recode("LP" = "Licence pr")
                  ,
                composante = apogee::hier_composante_parent(code_composante) %>% 
                  apogee::lib_composante(),
                departement = apogee::lib_etape(code_etape, prefixe = NA_character_, suffixe = NA_character_),
                mention = apogee::lib_mention_diplome(mention),
                mention = dplyr::if_else(
                  !apogee::temoin_mention_actif(mention) | is.na(mention),
                  "Mentions non-reconduites", mention) %>% 
                  factor(levels = c(unique(sort(.[which(. != "Mentions non-reconduites")])), "Mentions non-reconduites")),
                secteur = apogee::hier_etape_secteur(code_etape),
                formation = apogee::lib_etape(code_etape, prefixe = "diplome", suffixe = c("ville", "option", "particularite")),
                formation = dplyr::if_else(!apogee::temoin_etape_actif(code_etape), "Formations non-reconduites", formation) %>% 
                  factor(levels = c(unique(sort(.[which(. != "Formations non-reconduites")])), "Formations non-reconduites"))) %>% 
  dplyr::select(annee, type_diplome, composante, code_etape, mention, departement, secteur, formation, identifiant, sexe, regime_inscription, code_bourse, code_nationalite, code_type_diplome_origine) %>% 
  dplyr::left_join(insertion.pro::reponses %>% 
                     dplyr::select(-annee, -type_diplome, -code_etape, -regime_inscription) %>% 
                     dplyr::mutate(repondant = 1),
                   by = "identifiant") %>% 
  tidyr::replace_na(list(repondant = 0)) %>% 
  dplyr::mutate(
    regime_inscription = regime_inscription %>% 
      dplyr::recode(
        "FI" = "Formation initiale",
        "AP" = "Contrat d'apprentissage",
        "CP" = "Contrat de professionnalisation",
        "FC" = "Formation continue",
        "VAE" = "Validation des acquis de l'expérience (VAE)"
      ) %>% 
      factor(levels = c("Formation initiale", "Contrat d'apprentissage", "Contrat de professionnalisation", "Formation continue", "Validation des acquis de l'expérience (VAE)")),
    emploi_premier_duree_recherche = dplyr::if_else(emploi_premier_duree_recherche > 30, 30L, emploi_premier_duree_recherche),
    emploi_n2_type = as.character(emploi_n2_type),
    emploi_n2_type = dplyr::case_when(
        emploi_n2_type == "Profession libérale, indépendant, chef-fe d'entreprise, auto-entrepreneur" ~ "Indépendant",
        emploi_n2_type %in% c("Fonctionnaire", "CDI", "CDD") ~ emploi_n2_type,
        TRUE ~ "Autre"
    ),
    emploi_n2_type = factor(emploi_n2_type, levels = c("CDI", "Fonctionnaire", "Indépendant", "CDD", "Autre")),
    emploi_n2_niveau = as.character(emploi_n2_niveau),
    emploi_n2_niveau = dplyr::case_when(
      emploi_n2_niveau %in% c("Agriculteur", "Artisan, commerçant, chef d'entreprise", "Profession libérale") ~ "Indépendant",
      emploi_n2_niveau == "Fonction publique - Catégorie A" ~ "Cadre",
      emploi_n2_niveau == "Fonction publique - Catégorie B" ~ "Technicien-ne",
      emploi_n2_niveau %in% c("Fonction publique - Catégorie C", "Ouvrier-ère") ~ "Employé-e"
    ),
    emploi_n2_niveau = factor(emploi_n2_niveau, levels = c("Cadre", "Technicien-ne", "Employé-e", "Indépendant")),
    emploi_n2_salaire = dplyr::if_else(!is.na(emploi_n2_primes),
                                       emploi_n2_salaire + emploi_n2_primes / 12,
                                       emploi_n2_salaire)) %>% 
  dplyr::mutate_at(c("emploi_premier_departement", "emploi_n2_departement"), stringr::str_pad, 3, "left", "0") %>% 
  dplyr::mutate(emploi_premier_localisation = geographie::hier_departement_region(emploi_premier_departement),
                emploi_n2_localisation = geographie::hier_departement_region(emploi_n2_departement)) %>% 
  dplyr::mutate_at(c("emploi_premier_localisation", "emploi_n2_localisation"),
                   dplyr::recode, "76" = "Occitanie<br>(hors Haute-Garonne)", "11" = "Ile-de-France", "99" = "Etranger", .default = "Autres régions", .missing = NA_character_) %>% 
  dplyr::mutate(emploi_premier_localisation = dplyr::if_else(emploi_premier_departement == "031", "Haute-Garonne", emploi_premier_localisation),
                emploi_n2_localisation = dplyr::if_else(emploi_n2_departement == "031", "Haute-Garonne", emploi_n2_localisation)) %>% 
  dplyr::mutate_at(c("emploi_premier_localisation", "emploi_n2_localisation"),
                   factor, levels = c("Haute-Garonne", "Occitanie<br>(hors Haute-Garonne)", "Ile-de-France", "Autres régions", "Etranger")) %>% 
  dplyr::mutate_at("emploi_n2_type_ent", as.character) %>% 
  dplyr::mutate(
    emploi_n2_type_ent = dplyr::case_when(
      emploi_n2_type_ent %in% c("Profession libérale ou indépendant", "Organisation internationale", "Société d'économie mixte", "Particulier") ~ "Autre",
      TRUE ~ emploi_n2_type_ent
    )
  )
  

usethis::use_data(donnees, overwrite = TRUE)
