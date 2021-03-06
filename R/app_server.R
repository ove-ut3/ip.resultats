#' @import shiny
app_server <- function(input, output, session) {
  # List the first level callModules here

  shinyjs::runjs("$('body').addClass('control-sidebar-open');")
  
  rv <- reactiveValues()
  
  rv <- callModule(mod_filtre_control_bar_server, "filtre_control_bar_ui", rv)

  callModule(mod_accueil_server, "accueil_ui")
  
  callModule(
    shiny.modules::selected_filters_server, "selected_filters_ui",
    group_inputs = rv$inputs,
    label_none = "Aucun",
    labels = c(
      annee = "Ann\u00e9e",
      composante = "Composante",
      departement = "D\u00e9partement",
      mention = "Mention",
      secteur = "Secteur",
      formation = "Formation"
    )
  )
  
  rv$dt_evolution <- reactive({
    
    golem::get_golem_options("data") %>% 
      dplyr::semi_join(rv$dt_filtre(), by = rv$filter_vars[-which(rv$filter_vars == "annee")]) %>% 
      dplyr::group_by(.data$annee, .data$code_etudiant) %>% 
      dplyr::filter(dplyr::row_number() == 1) %>% 
      dplyr::ungroup()
    
  })
  
  rv$dt_diplomes <- reactive({
    
    rv$dt_filtre() %>% 
      dplyr::group_by(.data$annee, .data$code_etudiant) %>% 
      dplyr::filter(dplyr::row_number() == 1) %>% 
      dplyr::ungroup()
  })
  
  rv$dt_reponses <- reactive({
    rv$dt_diplomes() %>% 
      dplyr::filter(.data$repondant)
  })
  
  rv$dt_etudes <- reactive({
    rv$dt_reponses() %>% 
      dplyr::filter(.data$parcours %in% c("Poursuite d'\u00e9tudes directe", "Reprise d'\u00e9tudes"))
  })
  
  rv$dt_vad <- reactive({
    rv$dt_reponses() %>% 
      dplyr::filter(.data$parcours == "Vie active durable")
  })
  
  rv$dt_emploi_occupe <- reactive({
    rv$dt_vad() %>% 
      dplyr::filter(.data$emploi_occupe == "Oui")
  })
  
  rv$dt_emploi_30mois <- reactive({
    rv$dt_vad() %>% 
      dplyr::filter(.data$situation_pro_n2 == "En emploi")
  })
  
  callModule(mod_chiffres_cles_server, "chiffres_cles_ui", rv)
  
  callModule(mod_poursuite_etudes_server, "poursuite_etudes_ui", rv)
  
  callModule(mod_vie_active_durable_server, "vie_active_durable_ui", rv)
  
  callModule(mod_emploi_premier_server, "emploi_premier_ui", rv)
  
  callModule(mod_emploi_30mois_poste_server, "emploi_30mois_poste_ui", rv)
  
  callModule(mod_emploi_30mois_employeur_server, "emploi_30mois_employeur_ui", rv)
  
  callModule(mod_emploi_30mois_adequation_server, "emploi_30mois_adequation_ui", rv)
  
  callModule(mod_repondants_server, "repondants_ui", rv)

}
