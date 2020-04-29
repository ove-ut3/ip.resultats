# Module UI
  
#' @title   mod_emploi_30mois_employeur_ui and mod_emploi_30mois_employeur_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_emploi_30mois_employeur
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_emploi_30mois_employeur_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12, offset = 3,
        box(
          title = "Dipl\u00f4m\u00e9s en emploi",
          valueBoxOutput(ns("nombre_emploi"), width = 12)
        )
      )
    ),
    fluidRow(
      tabBox(
        title = "Type d'employeur",
        tabPanel(
          "Ann\u00e9es s\u00e9lectionn\u00e9es",
          plotly::plotlyOutput(ns("emploi_30mois_employeur_type"))
        ),
        tabPanel(
          "\u00C9volution",
          plotly::plotlyOutput(ns("emploi_30mois_employeur_type_histo"))
        )
      ),
      tabBox(
        title = "Localisation",
        tabPanel(
          "Ann\u00e9es s\u00e9lectionn\u00e9es",
          plotly::plotlyOutput(ns("emploi_30mois_employeur_localisation"))
        ),
        tabPanel(
          "\u00C9volution",
          plotly::plotlyOutput(ns("emploi_30mois_employeur_localisation_histo"))
        )
      )
    ),
    fluidRow(
      tabBox(
        title = "Taille de l'employeur",
        tabPanel(
          "Ann\u00e9es s\u00e9lectionn\u00e9es",
          plotly::plotlyOutput(ns("emploi_30mois_employeur_taille"))
        ),
        tabPanel(
          "\u00C9volution",
          plotly::plotlyOutput(ns("emploi_30mois_employeur_taille_histo"))
        )
      ),
      box(
        title = "Secteur d'activit\u00e9",
        plotly::plotlyOutput(ns("emploi_30mois_employeur_secteur"))
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_emploi_30mois_employeur
#' @export
#' @keywords internal
    
mod_emploi_30mois_employeur_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$nombre_emploi <- renderValueBox({
    valueBox(
      nrow(rv$dt_emploi_30mois()) %>% scales::number(big.mark = "\u202F"),
      "Nombre de dipl\u00f4m\u00e9s en emploi \u00e0 30 mois", icon = icon("user-tie"), color = "black"
    )
  })
  
  output$emploi_30mois_employeur_type <- plotly::renderPlotly({
    
    data <- rv$dt_emploi_30mois() %>%  
      tidyr::drop_na(emploi_n2_type_ent)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    data %>% 
      dplyr::pull(emploi_n2_type_ent) %>% 
      graphr::shiny_barplot_horizontal(
        color = c("#313131", "#4b4b4b", "#646464", "#7e7e7e", "#9a9a9a", "#cccccc"),
        alpha = 0.8,
        font_family = golem::get_golem_options("graph_font_family")
      )
    
  })
  
  output$emploi_30mois_employeur_type_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(
        parcours == "Vie active durable",
        situation_pro_n2 == "En emploi"
      ) %>% 
      tidyr::drop_na(emploi_n2_type_ent) %>% 
      dplyr::mutate_at("annee", as.character)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s"),
      need(length(unique(data$annee)) >= 2, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    graphr::shiny_areas_evolution(
      data$annee, data$emploi_n2_type_ent,
      title_x = "Ann\u00e9e universitaire",
      color = c("#313131", "#4b4b4b", "#646464", "#7e7e7e", "#9a9a9a", "#cccccc"),
      font_family = golem::get_golem_options("graph_font_family")
    )
    
  })
  
  output$emploi_30mois_employeur_localisation <- plotly::renderPlotly({
    
    data <- rv$dt_emploi_occupe() %>%
      tidyr::drop_na(emploi_n2_localisation)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    data %>% 
      dplyr::pull(emploi_n2_localisation) %>%
      graphr::shiny_barplot_horizontal(
        color = c("#313131", "#4b4b4b", "#646464", "#7e7e7e", "#9a9a9a"),
        alpha = 0.8,
        font_family = golem::get_golem_options("graph_font_family")
      )
    
  })
  
  output$emploi_30mois_employeur_localisation_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(
        parcours == "Vie active durable",
        emploi_occupe == "Oui"
      ) %>% 
      tidyr::drop_na(emploi_n2_localisation) %>% 
      dplyr::mutate_at("annee", as.character)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s"),
      need(length(unique(data$annee)) >= 2, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    graphr::shiny_areas_evolution(
      data$annee, data$emploi_n2_localisation,
      title_x = "Ann\u00e9e universitaire",
      color = c("#313131", "#4b4b4b", "#646464", "#7e7e7e", "#9a9a9a"),
      font_family = golem::get_golem_options("graph_font_family")
    )
    
  })
  
  output$emploi_30mois_employeur_taille <- plotly::renderPlotly({
    
    data <- rv$dt_emploi_occupe() %>%
      tidyr::drop_na(emploi_n2_taille_entreprise)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    data %>% 
      dplyr::pull(emploi_n2_taille_entreprise) %>%
      graphr::shiny_barplot_horizontal(
        color = c("#313131", "#4b4b4b", "#646464", "#7e7e7e", "#9a9a9a"),
        alpha = 0.8,
        font_family = golem::get_golem_options("graph_font_family")
      )
    
  })
  
  output$emploi_30mois_employeur_taille_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(
        parcours == "Vie active durable",
        emploi_occupe == "Oui"
      ) %>% 
      tidyr::drop_na(emploi_n2_taille_entreprise) %>% 
      dplyr::mutate_at("annee", as.character)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s"),
      need(length(unique(data$annee)) >= 2, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    graphr::shiny_areas_evolution(
      data$annee, data$emploi_n2_taille_entreprise,
      title_x = "Ann\u00e9e universitaire",
      color = c("#313131", "#4b4b4b", "#646464", "#7e7e7e", "#9a9a9a"),
      font_family = golem::get_golem_options("graph_font_family")
    )
    
  })
  
  output$emploi_30mois_employeur_secteur <- plotly::renderPlotly({
    
    validate(
      need(!is.null(rv$inputs[["filtre-donnees-formation"]]), "Au moins une formation doit \u00eatre s\u00e9lectionn\u00e9e")
    )
    
    data <- rv$dt_emploi_occupe() %>%
      tidyr::drop_na(emploi_n2_secteur_ent)
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    data %>% 
      dplyr::pull(emploi_n2_secteur_ent) %>%
      graphr::shiny_barplot_horizontal(
        colors = "#585858",
        alpha = 0.8,
        font_family = golem::get_golem_options("graph_font_family")
      )
    
  })
  
}
