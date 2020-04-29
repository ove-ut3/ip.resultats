# Module UI
  
#' @title   mod_vie_active_durable_ui and mod_vie_active_durable_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_vie_active_durable
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_vie_active_durable_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      tabBox_footer(
        title = "Vie active durable", width = 12,
        tabPanel(
          "Années sélectionnées",
          fluidRow(
            valueBoxOutput(ns("diplomes"), width = 4),
            valueBoxOutput(ns("vie_active_durable"), width = 4),
            valueBoxOutput(ns("tx_vie_active_durable"), width = 4)
          )
        ),
        tabPanel(
          "\u00C9volution",
          plotly::plotlyOutput(ns("vie_active_durable_histo"))
        ),
        footer = HTML("<sup>1</sup> Sont retenus les diplômés n'ayant pas interrompu deux ans ou plus leurs études entre le baccalauréat et l'obtention du diplôme à l'Université Toulouse III - Paul Sabatier. Les résultats présentés concernent le public assimilé à la formation initiale.<br>
                    <sup>2</sup> Aucune poursuite d'études pendant les 30 mois consécutifs à l'obtention du diplôme. Les inactifs sont inclus.")
      )
    ),
    fluidRow(
      tabBox(
        title = "Situation professionnelle à 6, 18 et 30 mois", width = 12,
        tabPanel(
          "Années sélectionnées",
          plotly::plotlyOutput(ns("situation_pro"))
        ),
        tabPanel(
          "\u00C9volution",
          div(
            style = "display: inline-block;",
            uiOutput(ns("input_situation_pro_histo")
          )
          ),
          plotly::plotlyOutput(ns("situation_pro_histo"))
        )
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_vie_active_durable
#' @export
#' @keywords internal
    
mod_vie_active_durable_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$diplomes <- renderValueBox({
    valueBox(
      nrow(rv$dt_reponses()) %>% scales::number(big.mark = "\u202F"),
      HTML("Diplômés répondants<sup>1</sup>"), icon = icon("user-graduate"), color = "black"
    )
  })
  
  output$vie_active_durable <- renderValueBox({
    valueBox(
      nrow(rv$dt_vad()) %>% scales::number(big.mark = "\u202F"),
      HTML("Vie active durable<sup>2</sup>"), icon = icon("user-tie"), color = "black"
    )
  })
  
  output$tx_vie_active_durable <- renderValueBox({
    valueBox(
      scales::percent(nrow(rv$dt_vad()) / nrow(rv$dt_reponses()), suffix = NULL),
      "Taux de vie active durable", icon = icon("percent"), color = "black"
    )
  })
  
  output$vie_active_durable_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::filter(repondant == 1) %>% 
      dplyr::mutate_at("annee", as.character) %>%
      dplyr::mutate(vie_active_durable = dplyr::if_else(parcours == "Vie active durable", "oui", "non")) %>% 
      dplyr::count(annee, vie_active_durable) %>% 
      tidyr::spread(vie_active_durable, n, fill = 0) %>% 
      dplyr::mutate(pct = oui / (oui + non))
    
    validate(
      need(nrow(data) >= 1, "Pas de données disponibles avec les filtres sélectionnés"),
      need(length(unique(data$annee)) >= 2, "Pas de données disponibles avec les filtres sélectionnés")
    )
    
    graphr::shiny_line_percent(
      data$annee, data$pct,
      title_x = "Année universitaire", title_y = "Taux de vie active durable",
      hovertext = paste("Taux de vie active durable: ", scales::percent(data$pct, suffix = "\u202F%", accuracy = 0.1, decimal.mark = ",")), 
      color = "#585858",
      font_family = golem::get_golem_options("graph_font_family")
    )
    
  })
  
  output$situation_pro <- plotly::renderPlotly({
    
    data <- rv$dt_vad() %>% 
      dplyr::select(dplyr::matches("^situation_pro")) %>% 
      tidyr::gather("champ", "valeur", na.rm = TRUE) %>% 
      dplyr::mutate_at(
        "champ", 
        dplyr::recode, 
        "situation_pro_n" = "6 mois",
        "situation_pro_n1" = "18 mois",
        "situation_pro_n2" = "30 mois"
      ) %>% 
      dplyr::mutate_at("champ", factor, levels = c("6 mois", "18 mois", "30 mois")) %>% 
      dplyr::mutate_at(
        "valeur", 
        dplyr::recode, 
        "Promesse d'embauche" = "En recherche d'emploi"
      ) %>% 
      dplyr::mutate_at("valeur", factor, levels = c("En emploi", "En recherche d'emploi", "Inactif"))
    
    graphr::shiny_barplot_vertical_multi(
      data$champ, data$valeur, 
      title_x = "Nombre de mois après la diplômation", alpha = 0.67,
      colors = c("#313131", "#585858", "#7e7e7e"),
      font_family = golem::get_golem_options("graph_font_family")
    )
    
  })
  
  output$input_situation_pro_histo <- renderUI({
    
    selectInput(
      ns("filtre_situation_pro_histo"),
      label = "Situation à :",
      choices = c("30 mois", "18 mois", "6 mois")
    )
    
  })
  
  output$situation_pro_histo <- plotly::renderPlotly({
    
    req(input$filtre_situation_pro_histo)
    
    data <- rv$dt_evolution() %>%
      dplyr::mutate_at("annee", as.character) %>% 
      dplyr::filter(parcours == "Vie active durable") %>% 
      dplyr::select(annee, dplyr::matches("^situation_pro")) %>% 
      tidyr::gather("champ", "valeur", -annee, na.rm = TRUE) %>% 
      dplyr::mutate_at(
        "champ", 
        dplyr::recode, 
        "situation_pro_n" = "6 mois",
        "situation_pro_n1" = "18 mois",
        "situation_pro_n2" = "30 mois"
      ) %>% 
      dplyr::filter(champ %in% input$filtre_situation_pro_histo) %>% 
      dplyr::mutate_at(
        "valeur", 
        dplyr::recode, 
        "Promesse d'embauche" = "En recherche d'emploi"
      ) %>% 
      dplyr::mutate_at("valeur", factor, levels = c("En emploi", "En recherche d'emploi", "Inactif"))
    
    validate(
      need(nrow(data) >= 1, "Pas de données disponibles avec les filtres sélectionnés"),
      need(length(unique(data$annee)) >= 2, "Pas de données disponibles avec les filtres sélectionnés")
    )
    
    graphr::shiny_areas_evolution(
      data$annee, data$valeur,
      title_x = "Année universitaire",
      colors = c("#313131", "#585858", "#7e7e7e"),
      font_family = golem::get_golem_options("graph_font_family")
    )
    
  })
  
}
