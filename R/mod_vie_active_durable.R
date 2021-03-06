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
          "Ann\u00e9es s\u00e9lectionn\u00e9es",
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
        footer = HTML("<sup>1</sup> Sont retenus les dipl\u00f4m\u00e9s n'ayant pas interrompu deux ans ou plus leurs \u00e9tudes entre le baccalaur\u00e9at et l'obtention du dipl\u00f4me \u00e0 l'Universit\u00e9 Toulouse III - Paul Sabatier. Les r\u00e9sultats pr\u00e9sent\u00e9s concernent le public assimil\u00e9 \u00e0 la formation initiale.<br>
                    <sup>2</sup> Aucune poursuite d'\u00e9tudes pendant les 30 mois cons\u00e9cutifs \u00e0 l'obtention du dipl\u00f4me. Les inactifs sont inclus.")
      )
    ),
    fluidRow(
      tabBox(
        title = "Situation professionnelle \u00e0 6, 18 et 30 mois", width = 12,
        tabPanel(
          "Ann\u00e9es s\u00e9lectionn\u00e9es",
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
      HTML("Dipl\u00f4m\u00e9s r\u00e9pondants<sup>1</sup>"), icon = icon("user-graduate"), color = "black"
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
      dplyr::filter(.data$repondant) %>% 
      dplyr::mutate_at("annee", as.character) %>%
      dplyr::mutate(vie_active_durable = dplyr::if_else(.data$parcours == "Vie active durable", "oui", "non")) %>% 
      dplyr::count(.data$annee, .data$vie_active_durable) %>% 
      tidyr::spread(.data$vie_active_durable, .data$n, fill = 0) %>% 
      dplyr::mutate(pct = .data$oui / (.data$oui + .data$non))
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s"),
      need(length(unique(data$annee)) >= 2, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    graphr::shiny_line_percent(
      data$annee, data$pct,
      title_x = "Ann\u00e9e universitaire", title_y = "Taux de vie active durable",
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
      title_x = "Nombre de mois apr\u00e8s la dipl\u00f4mation", alpha = 0.67,
      colors = c("#313131", "#585858", "#7e7e7e"),
      font_family = golem::get_golem_options("graph_font_family")
    )
    
  })
  
  output$input_situation_pro_histo <- renderUI({
    
    selectInput(
      ns("filtre_situation_pro_histo"),
      label = "Situation \u00e0 :",
      choices = c("30 mois", "18 mois", "6 mois")
    )
    
  })
  
  output$situation_pro_histo <- plotly::renderPlotly({
    
    req(input$filtre_situation_pro_histo)
    
    data <- rv$dt_evolution() %>%
      dplyr::mutate_at("annee", as.character) %>% 
      dplyr::filter(.data$parcours == "Vie active durable") %>% 
      dplyr::select(.data$annee, dplyr::matches("^situation_pro")) %>% 
      tidyr::gather("champ", "valeur", -.data$annee, na.rm = TRUE) %>% 
      dplyr::mutate_at(
        "champ", 
        dplyr::recode, 
        "situation_pro_n" = "6 mois",
        "situation_pro_n1" = "18 mois",
        "situation_pro_n2" = "30 mois"
      ) %>% 
      dplyr::filter(.data$champ %in% input$filtre_situation_pro_histo) %>% 
      dplyr::mutate_at(
        "valeur", 
        dplyr::recode, 
        "Promesse d'embauche" = "En recherche d'emploi"
      ) %>% 
      dplyr::mutate_at("valeur", factor, levels = c("En emploi", "En recherche d'emploi", "Inactif"))
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s"),
      need(length(unique(data$annee)) >= 2, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    graphr::shiny_areas_evolution(
      data$annee, data$valeur,
      title_x = "Ann\u00e9e universitaire",
      colors = c("#313131", "#585858", "#7e7e7e"),
      font_family = golem::get_golem_options("graph_font_family")
    )
    
  })
  
}
