# Module UI
  
#' @title   mod_repondants_ui and mod_repondants_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_repondants
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_repondants_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Taux de r\u00e9ponse", width = 12,
        valueBoxOutput(ns("diplomes"), width = 4),
        valueBoxOutput(ns("repondants"), width = 4),
        valueBoxOutput(ns("tx_reponse"), width = 4)
      )
    ),
    fluidRow(
      box(title = "\u00C9volution du taux de r\u00e9pondants", width = 12,
        plotly::plotlyOutput(ns("repondants_histo"))
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_repondants
#' @export
#' @keywords internal
    
mod_repondants_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$diplomes <- renderValueBox({
    valueBox(
      nrow(rv$dt_diplomes()) %>% scales::number(big.mark = "\u202F"),
      HTML("Dipl\u00f4m\u00e9s"), icon = icon("user-graduate"), color = "olive"
    )
  })
  
  output$repondants <- renderValueBox({
    valueBox(
      nrow(rv$dt_reponses()) %>% scales::number(big.mark = "\u202F"),
      HTML("R\u00e9pondants"), icon = icon("clipboard-check"), color = "olive"
    )
  })
  
  output$tx_reponse <- renderValueBox({
    valueBox(
      scales::percent(nrow(rv$dt_reponses()) / nrow(rv$dt_diplomes()), suffix = NULL),
      "Taux de r\u00e9ponse", icon = icon("percent"), color = "olive"
    )
  })
  
  output$repondants_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::mutate(repondant = dplyr::if_else(repondant, "oui", "non", "non")) %>% 
      dplyr::mutate_at("annee", as.character) %>%
      dplyr::count(annee, repondant) %>% 
      tidyr::spread(repondant, n, fill = 0) %>% 
      dplyr::mutate(pct = oui / (oui + non))
    
    validate(
      need(nrow(data) >= 1, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s"),
      need(length(unique(data$annee)) >= 2, "Pas de donn\u00e9es disponibles avec les filtres s\u00e9lectionn\u00e9s")
    )
    
    graphr::shiny_line_percent(
      data$annee, data$pct,
      title_x = "Ann\u00e9e universitaire", title_y = "Taux de r\u00e9pondants",
      hovertext = paste("Taux de r\u00e9pondants: ", scales::percent(data$pct, suffix = "\u202F%", accuracy = 0.1, decimal.mark = ",")),
      color = "#a9a8a8",
      font_family = golem::get_golem_options("graph_font_family")
    )
    
  })
  
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
