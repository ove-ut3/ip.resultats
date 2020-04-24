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
        title = "Taux de réponse", width = 12,
        valueBoxOutput(ns("diplomes"), width = 4),
        valueBoxOutput(ns("repondants"), width = 4),
        valueBoxOutput(ns("tx_reponse"), width = 4)
      )
    ),
    fluidRow(
      box(title = "\u00C9volution du taux de répondants", width = 12,
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
      HTML("Diplômés"), icon = icon("user-graduate"), color = "light-blue"
    )
  })
  
  output$repondants <- renderValueBox({
    valueBox(
      nrow(rv$dt_reponses()) %>% scales::number(big.mark = "\u202F"),
      HTML("Répondants"), icon = icon("clipboard-check"), color = "light-blue"
    )
  })
  
  output$tx_reponse <- renderValueBox({
    valueBox(
      scales::percent(nrow(rv$dt_reponses()) / nrow(rv$dt_diplomes()), suffix = NULL),
      "Taux de réponse", icon = icon("percent"), color = "light-blue"
    )
  })
  
  output$repondants_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::mutate(repondant = dplyr::if_else(repondant, "oui", "non", "non")) %>% 
      dplyr::mutate_at("annee", as.character) %>%
      dplyr::count(annee, repondant) %>% 
      tidyr::spread(repondant, n, fill = 0) %>% 
      dplyr::mutate(pct = oui / (oui + non))
    
    graphr::shiny_line_percent(
      data$annee, data$pct,
      title_x = "Année universitaire", title_y = "Taux de répondants",
      hovertext = paste("Taux de répondants: ", scales::percent(data$pct, suffix = "\u202F%", accuracy = 0.1, decimal.mark = ",")),
      color = "#3c8dbc"
    )
    
  })
  
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
