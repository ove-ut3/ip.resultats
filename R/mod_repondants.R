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
        valueBoxOutput(ns("tx_reponse"), width = 4),
        footer = HTML("<sup>1</sup> Promotion complète, y compris le public assimilé à la formation continue.<br>
                      <sup>2</sup> Répondants, y compris le public assimilé à la formation continue.")
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
      HTML("Diplômés<sup>1</sup>"), icon = icon("user-graduate")
    )
  })
  
  output$repondants <- renderValueBox({
    valueBox(
      nrow(rv$dt_reponses()) %>% scales::number(big.mark = "\u202F"),
      HTML("Répondants<sup>2</sup>"), icon = icon("clipboard-check")
    )
  })
  
  output$tx_reponse <- renderValueBox({
    valueBox(
      scales::percent(nrow(rv$dt_reponses()) / nrow(rv$dt_diplomes()), suffix = NULL),
      "Taux de réponse", icon = icon("percent")
    )
  })
  
  output$repondants_histo <- plotly::renderPlotly({
    
    data <- rv$dt_evolution() %>%
      dplyr::mutate(repondant = dplyr::if_else(repondant == 1, "oui", "non", "non")) %>% 
      dplyr::mutate_at("annee", as.character) %>%
      dplyr::count(annee, repondant) %>% 
      tidyr::spread(repondant, n, fill = 0) %>% 
      dplyr::mutate(pct = oui / (oui + non) * 100)
    
    graphr::shiny_line_percent(
      data$annee, data$pct,
      title_x = "Année universitaire", title_y = "Taux de répondants",
      hovertext = paste("Taux de répondants: ", scales::percent(data$pct / 100, suffix = "\u202F%"))
    )
    
  })
  
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
