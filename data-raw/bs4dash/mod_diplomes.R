# Module UI
  
#' @title   mod_diplomes_ui and mod_diplomes_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_diplomes
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_diplomes_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      
    )
  )
}
    
# Module Server
    
#' @rdname mod_diplomes
#' @export
#' @keywords internal
    
mod_diplomes_server <- function(input, output, session, dt_diplomes){
  ns <- session$ns
  
  output$diplomes <- renderValueBox({
    
    valueBox(
      nrow(dt_diplomes()),
      "Nombre de diplômes", icon = icon("user-graduate"), color = "yellow"
    )
    
  })
  
  output$diplomes2 <- renderValueBox({
    
    valueBox(
      nrow(dt_diplomes()),
      "Nombre de diplômes", icon = icon("user-graduate"), color = "yellow"
    )
    
  })
  
  output$diplomes_histo <- plotly::renderPlotly({
    
    data <- ip.resultats::donnees %>%
      dplyr::semi_join(dt_diplomes(), by = c("type_diplome", "composante", "mention_departement", "secteur", "formation")) %>%
      dplyr::count(annee) %>%
      dplyr::mutate_at("annee", as.character) %>% 
      dplyr::arrange(annee) %>% 
      dplyr::mutate(base_100 = divr::base_100(n))
    
    data %>% 
      plotly::plot_ly(
        x = ~annee, 
        hoverinfo = "text",
        hovertext = ~paste("Effectif: ", n)
      ) %>%
      plotly::add_lines(y = ~base_100, name = "linear", line = list(shape = "linear")) %>% 
      plotly::layout(
        xaxis = list(title = "Année universitaire"),
        yaxis = list(title = paste("Nombre de diplômés <sup>1</sup>")),
        margin = list(r = 50, b = 50),
        annotations = list(text = paste("<sup>1</sup> Base 100, année", data$annee[1]), xref='paper', yref='paper',
                           x = 1.08, y = -0.16, xanchor='right', yanchor='auto',
                           showarrow = FALSE)
      )
        
  })
  
  output$femmes <- renderValueBox({
    
    valueBox(
      caractr::str_percent_fr(
        nrow(dplyr::filter(dt_diplomes(), sexe == "F")) / nrow(dt_diplomes())
      ),
      "Taux de femmes", icon = icon("female"), color = "yellow"
    )
    
  })
  
  # output$femmes_histo <- plotly::renderPlotly({
  #   
  #   data <- ip.resultats::donnees %>%
  #     dplyr::semi_join(dt_diplomes(), by = c("type_diplome", "composante", "mention_departement", "secteur", "formation")) %>%
  #     dplyr::count(annee) %>%
  #     dplyr::mutate_at("annee", as.character) %>% 
  #     dplyr::arrange(annee) %>% 
  #     dplyr::mutate(base_100 = divr::base_100(n))
  #   
  #   data %>% 
  #     plotly::plot_ly(
  #       x = ~annee, 
  #       hoverinfo = "text",
  #       hovertext = ~paste("Effectif: ", n)
  #     ) %>%
  #     plotly::add_lines(y = ~base_100, name = "linear", line = list(shape = "linear")) %>% 
  #     plotly::layout(
  #       xaxis = list(title = "Année universitaire"),
  #       yaxis = list(title = paste("Nombre de diplômés <sup>1</sup>")),
  #       margin = list(r = 50, b = 50),
  #       annotations = list(text = paste("<sup>1</sup> Base 100, année", data$annee[1]), xref='paper', yref='paper',
  #                          x = 1.08, y = -0.16, xanchor='right', yanchor='auto',
  #                          showarrow = FALSE)
  #     )
  #   
  # })
  
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
