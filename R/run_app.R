#' Run the Shiny Application
#' 
#' @param data \dots
#' @param diplome \dots
#' @param graph_font_family \dots
#' 
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(data, diplome, graph_font_family = NULL) {
  
 with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server
    ), 
    golem_opts = list(
      data = data,
      diplome = diplome,
      graph_font_family = graph_font_family
    )
  )
  
}
