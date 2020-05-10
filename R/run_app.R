#' Run the Shiny Application
#' 
#' @param data Table de données source
#' @param diplome Type de diplôme
#' @param graph_font_family Police de caractère utilisée
#' 
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(data, diplome, graph_font_family = NULL) {
  
 with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      enableBookmarking = "server"
    ), 
    golem_opts = list(
      data = data,
      diplome = diplome,
      graph_font_family = graph_font_family
    )
  )
  
}
