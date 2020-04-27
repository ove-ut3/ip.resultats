#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(data, diplome) {
  
 with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server
    ), 
    golem_opts = list(
      data = data,
      diplome = diplome
    )
  )
  
}
