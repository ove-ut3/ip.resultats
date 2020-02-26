#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(credentials, data, diplome) {
  
 with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server
    ), 
    golem_opts = list(
      credentials = credentials,
      data = data,
      diplome = diplome
    )
  )
  
}
