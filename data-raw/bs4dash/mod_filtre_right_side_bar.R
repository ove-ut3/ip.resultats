# Module UI
  
#' @title   mod_filtre_right_side_bar_ui and mod_filtre_right_side_bar_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_filtre_right_side_bar
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_filtre_right_side_bar_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyWidgets::selectizeGroupUI(
      ns("filtre-donnees"),
      inline = FALSE,
      btn_label = "Supprimer les filtres",
      params = list(
        annee = list(inputId = "annee", title = "Année"),
        type_diplome = list(inputId = "type_diplome", title = "Type diplôme"),
        composante = list(inputId = "composante", title = "Composante"),
        mention_departement = list(inputId = "mention_departement", title = "Mention / Département"),
        secteur = list(inputId = "secteur", title = "Secteur"),
        formation = list(inputId = "formation", title = "Formation")
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_filtre_right_side_bar
#' @export
#' @keywords internal
    
mod_filtre_right_side_bar_server <- function(input, output, session){
  ns <- session$ns
  
  donnees <- callModule(
    module = shinyWidgets::selectizeGroupServer,
    id = "filtre-donnees",
    data = ip.resultats::donnees,
    vars = c("annee", "type_diplome", "composante", "mention_departement", "secteur", "formation")
  )

  return(donnees)
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
