# Module UI
  
#' @title   mod_temoignages_ui and mod_temoignages_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_temoignages
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_temoignages_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Quel témoignage pourriez-vous apporter à un-e étudiant-e souhaitant s'orienter vers un diplôme de votre spécialité ?", width = 12,
        htmlOutput(ns("validate")),
        DT::DTOutput(ns("temoignages"))
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_temoignages
#' @export
#' @keywords internal
    
mod_temoignages_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$validate <- renderText({
    
    validate(
      need(length(rv$inputs[["filtre-donnees-formation"]]) == 1, "Une et une seule formation doit être sélectionnée pour afficher les témoignages des anciens diplômés.")
    )
    
    validate(
      need(nrow(rv$dt_diplomes()) >= 3, "Il n'y a pas suffisamment de diplômés dans cette formation.")
    )
    
  })
  
  output$temoignages <- DT::renderDT({
    
    validate(
      need(length(rv$inputs[["filtre-donnees-formation"]]) == 1, "")
    )
    
    validate(
      need(nrow(rv$dt_diplomes()) >= 3, "")
    )
    
    rv$dt_reponses() %>% 
      tidyr::drop_na(temoignage) %>% 
      dplyr::select(temoignage) %>% 
      DT::datatable(rownames = NULL, colnames = NULL, options = list(dom = 't', bSort=FALSE), selection = "none")
    
  })
  
}
