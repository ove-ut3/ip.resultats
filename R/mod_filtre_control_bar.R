# Module UI
  
#' @title   mod_filtre_control_bar_ui and mod_filtre_control_bar_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_filtre_control_bar
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_filtre_control_bar_ui <- function(id){
  ns <- NS(id)
  
  default_params_group <- list(
    annee = list(inputId = "annee", title = "Ann\u00e9e"),
    composante = list(inputId = "composante", title = "Composante"),
    formation = list(inputId = "formation", title = "Formation"),
    regime = list(inputId = "regime", title = "R\u00e9gime d'inscription")
  )
  
  if (golem::get_golem_options("diplome") == "DUT") {
    params_group <- c(
      default_params_group[1:2],
      list(
        campus = list(inputId = "campus", title = "Campus"),
        departement = list(inputId = "departement", title = "D\u00e9partement"),
        secteur = list(inputId = "secteur", title = "Secteur")
      ),
      default_params_group[3:4]
    )
  } else if (golem::get_golem_options("diplome") == "LP") {
    params_group <- c(
      default_params_group[1:2],
      list(
        campus = list(inputId = "campus", title = "Campus"),
        mention = list(inputId = "mention", title = "Mention"),
        secteur = list(inputId = "secteur", title = "Secteur")
      ),
      default_params_group[3:4]
    )
  } else if (golem::get_golem_options("diplome") == "Master") {
    params_group <- c(
      default_params_group[1:2],
      list(
        mention = list(inputId = "mention", title = "Mention")
      ),
      default_params_group[3:4]
    )
  }
  
  tagList(
    selectizeGroupUI(
      ns("filtre-donnees"),
      inline = FALSE,
      btn_label = "Supprimer les filtres",
      params = params_group
    )
  )
}
    
# Module Server
    
#' @rdname mod_filtre_control_bar
#' @export
#' @keywords internal
    
mod_filtre_control_bar_server <- function(input, output, session, rv){
  ns <- session$ns
  
  filter_vars <- switch(
    golem::get_golem_options("diplome"),
    DUT = c("annee", "composante", "campus", "departement", "secteur", "formation", "regime"),
    LP = c("annee", "composante", "campus", "mention", "secteur", "formation", "regime"),
    Master = c("annee", "composante", "mention", "formation", "regime")
  )
  
  rv$dt_filtre <- callModule(
    module = selectizeGroupServer,
    id = "filtre-donnees",
    data = dplyr::filter(golem::get_golem_options("data"), .data$type_diplome == golem::get_golem_options("diplome")),
    vars = filter_vars
  )
  
  onRestored(function(state) {

    input_list <- state$input %>%
      .[stringr::str_detect(names(.), "-(selectized|reset_all)$", negate = TRUE)] %>%
      { Filter(Negate(is.null), .) }
    
    if (length(input_list) >= 1) {
      for (num_filtre in seq_along(input_list)) {
        updateSelectInput(
          session,
          inputId = names(input_list)[num_filtre],
          selected = input_list[[num_filtre]]
        )
      }
    }

  })
  
  rv$filter_vars <- filter_vars
  rv$inputs <- input

  return(rv)
}
