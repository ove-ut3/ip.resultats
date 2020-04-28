#' @import shiny shinydashboard shinydashboardPlus
app_ui <- function() {
  
  list_menu_items <- list(
    menuItem("Accueil", tabName = "tab_home", icon = icon("home")),
    menuItem("Chiffres-clés", tabName = "tab_chiffres_cles", icon = icon("tachometer-alt")),
    menuItem("Poursuite d'études", tabName = "tab_poursuite_etudes", icon = icon("university")),
    convertMenuItem(
      "tab_vie_active_durable",
      menuItem(
        "Vie active durable", tabName = "tab_vie_active_durable", icon = icon("building"), startExpanded = TRUE,
        menuSubItem("Accès au premier emploi", tabName = "tab_emploi_premier", icon = icon("user-tie")),
        menuSubItem("Emploi à 30 mois", tabName = "tab_emploi_30mois_poste", icon = icon("user-tie")),
        menuSubItem("Employeur à 30 mois", tabName = "tab_emploi_30mois_employeur", icon = icon("building")),
        menuSubItem("Satisfaction à 30 mois", tabName = "tab_emploi_30mois_adequation", icon = icon("smile"))
      )
    ),
    menuItem("Taux de réponse", tabName = "tab_repondants", icon = icon("clipboard-check")),
    hr(),
    menuItem("Filtres sélectionnés", icon = icon("filter")),
    shiny.modules::selected_filters_ui("selected_filters_ui")
  )
  
  # Poursuite d'études après l'emploi
  if (golem::get_golem_options("diplome") %in% c("LP", "Master")) {
    
    list_menu_items <- c(
      list_menu_items[1:2],
      list_menu_items[4],
      list_menu_items[3],
      list_menu_items[5:8]
    )
    
  }
  
  ui <- tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here,
    dashboardPagePlus(
      title = paste0("Devenir des diplômés de ", golem::get_golem_options("diplome")),
      dashboardHeaderPlus(
        titleWidth = 500,
        title = div(
          style = "text-align: left; font-size: 16px;",
          tags$a(
            href = "http://www.univ-tlse3.fr",
            tags$img(
              src = "https://www.univ-tlse3.fr/uas/ups/LOGO/logo_ups_blanc_et_jaune_petit.png", 
              height = "35px", 
              width = "35px"
            )
          ),
          HTML("Université <strong>TOULOUSE <font color='#fbca00'>III</font>-PAUL SABATIER</strong>")
        ),
        left_menu = tagList(
          div(
            HTML("<strong>", paste0("Devenir des diplômés de ", golem::get_golem_options("diplome")), "</strong>")
          )
        ),
        enable_rightsidebar = TRUE,
        rightSidebarIcon = "filter"
      ),
      dashboardSidebar(
        sidebarMenu(
          .list = list_menu_items
        )
      ),
      rightsidebar = rightSidebar(
        .items = mod_filtre_control_bar_ui("filtre_control_bar_ui")
      ),
      dashboardBody(
        tabItems(
          tabItem(
            tabName = "tab_home",
            mod_accueil_ui("accueil_ui")
          ),
          tabItem(
            tabName = "tab_chiffres_cles",
            mod_chiffres_cles_ui("chiffres_cles_ui")
          ),
          tabItem(
            tabName = "tab_repondants",
            mod_repondants_ui("repondants_ui")
          ),
          tabItem(
            tabName = "tab_poursuite_etudes",
            mod_poursuite_etudes_ui("poursuite_etudes_ui")
          ),
          tabItem(
            "tab_vie_active_durable",
            mod_vie_active_durable_ui("vie_active_durable_ui")
          ),
          tabItem(
            "tab_emploi_premier",
            mod_emploi_premier_ui("emploi_premier_ui")
          ),
          tabItem(
            "tab_emploi_30mois_poste",
            mod_emploi_30mois_poste_ui("emploi_30mois_poste_ui")
          ),
          tabItem(
            "tab_emploi_30mois_employeur",
            mod_emploi_30mois_employeur_ui("emploi_30mois_employeur_ui")
          ),
          tabItem(
            "tab_emploi_30mois_adequation",
            mod_emploi_30mois_adequation_ui("emploi_30mois_adequation_ui")
          )
        )
      ),
      footer = dashboardFooter(
        left_text = "Université Toulouse III - Paul Sabatier",
        right_text = "Traitement : Observatoire de la vie étudiante"
      )
    )
  )
  
  ui
}

#' @import shiny
golem_add_external_resources <- function(){
  
  golem::add_resource_path(
    'www', app_sys('app/www')
  )
  
  extrafont::loadfonts(device = "win", quiet = TRUE)
  
  tags$head(
    golem::favicon(),
    golem::activate_js(),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css"),
    fresh::use_theme(
      fresh::create_theme(
        fresh::adminlte_global(
          content_bg = "#F5F5F5"
        ),
        fresh::adminlte_sidebar(
          width = "220px",
          dark_bg = "#353535",
          dark_hover_bg = "#585858"
        ),
        fresh::adminlte_color(
          light_blue = "#353535",
          olive = "#a9a8a8",
          yellow = "#fbca00",
          black = "#585858"
        )
      )
    )
  )
}
