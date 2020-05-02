#' @import shiny shinydashboard shinydashboardPlus
app_ui <- function() {
  
  list_menu_items <- list(
    menuItem("Accueil", tabName = "tab_home", icon = icon("home")),
    menuItem("Chiffres-cl\u00e9s", tabName = "tab_chiffres_cles", icon = icon("tachometer-alt")),
    menuItem("Poursuite d'\u00e9tudes", tabName = "tab_poursuite_etudes", icon = icon("university")),
    convertMenuItem(
      "tab_vie_active_durable",
      menuItem(
        "Vie active durable", tabName = "tab_vie_active_durable", icon = icon("building"), startExpanded = TRUE,
        menuSubItem("Acc\u00e8s au premier emploi", tabName = "tab_emploi_premier", icon = icon("user-tie")),
        menuSubItem("Emploi \u00e0 30 mois", tabName = "tab_emploi_30mois_poste", icon = icon("user-tie")),
        menuSubItem("Employeur \u00e0 30 mois", tabName = "tab_emploi_30mois_employeur", icon = icon("building")),
        menuSubItem("Satisfaction \u00e0 30 mois", tabName = "tab_emploi_30mois_adequation", icon = icon("smile"))
      )
    ),
    menuItem("Taux de r\u00e9ponse", tabName = "tab_repondants", icon = icon("clipboard-check")),
    hr(),
    menuItem("Filtres s\u00e9lectionn\u00e9s", icon = icon("filter")),
    shiny.modules::selected_filters_ui("selected_filters_ui")
  )
  
  # Poursuite d'\u00e9tudes apr\u00e8s l'emploi
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
      title = paste0("Devenir des dipl\u00f4m\u00e9s de ", golem::get_golem_options("diplome")),
      dashboardHeaderPlus(
        titleWidth = 810,
        title = div(
          style = "display: inline-block;",
          tags$a(
            href = "http://www.univ-tlse3.fr",
            tags$img(
              src = "https://www.univ-tlse3.fr/uas/ups/LOGO/logo_ups_blanc_et_jaune_petit.png", 
              height = "35px", 
              width = "35px"
            )
          ),
          HTML("UNIVERSIT\u00c9 <strong>TOULOUSE <font color='#fbca00'>III</font>-PAUL SABATIER</strong>"),
          HTML(rep("&nbsp;", 7)),
          div(
            style = "display: inline-block; font-size: 22px; color: #fbca00; font-family: \"Roboto Slab\";",
            p(
              HTML(paste0("<strong>Devenir des dipl\u00f4m\u00e9s de ", golem::get_golem_options("diplome"), "</strong>"))
            )
          )
        ),
        # left_menu = tagList(
        #   p(
        #     HTML(paste0("<strong>Devenir des dipl\u00f4m\u00e9s de ", golem::get_golem_options("diplome"), "</strong>"))
        #   )
        # ),
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
        left_text = "Universit\u00e9 Toulouse III - Paul Sabatier",
        right_text = "Traitement : Observatoire de la vie \u00e9tudiante"
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
  
  extrafont::loadfonts(quiet = TRUE)
  
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
