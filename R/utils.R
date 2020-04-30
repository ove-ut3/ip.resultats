convertMenuItem <- function(tabName, mi) {
  
  # https://stackoverflow.com/questions/48210709/show-content-for-menuitem-when-menusubitems-exist-in-shiny-dashboard
  
  mi$children[[1]]$attribs['data-toggle'] <- "tab"
  mi$children[[1]]$attribs['data-value'] <- tabName
  if (length(mi$attribs$class) > 0 && mi$attribs$class == "treeview") {
    mi$attribs$class <- NULL
  }
  
  # change to angle-down icon
  mi$children[[1]]$children[[3]]$attribs['class'] <- "fa fa-angle-down pull-right"
  
  # Set class to custom
  mi$children[[2]]$attribs['class'] <- "treeview-menu-custom"
  
  mi
  
}

tabBox_footer <- function(..., id = NULL, selected = NULL, title = NULL, footer = NULL,
                          width = 6, height = NULL, side = c("left", "right"))
{
  side <- match.arg(side)
  
  # The content is basically a tabsetPanel with some custom modifications
  content <- shiny::tabsetPanel(..., id = id, selected = selected)
  content$attribs$class <- "nav-tabs-custom"
  
  # Set height
  if (!is.null(height)) {
    content <- tagAppendAttributes(content,
                                   style = paste0("height: ", validateCssUnit(height))
    )
  }
  
  # Move tabs to right side if needed
  if (side == "right") {
    content$children[[1]] <- tagAppendAttributes(content$children[[1]],
                                                 class = "pull-right"
    )
  }
  
  # Add title
  if (!is.null(title)) {
    if (side == "left")
      titleClass <- "pull-right"
    else
      titleClass <- "pull-left"
    
    content$children[[1]] <- htmltools::tagAppendChild(content$children[[1]],
                                                       tags$li(class = paste("header", titleClass), title)
    )
  }
  
  # Add footer
  if (!is.null(footer)) {
    content <- htmltools::tagAppendChild(
      content,
      div(class = "box-footer", footer)
    )
  }
  
  div(class = paste0("col-sm-", width), content)
}

tagAssert <- function(tag, type = NULL, class = NULL, allowUI = TRUE) {
  if (!inherits(tag, "shiny.tag")) {
    print(tag)
    stop("Expected an object with class 'shiny.tag'.")
  }
  
  # Skip dynamic output elements
  if (allowUI &&
      (hasCssClass(tag, "shiny-html-output") ||
       hasCssClass(tag, "shinydashboard-menu-output") ||
       hasCssClass(tag, "ygdashboard-module-output"))) {
    return()
  }
  
  if (!is.null(type) && tag$name != type) {
    stop("Expected tag to be of type ", type)
  }
  
  if (!is.null(class)) {
    if (is.null(tag$attribs$class)) {
      stop("Expected tag to have class '", class, "'")
      
    } else {
      tagClasses <- strsplit(tag$attribs$class, " ")[[1]]
      if (!(class %in% tagClasses)) {
        stop("Expected tag to have class '", class, "'")
      }
    }
  }
}

dashboardHeaderPlus <- function(..., title = NULL, titleWidth = NULL, 
                                disable = FALSE, .list = NULL, left_menu = NULL,
                                enable_rightsidebar = FALSE,
                                rightSidebarIcon = "gears", fixed = FALSE) {
  # handle right menu items
  items <- c(list(...), .list)
  lapply(items, tagAssert, type = "li", class = "dropdown")
  
  # handle left menu items
  if (!is.null(left_menu)) {
    left_menu_items <- lapply(1:length(left_menu), FUN = function(i) {
      left_menu_item <- left_menu[[i]]
      name <- left_menu_item$name
      class <- left_menu_item$attribs$class
      
      # if the left menu item is not a li tag and does not have
      # the dropdown class, create a wrapper to make it work
      if (name != "li" || !is.null(class) || class != "dropdown") {
        dropdownTag <- shiny::tags$li(class = "dropdown")
        left_menu_item <- shiny::tagAppendChild(dropdownTag, left_menu_item)
        # add some custom css to make it nicer
        left_menu_item <- shiny::tagAppendAttributes(
          left_menu_item,
          style = "margin-top: 7.5px; margin-left: 5px; margin-right: 5px;"
        )
      } else {
        left_menu_item
      }
    })
    # when left_menu is null, left_menu_items are also NULL 
  } else {
    left_menu_items <- left_menu
  }
  
  titleWidth <- shiny::validateCssUnit(titleWidth)
  
  # Set up custom CSS for custom width.
  custom_css <- NULL
  if (!is.null(titleWidth)) {
    # This CSS is derived from the header-related instances of '230px' (the
    # default sidebar width) from inst/AdminLTE/AdminLTE.css. One change is that
    # instead making changes to the global settings, we've put them in a media
    # query (min-width: 768px), so that it won't override other media queries
    # (like max-width: 767px) that work for narrower screens.
    custom_css <- shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          gsub(
            "_WIDTH_", 
            titleWidth, 
            fixed = TRUE, 
            '@media (min-width: 768px) {
              .main-header > .navbar {
                margin-left: _WIDTH_;
              }
              .main-header .logo {
                width: _WIDTH_;
              }
             }
              '
          )
        )
      )
    )
  }
  
  shiny::tags$header(
    class = "main-header",
    custom_css,
    style = if (disable) "display: none;",
    # only hide on small screen devices when title is NULL
    shiny::tags$span(class = if (is.null(title)) "logo hidden-xs" else "logo", title),
    shiny::tags$nav(
      class = paste0("navbar navbar-", if (fixed) "fixed" else "static", "-top"), 
      role = "navigation",
      # Embed hidden icon so that we get the font-awesome dependency
      shiny::tags$span(shiny::icon("bars"), style = "display:none;"),
      # Sidebar toggle button
      shiny::tags$a(
        href = "#", 
        class = "sidebar-toggle", 
        `data-toggle` = "offcanvas",
        role = "button",
        shiny::tags$span(class = "sr-only", "Toggle navigation")
      ),
      # left menu
      shiny::tags$div(
        class = "navbar-custom-menu-ove",
        style = "float: left; margin-left: 10px;",
        shiny::tags$ul(
          class = "nav navbar-nav",
          left_menu_items
        )
      ),
      # right menu
      shiny::tags$div(
        class = "navbar-custom-menu",
        shiny::tags$ul(
          class = "nav navbar-nav",
          items,
          # right sidebar
          if (isTRUE(enable_rightsidebar)) {
            shiny::tags$li(
              shiny::tags$a(
                href = "#", 
                `data-toggle` = "control-sidebar", 
                shiny::icon(rightSidebarIcon)
              )
            )
          }
        )
      )
    )
  )
}