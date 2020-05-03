convertMenuItem <- function(tabName, mi, request) {
  
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
  mi$children[[2]]$attribs['style'] <- "display: block;"
  
  if (grepl("state=", request$QUERY_STRING)) {
    
    state_id <- request$QUERY_STRING %>%
      stringr::str_match("=(.+)$") %>%
      .[, 2]

    bookmark_menu_selected <- list.files("/home/shiny/shiny", pattern = paste0(state_id, "/input.rds"), full.names = TRUE, recursive = TRUE) %>% 
    readRDS(rds_file) %>%
      .[["menus"]]

    num_menu <- which(purrr::map_lgl(mi$children[[2]]$children, ~ stringr::str_detect(paste(., collapse = ""), bookmark_menu_selected)))

    if (length(num_menu) == 1) {
      mi$children[[2]]$children[[num_menu]]$children[[1]]$attribs['data-start-selected'] <- "1"
    }
    
  }
  
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

`%inT%` <- function(x, table) {
  if (!is.null(table) && ! "" %in% table) {
    x %in% table
  } else {
    rep_len(TRUE, length(x))
  }
}

toggleDisplayUi <- function() {
  htmltools::tags$script(
    paste(
      "Shiny.addCustomMessageHandler('toggleDisplay',",
      "function(data) {",
      "$('#' + data.id).css('display', data.display);",
      "});",
      sep = "\n"
    )
  )
}

toggleDisplayServer <- function(session, id, display = c("none", "block", "inline-block", "table-cell")) {
  display <- match.arg(display)
  session$sendCustomMessage(
    type = 'toggleDisplay',
    message = list(id = id, display = display)
  )
}

selectizeGroupUI <- function(id, params, label = NULL, btn_label = "Reset filters", inline = TRUE) {
  
  # Namespace
  ns <- NS(id)
  
  if (inline) {
    selectizeGroupTag <- tagList(
      tags$b(label),
      tags$div(
        class="btn-group-justified selectize-group",
        role="group", `data-toggle`="buttons",
        lapply(
          X = seq_along(params),
          FUN = function(x) {
            input <- params[[x]]
            tagSelect <- tags$div(
              class = "btn-group",
              id = ns(paste0("container-", input$inputId)),
              selectInput(
                inputId = ns(input$inputId),
                label = input$title,
                choices = input$choices,
                selected = input$selected,
                multiple = TRUE,
                width = "100%"#,
                # options = list(
                #   placeholder = input$placeholder, plugins = list("remove_button"),
                #   onInitialize = I('function() { this.setValue(""); }')
                # )
              )
            )
            return(tagSelect)
          }
        )
      ),
      actionLink(
        inputId = ns("reset_all"),
        label = btn_label,
        icon = icon("remove"),
        style = "float: right;"
      )
    )
  } else {
    selectizeGroupTag <- tagList(
      tags$b(label),
      lapply(
        X = seq_along(params),
        FUN = function(x) {
          input <- params[[x]]
          tagSelect <- selectInput(
            inputId = ns(input$inputId),
            label = input$title,
            choices = input$choices,
            selected = input$selected,
            multiple = TRUE,
            width = "100%"#,
            # options = list(
            #   placeholder = input$placeholder, plugins = list("remove_button"),
            #   onInitialize = I('function() { this.setValue(""); }')
            # )
          )
          return(tagSelect)
        }
      ),
      actionLink(
        inputId = ns("reset_all"),
        label = btn_label,
        icon = icon("remove"),
        style = "float: right;"
      )
    )
  }
  
  tagList(
    singleton(
      tagList(
        # tags$link(
        #   rel="stylesheet",
        #   type="text/css",
        #   href="shinyWidgets/modules/styles-modules.css"
        # ),
        toggleDisplayUi()
      )
    ),
    selectizeGroupTag
  )
  
}

selectizeGroupServer <- function(input, output, session, data, vars) { # nocov start
  
  # Namespace
  ns <- session$ns
  toggleDisplayServer(
    session = session, id = ns("reset_all"), display = "none"
  )
  
  
  # data <- as.data.frame(data)
  rv <- reactiveValues(data = NULL, vars = NULL)
  observe({
    if (is.reactive(data)) {
      rv$data <- data()
    } else {
      rv$data <- as.data.frame(data)
    }
    if (is.reactive(vars)) {
      rv$vars <- vars()
    } else {
      rv$vars <- vars
    }
    for (var in names(rv$data)) {
      if (var %in% rv$vars) {
        toggleDisplayServer(
          session = session, id = ns(paste0("container-", var)), display = "table-cell"
        )
      } else {
        toggleDisplayServer(
          session = session, id = ns(paste0("container-", var)), display = "none"
        )
      }
    }
  })
  
  observe({
    lapply(
      X = rv$vars,
      FUN = function(x) {
        vals <- sort(unique(rv$data[[x]]))
        updateSelectInput(
          session = session,
          inputId = x,
          choices = vals#,
          #server = TRUE
        )
      }
    )
  })
  
  observeEvent(input$reset_all, {
    lapply(
      X = rv$vars,
      FUN = function(x) {
        vals <- sort(unique(rv$data[[x]]))
        updateSelectInput(
          session = session,
          inputId = x,
          choices = vals#,
          #server = TRUE
        )
      }
    )
  })
  
  
  observe({
    vars <- rv$vars
    lapply(
      X = vars,
      FUN = function(x) {
        
        ovars <- vars[vars != x]
        
        observeEvent(input[[x]], {
          
          data <- rv$data
          
          indicator <- lapply(
            X = vars,
            FUN = function(x) {
              data[[x]] %inT% input[[x]]
            }
          )
          indicator <- Reduce(f = `&`, x = indicator)
          data <- data[indicator, ]
          
          if (all(indicator)) {
            toggleDisplayServer(session = session, id = ns("reset_all"), display = "none")
          } else {
            toggleDisplayServer(session = session, id = ns("reset_all"), display = "block")
          }
          
          for (i in ovars) {
            if (is.null(input[[i]])) {
              updateSelectInput(
                session = session,
                inputId = i,
                choices = sort(unique(data[[i]]))#,
                #server = TRUE
              )
            }
          }
          
          if (is.null(input[[x]])) {
            updateSelectInput(
              session = session,
              inputId = x,
              choices = sort(unique(data[[x]]))#,
              #server = TRUE
            )
          }
          
        }, ignoreNULL = FALSE, ignoreInit = TRUE)
        
      }
    )
  })
  
  return(reactive({
    data <- rv$data
    vars <- rv$vars
    indicator <- lapply(
      X = vars,
      FUN = function(x) {
        data[[x]] %inT% input[[x]]
      }
    )
    indicator <- Reduce(f = `&`, x = indicator)
    data <- data[indicator, ]
    return(data)
  }))
}
