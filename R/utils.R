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
