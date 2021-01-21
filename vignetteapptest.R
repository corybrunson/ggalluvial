library(ggalluvial)
library(shiny)
library(htmltools)
library(sp)

example_data <- data.frame(
  weight = c(3, rep(1, 5), rep(2, 5), 3),
  ID = 1:12,
  cluster = rep(c(1, 2), c(4, 8)),
  grp1 = rep(c('1a', '1b', '1a', '1b'), c(3, 2, 3, 4)),
  grp2 = rep(c('2a', '2b', '2a', '2b', '2a'), c(2, 2, 2, 2, 4)),
  grp3 = rep(c('3a','3b', '3a', '3b'), c(3, 2, 2, 5))
)

# User interface
ui <- fluidPage(
  fluidRow(tags$div(
    style = "position: relative;",
    plotOutput("alluvial_plot", height = "500px", 
               hover = hoverOpts(id = "plot_hover")
    ),
    htmlOutput("tooltip")))
)

server <- function(input, output, session) {
  
  # Draw plot and extract coordinates
  output$alluvial_plot <- renderPlot({
    
    # Width of node boxes
    node_width <<- 1/4
    
    p <- ggplot(example_data,
                aes(y = weight, axis1 = grp1, axis2 = grp2, axis3 = grp3)) + 
      geom_alluvium(aes(fill = factor(cluster)), knot.pos = 0.25) + 
      geom_stratum(width = node_width, reverse = TRUE) + 
      geom_text(aes(label = after_stat(stratum)), 
                stat = "stratum", 
                reverse = TRUE, 
                size = rel(3)) + 
      theme_bw() +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0))
    
    # Build the plot. Use global assignment so that this object is accessible
    # later.
    pbuilt <<- ggplot_build(p)
    
    # Use built plot data to recalculate the locations of the flow polygons:
    
    # Add width parameter, and then convert built plot data to xsplines
    data_draw <- transform(pbuilt$data[[1]], width = 1/3)
    groups_to_draw <- split(data_draw, data_draw$group)
    group_xsplines <- lapply(groups_to_draw,
                             ggalluvial:::data_to_xspline,
                             knot.prop = TRUE) 
    
    # Convert xspline coordinates to grid object.
    xspline_coords <- lapply(
      group_xsplines,
      function(coords) grid::xsplineGrob(x = coords$x, 
                                         y = coords$y, 
                                         shape = coords$shape, 
                                         open = FALSE)
    )
    
    # Use grid::xsplinePoints to draw the curve for each polygon
    xspline_points <- lapply(xspline_coords, grid::xsplinePoints)
    
    # Define the x and y axis limits in grid coordinates (old) and plot
    # coordinates (new)
    xrange_old <- range(unlist(lapply(
      xspline_points,
      function(pts) as.numeric(pts$x)
    )))
    yrange_old <- range(unlist(lapply(
      xspline_points,
      function(pts) as.numeric(pts$y)
    )))
    xrange_new <- c(1 - 1/6, 3 + 1/6) 
    yrange_new <- c(0, sum(example_data$weight)) 
    
    # Define function to convert grid graphics coordinates to data coordinates
    new_range_transform <- function(x_old, range_old, range_new) {
      (x_old - range_old[1])/(range_old[2] - range_old[1]) *
        (range_new[2] - range_new[1]) + range_new[1]
    }
    
    # Using the x and y limits, convert the grid coordinates into plot
    # coordinates. Use global assignment.
    polygon_coords <<- lapply(xspline_points, function(pts) {
      x_trans <- new_range_transform(x_old = as.numeric(pts$x), 
                                     range_old = xrange_old, 
                                     range_new = xrange_new)
      y_trans <- new_range_transform(x_old = as.numeric(pts$y), 
                                     range_old = yrange_old, 
                                     range_new = yrange_new)
      list(x = x_trans, y = y_trans)
    })
    
    # Return plot
    p
  }, 
  res = 200)
  
  output$tooltip <- renderText(
    if(!is.null(input$plot_hover)) {
      hover <- input$plot_hover
      x_coord <- round(hover$x)
      
      if(abs(hover$x - x_coord) < (node_width / 2)) {
        # Display node information if cursor is over a stratum box.
        
        # Determine stratum name from x and y coord, and the n.
        node_row <- pbuilt$data[[2]]$x == x_coord & 
          hover$y > pbuilt$data[[2]]$ymin & 
          hover$y < pbuilt$data[[2]]$ymax
        node_label <- pbuilt$data[[2]]$stratum[node_row]
        node_n <- pbuilt$data[[2]]$n[node_row]
        
        # Offset, in pixels, for location of tooltip relative to mouse cursor,
        # in both x and y direction.
        offset <- 5
        
        # Render tooltip
        renderTags(
          tags$div(
            node_label, tags$br(),
            "n =", node_n,
            style = paste0(
              "position: absolute; ",
              "top: ", hover$coords_css$y + offset, "px; ",
              "left: ", hover$coords_css$x + offset, "px; ",
              "background: gray; ",
              "padding: 3px; ",
              "color: white; "
            )
          )
        )$html
      } else {
        # Display flow information if cursor is over a flow polygon: what
        # alluvia does it pass through?
        
        # Calculate whether coordinates of hovering cursor are inside one of the
        # polygons.
        hover_within_flow <- sapply(
          polygon_coords,
          function(pol) point.in.polygon(point.x = hover$x, 
                                         point.y = hover$y, 
                                         pol.x = pol$x, 
                                         pol.y = pol$y)
        )
        if (any(hover_within_flow)) {
          # Find the alluvium that is plotted on top. (last)
          coord_id <- rev(which(hover_within_flow == 1))[1]
          # Get the corresponding row ID from the data.
          flow_id <- example_data$ID[coord_id]
          # Get the axis 1-3 values for all axes for that row ID.
          axis_values <- example_data[flow_id, c('grp1', 'grp2', 'grp3')]
          
          offset <- 5
          
          # Render tooltip
          renderTags(
            tags$div(
              paste(axis_values, collapse = ' -> '),
              style = paste0(
                "position: absolute; ",
                "top: ", hover$coords_css$y + offset, "px; ",
                "left: ", hover$coords_css$x + offset, "px; ",
                "background: gray; ",
                "padding: 3px; ",
                "color: white; "
              )
            )
          )$html
        }
      }
    }
  )
}

shinyApp(ui = ui, server = server)
