library(ggalluvial)
library(shiny)
library(htmltools)
library(sp)

data(vaccinations)
vaccinations <- transform(vaccinations,
                          response = factor(response, rev(levels(response))))

# Offset, in pixels, for location of tooltip relative to mouse cursor,
# in both x and y direction.
offset <- 5
# Width of node boxes
node_width <- 1/3
# Width of alluvia
alluvium_width <- 1/3

# Draw plot and extract coordinates
p <- ggplot(vaccinations,
            aes(x = survey, stratum = response, alluvium = subject,
                y = freq,
                fill = response, label = response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_flow(knot.pos = 1/4, width = alluvium_width) +
  geom_stratum(width = node_width) +
  geom_text(stat = "stratum", size = rel(2)) +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("Vaccination responses on three surveys") +
  theme(plot.title = element_text(size = rel(1)),
        legend.position = 'bottom')

# Build the plot.
pbuilt <- ggplot_build(p)

# Use built plot data to recalculate the locations of the flow polygons:

# Add width parameter, and then convert built plot data to xsplines
data_draw <- transform(pbuilt$data[[1]], width = alluvium_width)
groups_to_draw <- split(data_draw, data_draw$group) 
group_xsplines <- lapply(groups_to_draw,
                         data_to_alluvium) 

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
xrange_new <- c(1 - alluvium_width/2, max(pbuilt$data[[1]]$x) + alluvium_width/2) 
yrange_new <- c(0, sum(pbuilt$data[[2]]$count[pbuilt$data[[2]]$x == 1])) 

# Define function to convert grid graphics coordinates to data coordinates
new_range_transform <- function(x_old, range_old, range_new) {
  (x_old - range_old[1])/(range_old[2] - range_old[1]) *
    (range_new[2] - range_new[1]) + range_new[1]
}

# Using the x and y limits, convert the grid coordinates into plot coordinates. 
polygon_coords <- lapply(xspline_points, function(pts) {
  x_trans <- new_range_transform(x_old = as.numeric(pts$x), 
                                 range_old = xrange_old, 
                                 range_new = xrange_new)
  y_trans <- new_range_transform(x_old = as.numeric(pts$y), 
                                 range_old = yrange_old, 
                                 range_new = yrange_new)
  list(x = x_trans, y = y_trans)
})

# User interface
ui <- fluidPage(
  fluidRow(tags$div(
    style = "position: relative;",
    plotOutput("alluvial_plot", height = "650px", 
               hover = hoverOpts(id = "plot_hover")
    ),
    htmlOutput("tooltip")))
)

server <- function(input, output, session) {
  
  # Draw plot 
  output$alluvial_plot <- renderPlot(p, res = 200)
  
  # Display tooltip
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
        node_n <- pbuilt$data[[2]]$count[node_row] 
        
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
          # Find the strata labels and n corresponding to that alluvium in the data.
          flow_label <- paste(groups_to_draw[[coord_id]]$stratum, collapse = ' -> ')
          flow_n <- groups_to_draw[[coord_id]]$count[1]

          # Render tooltip
          renderTags(
            tags$div(
              flow_label, tags$br(),
              "n =", flow_n,
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
