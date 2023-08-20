library(DiagrammeR)
library(glue)

plot = grViz("
  digraph graphname {

    rankdir = LR
    nodesep = .8
    ranksep = .5

    # Node definitions with labels
    node [shape = box, color = black, style = filled, fillcolor = white]
    self_rating [label = 'Self Rating']
    standards [label = 'Standards']
    own_behavior [label = 'Own Behavior']
    friends [label = 'Friends']
    exemplars [label = 'Exemplars']

    # Positioning nodes
    {rank = same; friends; exemplars}
    {rank = same; standards; own_behavior}

    # Path definitions
    edge [color = black, arrowhead = open, arrowsize = 1]
    standards -> self_rating
    own_behavior -> self_rating
    standards -> own_behavior
    friends -> standards
    friends -> own_behavior
    exemplars -> standards
    exemplars -> own_behavior
    # Curved double-sided arrow from friends to exemplars
    friends -> exemplars [dir='both', constraint=false, weight=10, spline='true']

  }
")

plot

svg_content <- DiagrammeR::to_igraph(plot)

# Use magick to save the PNG
magick::image_write(img, path = "s2/p2/figures/path_diagram.png")


### add nums
add_edge_labels <- function(base_plot, labels) {
  # Extract the edge definitions from the base plot
  edges <- grep("^\\s*[a-zA-Z0-9_]+\\s*->\\s*[a-zA-Z0-9_]+", base_plot, value = TRUE)
  
  print(length(edges))
  print(length(labels))

  # Check if there are enough labels
  if (length(labels) != length(edges)) {
    stop("The number of labels doesn't match the number of edges.")
  }
  
  # Add the labels to the edges
  labeled_edges <- paste0(edges, " [label = ", labels, "]")
  
  # Replace the original edges with the labeled edges in the base plot
  for (i in 1:length(edges)) {
    base_plot <- gsub(edges[i], labeled_edges[i], base_plot, fixed = TRUE)
  }
  
  return(base_plot)
}

# Test the function
plot_string <- "
  digraph graphname {
    rankdir = LR
    nodesep = .8
    ranksep = .5

    node [shape = box, color = black, style = filled, fillcolor = white]
    self_rating [label = 'Self Rating']
    standards [label = 'Standards']
    own_behavior [label = 'Own Behavior']
    friends [label = 'Friends']
    exemplars [label = 'Exemplars']

    {rank = same; friends; exemplars}
    {rank = same; standards; own_behavior}

    standards -> self_rating
    own_behavior -> self_rating
    standards -> own_behavior
    friends -> standards
    friends -> own_behavior
    exemplars -> standards
    exemplars -> own_behavior
    friends -> exemplars [dir='both', constraint=false, weight=10, spline='true']
  }
"

labels <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1)
labeled_plot <- add_edge_labels(plot_string, labels)

# Check the result
print(labeled_plot)
