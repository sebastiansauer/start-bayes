library(DiagrammeR)

plot_zv <-
grViz("
digraph random_variable {
  graph [layout=dot, rankdir=LR];

  node [shape=ellipse, style=filled, fillcolor=lightblue, fontname=Helvetica, fontsize=12];

  # Sample space (left)
  NN [label='NN', pos='0,2!'];
  NT [label='NT', pos='0,1!'];
  TN [label='TN', pos='0,0!'];
  TT [label='TT', pos='0,-1!'];

  # Random variable values (right)
  N0 [label='0', fillcolor=lightyellow, pos='3,0!'];
  N1 [label='1', fillcolor=lightyellow, pos='3,1!'];
  N2 [label='2', fillcolor=lightyellow, pos='3,2!'];

  # Invisible clusters for ovals
  subgraph cluster_omega {
    style=rounded; color=black; label='Ereignisraum Ω'; fontsize=14;
    NN; NT; TN; TT;
  }

  subgraph cluster_values {
    style=rounded; color=black; label='Werte von X'; fontsize=14;
    N0; N1; N2;
  }

  # Arrows mapping outcomes to values
  NN -> N2;
  NT -> N1;
  TN -> N1;
  TT -> N0;
}
")


dpi <- 300
width_inch <- 6
height_inch <- 4

# Convert inches to pixels
width_px <- width_inch * dpi
height_px <- height_inch * dpi

svg_code <- export_svg(plot_zv)

# Render SVG to PNG
rsvg_png(charToRaw(svg_code), "images/zv_plot.png",
         width = width_px, height = height_px)
