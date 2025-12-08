library(DiagrammeR)

plot_muenzwurf_4fach <-
grViz("
digraph coin_tree {
  node [shape=box, style=rounded, fontname=Helvetica]
  rankdir = TB;

  # leaf colors by number of Heads (N)
  # 0 N = lightpink
  # 1 N = lightsalmon
  # 2 N = lightgoldenrod
  # 3 N = palegreen
  # 4 N = lightblue

  A [label='Start'];

  A -> B [label='N (1/2)'];
  A -> C [label='T (1/2)'];

  B -> D [label='N (1/2)']; 
  B -> E [label='T (1/2)'];
  C -> F [label='N (1/2)']; 
  C -> G [label='T (1/2)'];

  D -> H [label='N (1/2)']; 
  D -> I [label='T (1/2)'];
  E -> J [label='N (1/2)']; 
  E -> K [label='T (1/2)'];
  F -> L [label='N (1/2)']; 
  F -> M [label='T (1/2)'];
  G -> N [label='N (1/2)']; 
  G -> O [label='T (1/2)'];

  # level 4 leaves (16 total)
  H -> H1 [label='N (1/2)']; H1 [label='NNNN\\n(1/16)', style=filled, fillcolor=lightblue];
  H -> H2 [label='T (1/2)']; H2 [label='NNNT\\n(1/16)', style=filled, fillcolor=palegreen];

  I -> I1 [label='N (1/2)']; I1 [label='NNTN\\n(1/16)', style=filled, fillcolor=palegreen];
  I -> I2 [label='T (1/2)']; I2 [label='NNTT\\n(1/16)', style=filled, fillcolor=lightgoldenrod];

  J -> J1 [label='N (1/2)']; J1 [label='NTNN\\n(1/16)', style=filled, fillcolor=palegreen];
  J -> J2 [label='T (1/2)']; J2 [label='NTNT\\n(1/16)', style=filled, fillcolor=lightgoldenrod];

  K -> K1 [label='N (1/2)']; K1 [label='NTTN\\n(1/16)', style=filled, fillcolor=lightgoldenrod];
  K -> K2 [label='T (1/2)']; K2 [label='NTTT\\n(1/16)', style=filled, fillcolor=lightsalmon];

  L -> L1 [label='N (1/2)']; L1 [label='TNNN\\n(1/16)', style=filled, fillcolor=palegreen];
  L -> L2 [label='T (1/2)']; L2 [label='TNNT\\n(1/16)', style=filled, fillcolor=lightgoldenrod];

  M -> M1 [label='N (1/2)']; M1 [label='TNTN\\n(1/16)', style=filled, fillcolor=lightgoldenrod];
  M -> M2 [label='T (1/2)']; M2 [label='TNTT\\n(1/16)', style=filled, fillcolor=lightsalmon];

  N -> N1 [label='N (1/2)']; N1 [label='TTNN\\n(1/16)', style=filled, fillcolor=lightgoldenrod];
  N -> N2 [label='T (1/2)']; N2 [label='TTNT\\n(1/16)', style=filled, fillcolor=lightsalmon];

  O -> O1 [label='N (1/2)']; O1 [label='TTTN\\n(1/16)', style=filled, fillcolor=lightsalmon];
  O -> O2 [label='T (1/2)']; O2 [label='TTTT\\n(1/16)', style=filled, fillcolor=lightpink];
}
")
