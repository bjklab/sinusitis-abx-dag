#########################################
#' load libraries and set seed
##########################################
library(tidyverse)
library(dagitty)
library(ggdag)

set.seed(16)


#########################################
#' draw DAG
##########################################

dagify(Rx ~ pt_sx,
       Rx ~ md_trigger,
       appropriate ~ Rx,
       appropriate ~ pt_sx,
       md_trigger ~ U,
       pt_sx ~ U,
       exposure = "pt_sx",
       outcome = "appropriate",
       coords = list(x = c(pt_sx = 0, appropriate = 2, U = -1, md_trigger = 0, Rx = 1),
                     y = c(pt_sx = 0, appropriate = 0, U = 0.5, md_trigger = 1, Rx = 1))) |> 
  identity() -> sinusitis_abx_dag
sinusitis_abx_dag

sinusitis_abx_dag |> 
  ggdag::tidy_dagitty() |> 
  ggplot(data = _, aes(x = x, y = y, xend = xend, yend = yend)) +
  ggdag::geom_dag_edges() +
  ggdag::geom_dag_node() +
  ggdag::geom_dag_text(nudge_y = -0.2, color = "black") +
  theme_dag() +
  theme(text = element_text(size = 1))


#' unadjusted:
dagitty::paths(sinusitis_abx_dag, from = "pt_sx", to = "appropriate", Z = NULL, directed = FALSE) |> 
  tibble::as_tibble() |> 
  mutate(open = case_when(open == TRUE ~ "Open",
                          open == FALSE ~ "Closed")) |> 
  gt::gt() |> 
  gt::cols_label(paths = gt::md("Paths primary -> secondary"), open = "Status") |> 
  gt::tab_style(style = list(gt::cell_text(style = "italic", color = "red")),
                locations = gt::cells_body(columns = c(open),
                                           rows = open == "Open")) |> 
  gt::opt_row_striping() |> 
  identity() -> sinusitis_abx_dag_open
sinusitis_abx_dag_open


#' conditioned on Rx:
dagitty::paths(sinusitis_abx_dag, from = "pt_sx", to = "appropriate", Z = "Rx", directed = FALSE) |> 
  tibble::as_tibble() |> 
  mutate(open = case_when(open == TRUE ~ "Open",
                          open == FALSE ~ "Closed")) |> 
  gt::gt() |> 
  gt::cols_label(paths = gt::md("Paths primary -> secondary"), open = "Status") |> 
  gt::tab_style(style = list(gt::cell_text(style = "italic", color = "red")),
                locations = gt::cells_body(columns = c(open),
                                           rows = open == "Open")) |> 
  gt::opt_row_striping() |> 
  identity() -> sinusitis_abx_dag_Z_Rx
sinusitis_abx_dag_Z_Rx
