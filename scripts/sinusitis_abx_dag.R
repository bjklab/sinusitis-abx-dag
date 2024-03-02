#########################################
#' load libraries and set seed
##########################################
library(tidyverse)
library(dagitty)
library(ggdag)
library(gt)

set.seed(16)


#########################################
#' draw DAG
##########################################

dagify(#Rx ~ pt_sx,
       Rx ~ md_trigger,
       md_trigger ~ pt_sx,
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
  theme(text = element_text(size = 1)) |> 
  identity() -> p_sinusitis_abx_dag
p_sinusitis_abx_dag

p_sinusitis_abx_dag |> 
  ggsave(plot = _, filename = "figs/p_sinusitis_abx_dag.png", height = 6, width = 8, units = "in", dpi = 600)

p_sinusitis_abx_dag |> 
  ggsave(plot = _, filename = "figs/p_sinusitis_abx_dag.pdf", height = 6, width = 8, units = "in")



#' ###################################
#' paths to appropriateness
#' ###################################

#' unadjusted:
dagitty::paths(sinusitis_abx_dag, from = "pt_sx", to = "appropriate", Z = NULL, directed = FALSE) |> 
  tibble::as_tibble() |> 
  mutate(open = case_when(open == TRUE ~ "Open",
                          open == FALSE ~ "Closed")) |> 
  gt::gt() |> 
  gt::cols_label(paths = gt::md("Paths symptoms -> appropriate"), open = "Status") |> 
  gt::tab_style(style = list(gt::cell_text(style = "italic", color = "red")),
                locations = gt::cells_body(columns = c(open),
                                           rows = open == "Open")) |> 
  gt::opt_row_striping() |> 
  identity() -> sinusitis_abx_dag_open
sinusitis_abx_dag_open

sinusitis_abx_dag_open |> 
  gtsave("tabs/sinusitis_abx_dag_open.html")


#' conditioned on Rx:
dagitty::paths(sinusitis_abx_dag, from = "pt_sx", to = "appropriate", Z = "Rx", directed = FALSE) |> 
  tibble::as_tibble() |> 
  mutate(open = case_when(open == TRUE ~ "Open",
                          open == FALSE ~ "Closed")) |> 
  gt::gt() |> 
  gt::cols_label(paths = gt::md("Paths symptoms -> appropriate (conditioned on Rx)"), open = "Status") |> 
  gt::tab_style(style = list(gt::cell_text(style = "italic", color = "red")),
                locations = gt::cells_body(columns = c(open),
                                           rows = open == "Open")) |> 
  gt::opt_row_striping() |> 
  identity() -> sinusitis_abx_dag_Z_Rx
sinusitis_abx_dag_Z_Rx

sinusitis_abx_dag_Z_Rx |> 
  gtsave("tabs/sinusitis_abx_dag_Z_Rx.html")


#' ###################################
#' paths to Rx
#' ###################################

#' unadjusted:
dagitty::paths(sinusitis_abx_dag, from = "pt_sx", to = "Rx", Z = NULL, directed = FALSE) |> 
  tibble::as_tibble() |> 
  mutate(open = case_when(open == TRUE ~ "Open",
                          open == FALSE ~ "Closed")) |> 
  gt::gt() |> 
  gt::cols_label(paths = gt::md("Paths symptoms -> Rx"), open = "Status") |> 
  gt::tab_style(style = list(gt::cell_text(style = "italic", color = "red")),
                locations = gt::cells_body(columns = c(open),
                                           rows = open == "Open")) |> 
  gt::opt_row_striping() |> 
  identity() -> sinusitis_abx_Rx_dag_open
sinusitis_abx_Rx_dag_open

sinusitis_abx_Rx_dag_open |> 
  gtsave("tabs/sinusitis_abx_Rx_dag_open.html")



#' conditioned on appropriate:
dagitty::paths(sinusitis_abx_dag, from = "pt_sx", to = "Rx", Z = "appropriate", directed = FALSE) |> 
  tibble::as_tibble() |> 
  mutate(open = case_when(open == TRUE ~ "Open",
                          open == FALSE ~ "Closed")) |> 
  gt::gt() |> 
  gt::cols_label(paths = gt::md("Paths symptoms -> Rx (conditioned on appropriateness)"), open = "Status") |> 
  gt::tab_style(style = list(gt::cell_text(style = "italic", color = "red")),
                locations = gt::cells_body(columns = c(open),
                                           rows = open == "Open")) |> 
  gt::opt_row_striping() |> 
  identity() -> sinusitis_abx_Rx_dag_Z_appropriate
sinusitis_abx_Rx_dag_Z_appropriate

sinusitis_abx_Rx_dag_Z_appropriate |> 
  gtsave("tabs/sinusitis_abx_Rx_dag_Z_appropriate.html")


