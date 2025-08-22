## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(flowchart)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)

## ----eval=FALSE---------------------------------------------------------------
# install.packages("flowchart")

## ----eval=FALSE---------------------------------------------------------------
# # install.packages("remotes")
# remotes::install_github('bruigtp/flowchart')

## -----------------------------------------------------------------------------
library(flowchart)

data(safo)

head(safo) 

## -----------------------------------------------------------------------------
safo_fc <- safo |> 
  as_fc()

str(safo_fc, max.level = 1)

## -----------------------------------------------------------------------------
safo_fc$fc

## ----include=FALSE------------------------------------------------------------
as_fc(N = 230)

## -----------------------------------------------------------------------------
safo_fc |> 
  fc_draw()

## ----fig.width = 6, fig.height = 5--------------------------------------------
safo |> 
  as_fc(label = "Patients assessed for eligibility") |> 
  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |> 
  fc_draw()

## ----fig.width = 6, fig.height = 5--------------------------------------------
safo |> 
  as_fc(label = "Patients assessed for eligibility") |> 
  fc_filter(N = 215, label = "Randomized", show_exc = TRUE) |> 
  fc_draw()

## ----fig.width = 6, fig.height = 5--------------------------------------------
safo |>
  dplyr::filter(!is.na(group)) |>
  as_fc(label = "Randomized patients") |>
  fc_split(group) |>
  fc_draw()

## ----fig.width = 6, fig.height = 5--------------------------------------------
safo |>
  dplyr::filter(!is.na(group)) |>
  as_fc(label = "Randomized patients") |>
  fc_split(N = c(105, 110), label = c("cloxacillin plus fosfomycin", "cloxacillin alone")) |> 
  fc_draw()

## ----eval = FALSE-------------------------------------------------------------
# safo |>
#   as_fc(label = "Patients assessed for eligibility") |>
#   fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |>
#   fc_draw() |>
#   fc_export("flowchart.png")

## ----eval = FALSE-------------------------------------------------------------
# safo |>
#   as_fc(label = "Patients assessed for eligibility") |>
#   fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |>
#   fc_draw() |>
#   fc_export("flowchart.png", width = 3000, height = 4000, res = 700)

