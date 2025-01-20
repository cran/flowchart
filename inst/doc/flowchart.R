## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(flowchart)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("flowchart")

## ----eval=FALSE---------------------------------------------------------------
#  # install.packages("remotes")
#  remotes::install_github('bruigtp/flowchart')

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

## ----fig.width = 6, fig.height = 4--------------------------------------------
safo_fc <- safo |> 
  as_fc(label = "Patients assessed for eligibility") |>
  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) 

safo_fc |> 
  fc_draw()

## -----------------------------------------------------------------------------
safo_fc |> 
  fc_view("fc")

## ----fig.width = 7, fig.height = 5--------------------------------------------
safo_fc |> 
  fc_modify(
    ~ . |> 
      mutate(
        text = ifelse(id == 3, str_glue("- {sum(safo$inclusion_crit == 'Yes')} not met the inclusion criteria\n- {sum(safo$exclusion_crit == 'Yes')} met the exclusion criteria"), text),
        x = case_when(
          id == 3 ~ 0.75,
          TRUE ~ x
        ),
        y = case_when(
          id == 1 ~ 0.8,
          id == 2 ~ 0.2,
          TRUE ~ y
        )
      )
  ) |> 
  fc_draw()

## ----fig.width = 8------------------------------------------------------------
# Create first flowchart for ITT
fc1 <- safo |> 
  as_fc(label = "Patients assessed for eligibility") |>
  fc_filter(itt == "Yes", label = "Intention to treat (ITT)")

fc_draw(fc1)

# Create second flowchart for visits
fc2 <- safo |> 
  as_fc(label = "Patients assessed for eligibility") |>
  fc_filter(pp == "Yes", label = "Per protocol (PP)")

fc_draw(fc2)

list(fc1, fc2) |> 
  fc_merge() |> 
  fc_draw()

## ----warning = FALSE, fig.width = 6, fig.height = 5---------------------------
list(fc1, fc2) |> 
  fc_stack() |> 
  fc_draw()

## ----warning=FALSE, fig.width = 6, fig.height = 5-----------------------------
fc1 <- fc1 |> 
  fc_split(group)

list(fc1, fc2) |> 
  fc_stack(unite = TRUE) |> 
  fc_draw()

## ----eval = FALSE-------------------------------------------------------------
#  safo |>
#    as_fc(label = "Patients assessed for eligibility") |>
#    fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |>
#    fc_draw() |>
#    fc_export("flowchart.png")

## ----eval = FALSE-------------------------------------------------------------
#  safo |>
#    as_fc(label = "Patients assessed for eligibility") |>
#    fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |>
#    fc_draw() |>
#    fc_export("flowchart.png", width = 3000, height = 4000, res = 700)

## ----warning = FALSE, fig.width = 7, fig.height = 7---------------------------
safo |> 
  as_fc(label = "Patients assessed for eligibility") |>
  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |> 
  fc_split(group) |> 
  fc_filter(itt == "Yes", label = "Included in ITT") |> 
  fc_filter(pp == "Yes", label = "Included in PP") |> 
  fc_draw()

## ----warning=FALSE, fig.width = 12, fig.height = 8----------------------------
# Create labels for exclusion box:
label_exc <- paste(
  c(str_glue("{sum(safo$inclusion_crit == 'Yes' | safo$exclusion_crit == 'Yes' | safo$decline_part == 'Yes', na.rm = T)} excluded:"),
    map_chr(c("inclusion_crit", "decline_part", "exclusion_crit"), ~str_glue("{sum(safo[[.x]] == 'Yes', na.rm = TRUE)} {attr(safo[[.x]], 'label')}")),
    map_chr(4:15, ~str_glue(" -  {sum(safo[[.x]] == 'Yes')} {attr(safo[[.x]], 'label')}"))),
  collapse = "\n")

label_exc <- gsub("exclusion criteria", "exclusion criteria:", label_exc)

safo1 <- safo |> 
  filter(group == "cloxacillin alone", !is.na(reason_pp)) |> 
  mutate(reason_pp = droplevels(reason_pp))

label_exc1 <- paste(
  c(str_glue("{nrow(safo1)} excluded:"),
    map_chr(levels(safo1$reason_pp), ~str_glue(" -  {sum(safo1$reason_pp == .x)} {.x}"))),
  collapse = "\n")

label_exc1 <- str_replace_all(label_exc1, c("resistant" = "resistant\n", "blood" = "blood\n"))

safo2 <- safo |> 
  filter(group == "cloxacillin plus fosfomycin", !is.na(reason_pp)) |> 
  mutate(reason_pp = droplevels(reason_pp))

label_exc2 <- paste(
  c(str_glue("{nrow(safo2)} excluded:"),
    map_chr(levels(safo2$reason_pp), ~str_glue(" -  {sum(safo2$reason_pp == .x)} {.x}"))),
  collapse = "\n")

label_exc2 <- str_replace_all(label_exc2, c("nosocomial" = "nosocomial\n", "treatment" = "treatment\n"))

## ----warning=FALSE, fig.width = 13, fig.height = 9----------------------------
safo |> 
  as_fc(label = "patients assessed for eligibility", text_pattern = "{N} {label}") |> 
  fc_filter(!is.na(group), label = "randomized", text_pattern = "{n} {label}", show_exc = TRUE,
            just_exc = "left", text_pattern_exc = "{label}", label_exc = label_exc, text_fs_exc = 7) |>
  fc_split(group, text_pattern = "{n} asssigned\n {label}") |> 
  fc_filter(itt == "Yes", label = "included in intention-to-treat\n population", show_exc = TRUE, 
            text_pattern = "{n} {label}", 
            label_exc = "patient did not receive allocated\n treatment (withdrew consent)", 
            text_pattern_exc = "{n} {label}", text_fs_exc = 7) |>
  fc_filter(pp == "Yes", label = "included in per-protocol\n population", show_exc = TRUE,
            just_exc = "left", text_pattern = "{n} {label}", text_fs_exc = 7) |> 
  fc_modify(
    ~.x |> 
      filter(n != 0) |> 
      mutate(
        text = case_when(id == 11 ~ label_exc1, id == 13 ~ label_exc2, TRUE ~ text),
        x = case_when(id == 3 ~ x + 0.15, id %in% c(11, 13) ~ x + 0.01, TRUE ~ x),
        y = case_when(id %in% c(1, 3) ~ y + 0.05, id >= 2 ~ y - 0.05, TRUE ~ y)
      )
  ) |> 
  fc_draw()

## ----warning=FALSE, fig.width = 7, fig.height = 6-----------------------------
as_fc(N = 300) |> 
  fc_filter(N = 240, label = "Randomized patients", show_exc = TRUE) |> 
  fc_split(N = c(100, 80, 60), label = c("Group A", "Group B", "Group C")) |>
  fc_filter(N = c(80, 75, 50), label = "Finished the study") |> 
  fc_draw()

## ----warning=FALSE, fig.width = 7, fig.height = 6-----------------------------

safo |> 
  as_fc(label = "Patients assessed for eligibility") |>
  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |> 
  fc_split(group) |> 
  fc_split(N = c(50, 55, 10, 100), label = c("Group A", "Group B")) |> 
  fc_draw()


## ----fig.width = 6, fig.height = 5--------------------------------------------
safo |> 
  as_fc(label = "Patients assessed for eligibility") |>
  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |> 
  fc_split(group) |> 
  fc_draw()

## ----fig.width = 6, fig.height = 5--------------------------------------------
safo |> 
  as_fc(label = "Patients assessed for eligibility") |>
  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |> 
  fc_split(group) |> 
  fc_draw(title = "SAFO flowchart")

## ----fig.width = 6, fig.height = 5--------------------------------------------
safo |> 
  as_fc(label = "Patients assessed for eligibility") |>
  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE, perc_total = TRUE) |> 
  fc_split(group, perc_total = TRUE, title = "Treatment", bg_fill_title = "skyblue") |> 
  fc_draw()

## ----fig.width = 6, fig.height = 5--------------------------------------------
safo |> 
  as_fc(label = "Patients assessed for eligibility") |>
  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE, perc_total = TRUE) |> 
  fc_split(group, perc_total = TRUE) |> 
  fc_draw()

## ----fig.width = 6, fig.height = 5--------------------------------------------
safo |> 
  as_fc(label = "Patients assessed for eligibility") |>
  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE, perc_total = TRUE) |> 
  fc_split(group, offset = 0.1) |> 
  fc_draw()

safo |> 
  as_fc(label = "Patients assessed for eligibility") |>
  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE, perc_total = TRUE) |> 
  fc_split(group, offset = -0.1) |> 
  fc_draw()

## ----fig.width = 6, fig.height = 5--------------------------------------------
safo |> 
  as_fc(label = "Patients assessed for eligibility") |>
  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE, offset_exc = 0.1) |> 
  fc_split(group) |> 
  fc_draw()

## ----fig.width = 6, fig.height = 5--------------------------------------------
safo |> 
  as_fc(label = "Patients assessed for eligibility") |>
  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |> 
  fc_split(group) |> 
  fc_draw(box_corners = "sharp")

## -----------------------------------------------------------------------------
safo |> 
  as_fc(label = expression(paste("Patients ", italic("assessed"), " for ", bold("eligibility")))) |>
  fc_draw()

## -----------------------------------------------------------------------------
safo |> 
  as_fc(label = expression(paste(y, " = ", alpha, " + ", beta, x))) |>
  fc_draw()

## ----fig.width = 6, fig.height = 6--------------------------------------------
safo |> 
  as_fc(label = "Patients assessed for eligibility") |>
  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |> 
  fc_split(group) |> 
  fc_split(N = c(50, 60), sel_group = "cloxacillin alone") |> 
  fc_draw()

## ----fig.width = 6, fig.height = 6--------------------------------------------
safo |> 
  as_fc(label = "Patients assessed for eligibility") |>
  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |> 
  fc_split(group) |> 
  fc_split(N = c(50, 60), sel_group = "cloxacillin alone") |> 
  fc_filter(N = 50, sel_group = "cloxacillin plus fosfomycin") |> 
  fc_draw()

## ----fig.width = 6, fig.height = 7--------------------------------------------
safo |> 
  as_fc(label = "Patients assessed for eligibility") |>
  fc_filter(!is.na(group), label = "Randomized", show_exc = TRUE) |> 
  fc_split(group) |> 
  fc_split(N = c(50, 55, 10, 100)) |> 
  fc_filter(N = 60, sel_group = c("cloxacillin alone", "group 2")) |> 
  fc_draw()

