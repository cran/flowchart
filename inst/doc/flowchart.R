## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(flowchart)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("flowchart")

## -----------------------------------------------------------------------------
data(clinic_patient)
data(clinic_visit)

# Per patient dataset
str(clinic_patient)

# Per visit dataset
str(clinic_visit)

## -----------------------------------------------------------------------------
fc <- clinic_patient |> 
  as_fc() 

class(fc)
str(fc)

## -----------------------------------------------------------------------------
fc |> 
  fc_draw()

## ----fig.width = 6, fig.height = 5--------------------------------------------
clinic_patient |> 
  as_fc(label = "Patients included") |> 
  fc_filter(age >= 18 & consent == "Yes", label = "Patients included", show_exc = TRUE) |> 
  fc_draw()

## ----fig.width = 6, fig.height = 5--------------------------------------------
clinic_patient |> 
  filter(!is.na(group)) |> 
  as_fc(label = "Patients included") |> 
  fc_split(group) |> 
  fc_draw()

## ----fig.width = 8------------------------------------------------------------
# Create first flowchart for patients
fc1 <- clinic_patient |> 
  filter(!is.na(group)) |> 
  as_fc(label = "Patients included") |> 
  fc_split(group)

# Create second flowchart for visits
fc2 <- clinic_visit |> 
  filter(!is.na(group)) |> 
  as_fc(label = "Number of visits") |> 
  fc_split(group) 

list(fc1, fc2) |> 
  fc_merge() |> 
  fc_draw()

## ----warning = FALSE, fig.width = 6, fig.height = 5---------------------------
# Create first flowchart for patients
fc1 <- clinic_patient |> 
  filter(!is.na(group)) |> 
  as_fc(label = "Patients included") |> 
  fc_split(group)

# Create second flowchart for visits
fc2 <- clinic_visit |> 
  filter(!is.na(group)) |> 
  as_fc(hide = TRUE) |> 
  fc_split(group, label = c("Number of visits (Control)", "Number of visits (Treatment)"), text_pattern = "{label}\n {n}") 

list(fc1, fc2) |> 
  fc_stack() |> 
  fc_draw()

## ----fig.width = 6, fig.height = 5--------------------------------------------
fc <- clinic_patient |> 
  as_fc(label = "Patients included") |> 
  fc_filter(age >= 18 & consent == "Yes", label = "Patients included", show_exc = TRUE) |> 
  fc_modify(~.x |> 
              mutate(
                text = case_when(
                  id == 3 ~ str_glue("Excluded patients:
                                     - {sum(clinic_patient$age < 18)} under-age
                                     - {sum(clinic_patient$consent == 'No')} without a signed consent
                                     "),
                  TRUE ~ text
                )
              )) 

fc |> 
  fc_draw()

## ----fig.width = 6, fig.height = 5--------------------------------------------
fc |> 
  fc_modify(~.x |> 
              mutate(
                x = case_when(
                  id == 3 ~ 0.8,
                  TRUE ~ x
                ),
                y = case_when(
                  id == 1 ~ 0.85,
                  id == 2 ~ 0.15,
                  id == 3 ~ 0.5
                )
              )) |> 
  fc_draw()

## ----warning = FALSE, fig.width = 7, fig.height = 6---------------------------
clinic_patient |> 
  as_fc(label = "Available patients") |> 
  fc_filter(age >= 18 & consent == "Yes", label = "Patients included", show_exc = TRUE) |> 
  fc_split(group) |> 
  fc_filter(n_visits == 2, label = "Two visits available", show_exc = TRUE) |> 
  fc_split(marker_alt, label = c("Marker not alterated", "Marker alterated")) |> 
  fc_draw()

## ----warning=FALSE, fig.width = 9, fig.height = 7-----------------------------
# Create labels for exclusion box:
data(safo)

label_exc <- paste(
  c(str_glue("{sum(safo$inclusion_crit == 1 | safo$exclusion_crit == 1 | safo$decline_part == 1, na.rm = T)} excluded:"),
    map_chr(c("inclusion_crit", "decline_part", "exclusion_crit"), ~str_glue("{sum(safo[[.x]] == 1, na.rm = TRUE)} {attr(safo[[.x]], 'label')}")),
    map_chr(4:15, ~str_glue(" -  {sum(safo[[.x]] == 1)} {attr(safo[[.x]], 'label')}"))),
  collapse = "\n")

label_exc <- gsub("exclusion criteria", "exclusion criteria:", label_exc)

safo1 <- safo |> 
  filter(group == "cloxacilin alone", !is.na(reason_pp)) |> 
  mutate(reason_pp = droplevels(reason_pp))

label_exc1 <- paste(
  c(str_glue("{nrow(safo1)} excluded:"),
    map_chr(levels(safo1$reason_pp), ~str_glue(" -  {sum(safo1$reason_pp == .x)} {.x}"))),
  collapse = "\n")

label_exc1 <- str_replace_all(label_exc1, c("resistant" = "resistant\n", "blood" = "blood\n"))

safo2 <- safo |> 
  filter(group == "cloxacilin plus fosfomycin", !is.na(reason_pp)) |> 
  mutate(reason_pp = droplevels(reason_pp))

label_exc2 <- paste(
  c(str_glue("{nrow(safo2)} excluded:"),
    map_chr(levels(safo2$reason_pp), ~str_glue(" -  {sum(safo2$reason_pp == .x)} {.x}"))),
  collapse = "\n")

label_exc2 <- str_replace_all(label_exc2, c("nosocomial" = "nosocomial\n", "treatment" = "treatment\n"))

## ----warning=FALSE, fig.width = 10, fig.height = 7----------------------------
safo |> 
  as_fc("patients assessed for eligibility", text_pattern = "{n} {label}") |> 
  fc_filter(!is.na(group), label = "randomized", text_pattern = "{n} {label}", show_exc = TRUE,
            just_exc = "left", text_pattern_exc = "{label}", label_exc = label_exc, text_fs_exc = 7) |>
  fc_split(group, text_pattern = "{n} {label}") |> 
  fc_filter(itt == 1, label = "included in intention-to-treat\n population", show_exc = TRUE, 
            text_pattern = "{n} {label}", 
            label_exc = "patient did not receive allocated\n treatment (withdrew consent)", 
            text_pattern_exc = "{n} {label}", text_fs_exc = 7) |>
  fc_filter(pp == 1, label = "included in per-protocol\n population", show_exc = TRUE,
            just_exc = "left", text_pattern = "{n} {label}", text_fs_exc = 7) |> 
  fc_modify(
    ~.x |> 
      filter(id != 9) |> 
      mutate(
        text = case_when(id == 11 ~ label_exc1, id == 13 ~ label_exc2, TRUE ~ text),
        x = case_when(id == 3 ~ 0.8, id %in% c(7, 11) ~ 0.15, id == 13 ~ x + 0.035, TRUE ~ x),
        y = case_when(id == 3 ~ y + 0.015, TRUE ~ y)
      )
  ) |> 
  fc_draw()

