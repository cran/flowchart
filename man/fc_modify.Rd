% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fc_modify.R
\name{fc_modify}
\alias{fc_modify}
\title{fc_modify}
\usage{
fc_modify(object, fun, ...)
}
\arguments{
\item{object}{flowchart created as a fc object.}

\item{fun}{A function or formula that will be applied to `.$fc`. If a _function_, it is used as is. If a _formula_, e.g. `fun = ~.x |> mutate(x = x + 0.2)`, it is converted to a function.}

\item{...}{Additional arguments passed on to the mapped function.}
}
\value{
List with the dataset and the modified flowchart parameters.
}
\description{
This function allows to modify the `.$fc` tibble included in each fc object that contains all the parameters of the flowchart.
}
\examples{
#Example 1 (change text):
clinic_patient |>
  as_fc(label = "Patients included") |>
  fc_filter(age >= 18 & consent == "Yes", label = "Patients included", show_exc = TRUE) |>
  fc_modify(~.x |>
              dplyr::mutate(
                text = dplyr::case_when(
                  id == 3 ~ stringr::str_glue("Excluded patients:
                                     - {sum(clinic_patient$age < 18)} under-age
                                     - {sum(clinic_patient$consent == 'No')} no consent
                                     "),
                  TRUE ~ text
                )
              )) |>
  fc_draw()

#Example 2 (change coordinates):
clinic_patient |>
  as_fc(label = "Patients included") |>
  fc_filter(age >= 18 & consent == "Yes", label = "Patients included", show_exc = TRUE) |>
  fc_modify(~.x |>
             dplyr::mutate(
               x = dplyr::case_when(
                 id == 3 ~ 0.8,
                 TRUE ~ x
               ),
               y = dplyr::case_when(
                 id == 1 ~ 0.85,
                 id == 2 ~ 0.15,
                 id == 3 ~ 0.5
               )
             )) |>
   fc_draw()
}