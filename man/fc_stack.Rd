% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fc_stack.R
\name{fc_stack}
\alias{fc_stack}
\title{fc_stack}
\usage{
fc_stack(fcs, unite = FALSE)
}
\arguments{
\item{fcs}{list with all the flowcharts that we want to merge}

\item{unite}{logical value if the boxes have to be united or not. Default is FALSE.}
}
\value{
List containing a list with the datasets belonging to each flowchart and the flowchart parameters combining all the flowcharts.
}
\description{
This function allows to combine vertically two different flowcharts.
}
\examples{
# Create first flowchart for ITT
fc1 <- safo |>
  as_fc(label = "Patients assessed for eligibility") |>
  fc_filter(itt == "Yes", label = "Intention to treat (ITT)")


# Create second flowchart for PP
fc2 <- safo |>
  as_fc(label = "Patients assessed for eligibility") |>
 fc_filter(pp == "Yes", label = "Per protocol (PP)")

list(fc1, fc2) |>
  fc_stack() |>
  fc_draw()

}
