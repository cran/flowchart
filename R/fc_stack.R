#' @title fc_stack
#' @description This function allows to combine vertically two different flowcharts.
#'
#' @param fcs list with all the flowcharts that we want to merge
#' @param unite logical value if the boxes have to be united or not. Default is FALSE.
#' @return List containing a list with the datasets belonging to each flowchart and the flowchart parameters combining all the flowcharts.
#'
#' @examples
#' # Create first flowchart for ITT
#' fc1 <- safo |>
#'   as_fc(label = "Patients assessed for eligibility") |>
#'   fc_filter(itt == "Yes", label = "Intention to treat (ITT)")
#'
#'
#' # Create second flowchart for PP
#' fc2 <- safo |>
#'   as_fc(label = "Patients assessed for eligibility") |>
#'  fc_filter(pp == "Yes", label = "Per protocol (PP)")
#'
#' list(fc1, fc2) |>
#'   fc_stack() |>
#'   fc_draw()
#'
#' @export
#' @importFrom rlang .data

fc_stack <- function(fcs, unite = FALSE) {

  purrr::map(fcs, ~is_class(.x, "fc"))

  object <- tibble::tibble(
    id = 1:length(fcs),
    data = purrr::map(fcs, ~.x$data),
    fc = purrr::map(fcs, ~.x$fc)
  )

  object <- object |>
    dplyr::mutate(
      fc = purrr::map(seq_along(.data$fc), function(i) {
        .data$fc[[i]] |>
          dplyr::mutate(
            y = update_y_stack(.data$y, .data$x, .data$type, i, length(fcs))
          )
      })
    )

  if (unite) {

    object <- list(
      data = object$data,
      fc = do.call(rbind, purrr::map(seq_along(object$fc), ~object$fc[[.x]] |>
                                       dplyr::mutate(fc = .x))) |>
        dplyr::mutate(
          y = update_y_stack_unite(.data$y, .data$x, .data$type),
          change = dplyr::case_when(
            is.na(dplyr::lag(.data$fc)) ~ FALSE,
            fc != dplyr::lag(.data$fc) ~ TRUE,
            TRUE ~ FALSE
          )
        ) |>
        dplyr::group_by(.data$y) |>
        dplyr::mutate(
          type = dplyr::case_when(
            any(.data$change) ~ "stack",
            TRUE ~ .data$type
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::select(-"change") |>
        #Recalculate ids
        dplyr::mutate(
          id = dplyr::row_number()
        )
    )

    #We can only unite the boxes if either the last level of the previous flowchart or the first level of the following flowchart have only one box, or the same number of boxes.

    n_fc <- object$fc |>
      dplyr::group_by(.data$fc) |>
      dplyr::summarise(
        n_first = sum(.data$y == max(.data$y)),
        n_last = sum(.data$y == min(.data$y))
      ) |>
      dplyr::mutate(
        n_last = dplyr::lag(.data$n_last)
      ) |>
      dplyr::filter(dplyr::row_number() != 1)

    if(with(n_fc, any(n_first != n_last & n_first > 1 & n_last > 1))) {
      cli::cli_abort(
        c(
        "Flowcharts can't be united because they have a different number of boxes in their connecting levels.",
        "i" = "Set {.code unite = FALSE}."
        )
      )
    }

    object$fc <- object$fc |>
      dplyr::select(-"fc")

  }

  class(object) <- "fc"

  object

}
