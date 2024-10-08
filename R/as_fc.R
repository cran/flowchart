#' @title as_fc
#' @description This function allows to initialize a flow chart given any database. It will create a fc object showing the number of rows of the database. If a database is not available, the user can instead directly enter the number of rows in the study.
#'
#' @param .data Data frame to be initialised as a flowchart.
#' @param N Number of rows of the study in case `.data` is NULL.
#' @param label Character with the text that will be shown in the box.
#' @param text_pattern Structure that will have the text in the box. It recognizes label, n, N and perc within brackets. For default it is "\{label\}\\n\{N\}".
#' @param just Justification for the text: left, center or right. Default is center.
#' @param text_color Color of the text. It is black by default. See the `col` parameter for \code{\link{gpar}}.
#' @param text_fs Font size of the text. It is 8 by default. See the `fontsize` parameter for \code{\link{gpar}}.
#' @param text_fface Font face of the text. It is 1 by default. See the `fontface` parameter for \code{\link{gpar}}.
#' @param text_ffamily Changes the font family of the text. Default is NA. See the `fontfamily` parameter for \code{\link{gpar}}.
#' @param text_padding Changes the text padding inside the box. Default is 1. This number has to be greater than 0.
#' @param bg_fill Box background color. It is white by default. See the `fill` parameter for \code{\link{gpar}}.
#' @param border_color Box border color. It is black by default. See the `col` parameter for \code{\link{gpar}}.
#' @param hide Logical value to hide the initial box or not. Default is FALSE. hide = TRUE can only be combined with fc_split().
#'
#' @return List with the dataset and the initialized flowchart parameters.
#'
#' @examples
#' safo |>
#' as_fc(label = "Patients assessed for eligibility") |>
#' fc_draw()
#'
#' @export

as_fc <- function(.data = NULL, N = NULL, label = "Initial dataframe", text_pattern = "{label}\n{N}", just = "center", text_color = "black", text_fs = 8, text_fface = 1, text_ffamily = NA, text_padding = 1, bg_fill = "white", border_color = "black", hide = FALSE) {

  if(is.null(.data) & is.null(N)) {
    stop("Either `.data` or `N` arguments have to be specified.")
  }else if(!is.null(.data) & !is.null(N)) {
    stop("`.data` and `N` arguments cannot be specified simultaneously.")
  }

  if(!is.null(.data)) {
    N <- nrow(.data)
  } else {
    .data <- tibble::tibble(id = 1:N)
  }

  if(text_padding == 0) {
    stop("Text padding cannot be equal to zero")
  }

  if(!hide) {
    fc_new <- tibble::tibble(
      id = 1,
      x = 0.5,
      y = 0.5,
      n = N,
      N = N,
      perc = "100",
      text = as.character(stringr::str_glue(text_pattern)),
      type = "init",
      group = NA,
      just = just,
      text_color = text_color,
      text_fs = text_fs,
      text_fface = text_fface,
      text_ffamily = text_ffamily,
      text_padding = text_padding,
      bg_fill = bg_fill,
      border_color = border_color
    )
  } else {
    warning("Remember that hide = TRUE can only be combined with fc_split()")
    fc_new <- NULL
  }

  #Initialize flowchart as x is a dataframe
  object <- list(
    data = .data |>
      dplyr::ungroup(),
    fc = fc_new
  )
  class(object) <- c("fc")
  object
}
