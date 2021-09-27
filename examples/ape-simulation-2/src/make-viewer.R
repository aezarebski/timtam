library(htmltools)
library(base64enc)
library(purrr)

#' An HTML tag encoding an image stored in a PNG.
#'
#' This uses the \code{base64enc} and \code{htmltools} packages.
#'
#' @param filepath is the path to the PNG
#' @param ... is additional arguments to \code{tags$img} such as style.
#'
png_as_img <- function(filepath, ...) {
  if (tools::file_ext(filepath) == "png") {
    b64 <- base64enc::base64encode(what = filepath)
    tags$img(
           src = paste("data:image/png;base64", b64, sep = ","),
           ...
         )
  } else {
    stop("Filepath given to png_as_img must be a PNG.")
  }
}

traceplot_div <- function(fps) {
  subtags <-
    c(
      list(tags$h5("MCMC traceplot")),
      lapply(fps, \(fp) png_as_img(filepath = fp,
                                   style = "width: 600px;"
                                   ))
    )
  lift_dl(tags$div)(subtags)
}

marginal_div <- function(fp) {
  tags$div(
         tags$h5("Posterior marginals"),
         png_as_img(filepath = fp, style = "width: 900px;")
       )
}

#' =============================================================================
#' Construct the actual HTML page
#' =============================================================================

unscheduled_data_traceplot_fps <- list.files(
  path = "out/unscheduled-data/",
  pattern = "*traceplot*",
  full.names = TRUE
)


html_body <-
  tags$body(
         tags$h1("ape simulation example II"),
         tags$div(
                tags$h3("Unscheduled data"),
                tags$div(
                       marginal_div("out/unscheduled-data/marginal-distributions.png"),
                       traceplot_div(unscheduled_data_traceplot_fps)
                     )
              )
       )

save_html(html_body, file = "index.html")
