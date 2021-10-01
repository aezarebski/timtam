library(htmltools)
library(base64enc)
library(purrr)

out_html <- "index.html"

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
      lapply(fps, \(fp) png_as_img(
        filepath = fp,
        style = "width: 600px;"
      ))
    )
  lift_dl(tags$div)(subtags)
}

marginal_div <- function(fp1, fp2, fp3) {
  if (!is.null(fp2)) {
    tags$div(
           tags$h5("Posterior marginals"),
           png_as_img(filepath = fp1, style = "width: 900px;"),
           tags$h5("Reproduction number"),
           png_as_img(filepath = fp2, style = "width: 900px;"),
           tags$h5("Prevalence"),
           png_as_img(filepath = fp3, style = "width: 900px;")
         )
  } else {
    tags$div(
           tags$h5("Posterior marginals"),
           png_as_img(filepath = fp1, style = "width: 900px;"),
           tags$h5("Prevalence"),
           png_as_img(filepath = fp3, style = "width: 900px;")
         )
  }
}

splom_div <- function(fp) {
  tags$div(
         tags$h5("Posterior SPLOM"),
         png_as_img(filepath = fp, style = "width: 900px;"))
}

#' =============================================================================
#' Construct the actual HTML page
#' =============================================================================


html_body <-
  tags$body(
         tags$h1("ape simulation example II"),
         tags$h3("Simulated data"),
         png_as_img(filepath = "out/ape-simulation-figure.png", style = "width: 900px;"),
         tags$h3("Aggregated simulated data"),
         png_as_img(filepath = "out/ape-simulation-figure-aggregated.png", style = "width: 900px;"),
    tags$div(
      tags$h3("Unscheduled data"),
      tags$div(
             marginal_div("out/unscheduled-data/marginal-distributions.png",
                          "out/unscheduled-data/r-naught.png",
                          "out/unscheduled-data/prevalence.png"),
             splom_div("out/unscheduled-data/splom.png"),
             traceplot_div(list.files(
               path = "out/unscheduled-data/",
               pattern = "*traceplot*",
               full.names = TRUE
))
      )
      ),
    tags$div(
           tags$h3("Aggregated data"),
           tags$div(
                  marginal_div("out/aggregated-data/marginal-distributions.png",
                               NULL,
                               "out/aggregated-data/prevalence.png"),
                  splom_div("out/aggregated-data/splom.png"),
                  traceplot_div(list.files(
                    path = "out/aggregated-data/",
                    pattern = "*traceplot*",
                    full.names = TRUE
))
                )
         )
  )

save_html(html_body, file = out_html)
cat(
  "Results can be viewed by pointing browser to\n\t",
  paste("file:/", getwd(), out_html, sep = "/"),
  "\n"
)
