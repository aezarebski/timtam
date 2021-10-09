library(htmltools)
library(base64enc)
library(purrr)

csv_as_table <- function(filepath, ...) {
  df <- read.csv(filepath)
  tmp_file <- tempfile()
  print(xtable::xtable(df, ...),
    type = "html",
    file = tmp_file
  )
  return(tags$div(includeHTML(tmp_file)))
}

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

diagnostics_div <- function() {
  tags$div(
    tags$h3("Diagnostics"),
    tags$div(
      tags$h5("Effective sample size"),
      png_as_img("out/effective-sample-sizes.png", style = "height: 900px;")
    )
  )
}

build_index <- function() {
  out_html <- "index.html"
  html_body <- tags$body(
    tags$div(
      tags$h1("CI calibration example II"),
      tags$div(
        tags$h3("Unscheduled data"),
        png_as_img(filepath = "out/prevalence-calibration-extra-2.png", style = "height: 900px;"),
        png_as_img(filepath = "out/estimate-calibration.png", style = "height: 900px;"),
        png_as_img(filepath = "out/mse-r-naught.png", style = "height: 900px;"),
        tags$div(
          tags$h5("R-naught coverage"),
          csv_as_table(filepath = "out/r-naught-coverage-table.csv"),
          tags$h5("Parameter coverage"),
          csv_as_table(filepath = "out/estimates-coverage.csv")
        )
      ),
      tags$div(
        tags$h3("Aggregated data"),
        png_as_img(filepath = "out/aggregated-estimate-calibration.png", style = "height: 900px;"),
        tags$div(
          tags$h5("Parameter coverage"),
          csv_as_table(filepath = "out/aggregated-estimates-coverage.csv")
        )
      ),
      tags$div(
        tags$h3("Comparing prevalence estimates"),
        png_as_img(filepath = "out/mse-prevalence.png", style = "height: 900px;"),
        tags$div(
          tags$h5("Coverage of prevalence estimates"),
          csv_as_table(filepath = "out/prevalence-coverage-table.csv")
        ),
        tags$div(
          tags$h5("Relative proportional error"),
          png_as_img(filepath = "out/prevalence-calibration.png", style = "height: 900px;")
        ),
        tags$div(
          tags$h5("Absolute error"),
          png_as_img(filepath = "out/prevalence-calibration-extra-1.png", style = "height: 900px;")
        )
      ),
      diagnostics_div()
    )
  )
  save_html(html_body, file = out_html)
  cat(
    "Results can be viewed by pointing browser to\n\t",
    paste("file:/", getwd(), out_html, sep = "/"),
    "\n"
  )
}

main <- function() {
  build_index()
}

if (!interactive()) {
  main()
}
