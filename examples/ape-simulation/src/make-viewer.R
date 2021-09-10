library(htmltools)
library(base64enc)

mcmc_diagnostics <- jsonlite::read_json("out/mcmc-diagnostics.json")
out_html <- "index.html"

foo <- lapply(mcmc_diagnostics, unlist) |> as.data.frame()
bar <- c()
for (ix in seq.int(nrow(foo))) {
  if (ncol(foo) == 4) {
    bar <- c(bar, sprintf("%s, %f, %f, %f", foo[ix,1], foo[ix,2], foo[ix,3], foo[ix, 4]))
  } else if (ncol(foo) == 3) {
    bar <- c(bar, sprintf("%s, %f, %f", foo[ix,1], foo[ix,2], foo[ix,3]))
  } else {
    stop("")
  }
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

html_body <-
  tags$body(
         tags$h1("ape simulation example"),
         tags$div(
                tags$h3("Simulated data"),
                png_as_img(filepath = "out/ape-simulation-figure.png",
                           style = "width: 1000px;")
              ),
         tags$div(
                tags$h3("MCMC summary"),
                tags$div(
                       tags$p(paste(names(foo), collapse = ", ")),
                       tags$ul(purrr::map(bar, tags$li))
                     ),
                tags$div(
                       tags$h5("Posterior distribution of parameters"),
                       png_as_img(filepath = "out/posterior-marginals.png",
                                  style = "width: 1000px;"),
                       tags$h5("Posterior distribution of R-naught"),
                       png_as_img(filepath = "out/posterior-r-naught.png",
                                  style = "width: 600px;")
                     )
              ),
         tags$div(
                tags$h3("MCMC traceplot"),
                png_as_img(filepath = "out/mcmc-traceplot-1.png"),
                png_as_img(filepath = "out/mcmc-traceplot-2.png")
              )
       )

save_html(html_body, file = out_html)
