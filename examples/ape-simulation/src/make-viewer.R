library(htmltools)

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


html <-
  tags$html(
         tags$head(tags$title("ape simulation example")),
         tags$body(
                tags$h1("ape simulation example"),
                tags$div(
                       tags$h3("Simulated data"),
                       tags$img(src = "out/ape-simulation-figure.png",
                                style = "width: 1000px;")
                     ),
                tags$div(
                       tags$h3("MCMC summary"),
                       tags$p(paste(names(foo), collapse = ", ")),
                       tags$ul(purrr::map(bar, tags$li))
                     ),
                tags$div(
                       tags$h3("MCMC traceplot"),
                       tags$img(src = "out/mcmc-traceplot-1.png"),
                       tags$img(src = "out/mcmc-traceplot-2.png")
                     )
              )
       )

save_html(doRenderTags(html), file = out_html)
