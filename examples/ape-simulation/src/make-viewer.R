library(htmltools)

out_html <- "index.html"

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
                       tags$h3("MCMC traceplot"),
                       tags$img(src = "out/mcmc-traceplot.png")
                     )
              )
       )

save_html(doRenderTags(html), file = out_html)
