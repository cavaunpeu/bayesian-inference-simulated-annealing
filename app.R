library(shiny)

source("mcmc.R")

ui <- fluidPage(

  titlePanel("Bayesian Inference via Simulated Annealing"),

  fixedRow(
    column(
      width = 4,
      wellPanel(
        h3("What's this?"),
        helpText("This is an application that ingests week-level RescueTime productivity data then seeks to infer the expected value of a typical
                 week. The result is a probability distribution across the proportion of your week you typically spend on", tags$b("Very Distracting,"), tags$b("Distracting,"), tags$b("Neutral,"),
                 tags$b("Productive"), "and", tags$b("Very Productive"), "activities.")
      ),
      wellPanel(
        h3("Download report"),
        helpText(
          "Please download your week-level report at the following",
          tags$a(href="https://www.rescuetime.com/browse/productivity/by/week/for/the/year/of/2016-01-01", "link."),
          "Should you not have one, you're free to use",
          tags$a(href="https://github.com/cavaunpeu/rescue-time-estimation/blob/publish/data/rescue_time_report.csv", "mine.")
        ),
        tags$hr(),
        h3("Upload report"),
        fileInput(
          inputId = "rescue.time.report",
          label = NULL,
          multiple = FALSE,
          accept = c("text/csv, text/comma-separated-values", ".csv")
        )
      ),
      wellPanel(
        h3("Documentation"),
        helpText(
          "Voilà the",
          tags$a(href="https://github.com/cavaunpeu/rescue-time-estimation", "code"),
          "and",
          tags$a(href="http://wp.me/p4zXJT-fm", "blog post"),
          "accompanying this project."
        )
      )
      ),
    column(
      width = 8,
      plotOutput("plotPanel", height = "750px", width = "auto")
    )
  )
)

server <- shinyServer(function(input, output, session){

  sampler <- metropolis.sampler()

  incremental.steps <- 25
  total.steps <- 2e3
  step.counter <- 0

  # Plot the 30 most recent values
  output$plotPanel <- renderPlot({
    if (step.counter < total.steps) invalidateLater(100, session)
    sampler$step(n.steps = incremental.steps)
    plot(x = sampler$get("mu.samples"), y = sampler$get("sigma.samples"), type = "l")
    step.counter <<- step.counter + incremental.steps
  })
})

shinyApp(ui=ui,server=server)
