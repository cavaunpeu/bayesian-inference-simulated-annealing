library(shiny)
library(latex2exp)

source("mcmc.R")

ui <- fluidPage(

  titlePanel("Bayesian Inference via Simulated Annealing"),
  withMathJax(),

  fixedRow(
    column(
      width = 4,
      wellPanel(
        h3("What's this?"),
        helpText(
          "This is a visual demonstration of a simple MCMC sampler. First, \\(N = 100\\) data points are generated
          artificially from a normal distribution with \\(\\mu_{true} = 3\\) and \\(\\sigma_{true} = .5\\). Next,
          we attempt to recover these true parameters via MCMC using the observed data alone."
          )
      ),
      wellPanel(
        h3("Data Generation"),
        helpText("The observed data can be generated with the following code: `y <- rnorm(n = 100, mean = 3, sd = 5)`."),
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
    plot(
      x = sampler$get("mu.samples"), y = sampler$get("sigma.samples"),
      type = "l",
      xlab = latex2exp::TeX("$\\mu$"), ylab = latex2exp::TeX("$\\sigma$"), main = latex2exp::TeX("Joint Trace Plot of $\\mu$ and $\\sigma$ Samples")
    )
    step.counter <<- step.counter + incremental.steps
  })
})

shinyApp(ui=ui,server=server)
