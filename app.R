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
        helpText("This is a visual demonstration of a simple MCMC sampler."),
        helpText(
          "First, \\(N = 100\\) data points are generated artificially from a normal distribution with
          \\(\\mu_{true} = 3\\) , \\(\\sigma_{true} = .5\\)."
        ),
        helpText("Next, we attempt to recover these parameters via MCMC using the observed data alone."),
        tags$hr(),
        h3("Sampler Strategy"),
        helpText("Metroplis.")
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

  sampler <- simulated.annealing.sampler()

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
