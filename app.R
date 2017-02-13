library(shiny)

source("mcmc.R")

ui <- shinyServer(fluidPage(
  plotOutput("first_column")
))

server <- shinyServer(function(input, output, session){

  sampler <- metropolis.sampler()

  incremental.steps <- 10
  total.steps <- 2e3
  step.counter <- 0

  # Plot the 30 most recent values
  output$first_column <- renderPlot({
    if (step.counter < total.steps) invalidateLater(100, session)
    sampler$step(n.steps = incremental.steps)
    plot(x = sampler$get("mu.samples"), y = sampler$get("sigma.samples"), type = "l")
    step.counter <<- step.counter + incremental.steps
  })
})

shinyApp(ui=ui,server=server)
