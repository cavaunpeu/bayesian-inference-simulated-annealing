library(shiny)
library(magrittr)

ui <- shinyServer(fluidPage(
  plotOutput("first_column")
))

server <- shinyServer(function(input, output, session){
  # Function to get new observations
  get_new_data <- function(){
    data <- rnorm(5) %>% rbind %>% data.frame
    return(data)
  }

  # Initialize my_data
  my_data <<- get_new_data()

  # Function to update my_data
  update_data <- function(){
    my_data <<- rbind(get_new_data(), my_data)
  }

  # Plot the 30 most recent values
  output$first_column <- renderPlot({
    print("Render")
    invalidateLater(100, session)
    update_data()
    plot(X1 ~ 1, data=my_data[1:30,], ylim=c(-3, 3), las=1, type="l")
  })
})

shinyApp(ui=ui,server=server)
