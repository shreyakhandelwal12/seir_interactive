setwd(dir="/Users/shreya/Documents")
library(shiny)
library(deSolve)
ui <- fluidPage(
  
  # App title ----
  titlePanel("An Interactive SEIR Model"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      h5("Choose the values of the input parameters", align = "center"),
      # Input: Slider for the number of bins ----
      textInput(inputId = "contact_rate",
                  label = "Contact Rate:", value = "50"),
      textInput(inputId = "transmission_probability",
                label = "Transmission Probability", value = "0.70"),
      textInput(inputId = "infectious_period",
                label = "Infectious Period", value = "14"),
      textInput(inputId = "latent_period",
                label = "Latent Period", value = "7"),
      sliderInput(inputId = "S", label = "Susceptible Hosts",
                  min = 0, max = 1000, value = 900),
      sliderInput(inputId = "E", label = "Exposed Hosts",
                  min = 0, max = 100, value = 1),
      sliderInput(inputId = "I", label = "Infected Hosts",
                  min = 0, max = 100, value = 1),
      sliderInput(inputId = "R", label = "Recovered Hosts",
                  min = 0, max = 1000, value = 0)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(

        plotOutput(outputId = "plot1")
      # Output: Histogram ----
      #fluidRow(
        #textOutput(outputId = "plot2")
      #)
      
    )
  )
)
#server

server <- function(input, output) {
  output$plot1<-renderPlot({
    
    seir_model = function (current_timepoint, state_values, parameters)
    {
      # create state variables (local variables)
      S = state_values [1]        # susceptibles
      E = state_values [2]        # exposed
      I = state_values [3]        # infectious
      R = state_values [4]        # recovered
      
      with ( 
        as.list (parameters),     # variable names within parameters can be used 
        {
          # compute derivatives
          dS = (-beta * S * I)
          dE = (beta * S * I) - (delta * E)
          dI = (delta * E) - (gamma * I)
          dR = (gamma * I)
          
          # combine results
          results = c (dS, dE, dI, dR)
          list (results)
        }
      )
    }
   
    
    contact_rate = as.numeric(input$contact_rate)                     # number of contacts per day
    transmission_probability = as.numeric(input$transmission_probability)       # transmission probability
    infectious_period = as.numeric(input$infectious_period)                 # infectious period
    latent_period = as.numeric(input$latent_period)                    # latent period
    
    beta_value = contact_rate * transmission_probability
    gamma_value = 1 / infectious_period
    delta_value = 1 / latent_period
    Ro = beta_value / gamma_value
    parameter_list = c (beta = beta_value, gamma = gamma_value, delta = delta_value) #disease parameter list
    
    W = as.numeric(input$S)        # susceptible hosts
    X = as.numeric(input$I)            # infectious hosts
    Y = as.numeric(input$R)            # recovered hosts
    Z = as.numeric(input$E)            # exposed hosts
    N = W + X + Y + Z
    initial_values = c (S = W/N, E = X/N, I = Y/N, R = Z/N)
    timepoints = seq (0, 50, by=1)
    output = lsoda (initial_values, timepoints, seir_model, parameter_list)
    # susceptible hosts over time
    plot (S ~ time, data = output, type='l', col = 'blue', ylab = 'S, E, I, R', main = 'SEIR epidemic')
    legend(x=35, y=450, legend = c("S","E","I","R"), col = c('blue','purple','red','green'), lty = 1)
    
    
    # remain on same frame
    par (new = TRUE)    
    
    # exposed hosts over time
    plot (E ~ time, data = output, type='l', col = 'purple', ylab = '', axes = FALSE) 
    
    # remain on same frame
    par (new = TRUE)  
    
    # infectious hosts over time
    plot (I ~ time, data = output, type='l', col = 'red', ylab = '', axes = FALSE) 
    
    # remain on same frame
    par (new = TRUE)  
    
    # recovered hosts over time
    plot (R ~ time, data = output, type='l', col = 'green', ylab = '', axes = FALSE)  
  })
  
  
}

my_app<-shinyApp(ui = ui, server = server)


