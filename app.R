library(shiny) # you may need to install.packages() this
library(tidyverse)

library(shiny)
library(fec16)


risk_data <- read.csv("risk_data.csv")
age_groups <- c("<20 years", "20 to 44", "45 to 64", "65 plus")
######################################################################################
######################################################################################
#
# 1. Shiny Apps have two basic parts to them
#
#   - The user interface (UI) defines how the app should look.
#
#     -- For example, the text on the page, the placement of the elements, etc.
#
#   - The server defines how the app should behave.
#
#     -- This is how live elements are updated - like selecting a state from a list.
#
#   - Those two pieces are combined by running shinyApp(ui, server) to create the app.
#
#      -- You can also click the green "Run App" button on the top right or
#         run runApp() in the console

ui <- fluidPage(navbarPage(
  "Shiny Example",
  tabPanel(
    "Main",
    # - UIs are built from "panel" functions, which specify areas of your page.
    #
    #   -- There is a "main panel," a "sidebar," a "title," etc.
    # Here is a sidebar!
    sidebarPanel(
      selectInput(
        inputId = "age_group",                 # a name for the value you choose here
        label = "Choose an Age Group",   # the name to display on the slider
        choices = age_groups             # your list of choices to choose from
      ),
      
    ),
    
    
    # And here is your "main panel" for the page.
    
    mainPanel(
      # - You can also make your UI more complicated with UI elements.
      #
      #   -- In general, these are defined by functions that you give arguments to 
      #      (e.g. min and max values).
      #
      # - These include:
      #
      #   -- selectInput() to choose from multiple options.
      #
      #   -- sliderInput() lets you choose a value from a slider of values you define.
      #
      #   -- radioButtons() let you choose a button from a number of options
      #
      #   -- textInput() lets you enter whatever text you want.
      #
      #   -- Lots of other options, like entering a date. Look at the resources for 
      #      other choices!
      #
      # - You then assign these inputs to a value and use those values in other places, 
      #   like in plots!
      #
      # - All of these functions have their own arguments. For example:
      
      textInput(
        inputId = "entered_text",               # a name for the value you choose here
        label = "Place your title text here:",  # a label above the text box
        value = "Example Title"                 # an initial value for the box
      ),
      
      textOutput("state_message"),              # load a text object called "state_message"
      textOutput("size_message"),
      textOutput("color_message"),
      textOutput("text_message"),
      plotOutput("state_plot")
    )
  ),
  tabPanel("About",
             h3("Hello, my name is PK and this is my final project for GOV 50. 
                Currently I am working with a dataset entitled: United States 
                Health-Care Spending Attributable to Modifiable Risk Factors 
                in 2016. This data is a merger of two existing studies and was 
                created by The Insitute for Health Metrics and Evaluation's 
                Disease Expenditure Study 2016. While I expect to change the
                data I work with, this is an interesting visualization.
                Here is the link for my repo
                https://github.com/pkumar1999/Gov50_FP.git"))
  )
)

server <- function(input, output, session) {
  # - Then, you use these named objects to update the data on your site via the input object.
  #
  #   -- render() functions are what show content that will change live on your site.
  #
  #   -- so here, renderText() is updating live text based on your choice.
  
  output$state_message <- renderText({
    paste0("This is the age group you chose: ", # this is just a string, so it will never change
           input$age_group, "!")       # this is based on your input, selected_state defined above.
  })
  # This line makes our dataset reactive.
  # That is, we can update it based on the values of input that define above.
  
  results <- reactive({
    risk_data
  })
  
  # Just like renderText(), we can renderPlot()!
  
  output$state_plot <- renderPlot({
    results() %>%
      filter(age_group_name == input$age_group) %>%
      ggplot(aes(x = risk_id, y = mean, color = age_group_name)) +
      geom_point() +
      labs(title = input$entered_text, x = "Risk ID", y = "Mean") +
      theme_bw()
  })
  
}

shinyApp(ui, server)
