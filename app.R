library(shiny)
library(tidyverse)
library(shiny)
library(fec16)
library(tidycensus)
library(readxl)
library(usmap)
library(rstanarm)
library(gt)
library(gtsummary)
library(shinythemes)
xlibrary(broom.mixed)

risk_data <- read.csv("risk_data.csv")
x <- c("cases", "deaths", "med_sal", "log_pop", "hs_2014_18", "hs_1970")
y <- c("cases", "deaths", "med_sal", "log_pop", "hs_2014_18", "hs_1970")
color <-c("cases", "deaths", "med_sal", "log_pop", "hs_2014_18", "hs_1970")
clean_names <- tibble(names = x, clean_names = c("COVID-19 Cases", 
                                                 "COVID-19 Deaths",
                                                 "Median Salary", 
                                                 "Log Population", 
                                                 "2014-2018 Grad Rates", 
                                                 "1970 Grad Rates"))

COVID <- read_csv("raw_data/County-level-data_10_19_2020.csv") %>%
  rename("Insurance" = "Insurance Type (Relevant for Clinical Data from Claims Only)") %>%
  filter(State != "District of Columbia", Insurance == "All") %>%
  rename(FIPS = `FIPS County Code`) %>%
  mutate(FIPS = ifelse(nchar(FIPS) == 4, paste("0", FIPS, sep = ""), FIPS))

county_wealth <- get_acs(geography = "county",
                         variables = c(medincome = "B19013_001"),
                         state = c(state.abb),
                         year = 2018)

test <- county_wealth %>%
  separate(NAME, c('County', 'State'), sep=",") %>%
  rename("Median Salary" = "estimate") %>%
  select(c("County", "State", "Median Salary")) %>%
  mutate(State = trimws(State, which = c("both")))

Med_Sal <- left_join(test, fips_codes, by = c("County" = "county", "State" = "state_name")) %>%
  mutate(FIPS = paste(state_code, county_code, sep = ""))

combined <- inner_join(COVID, Med_Sal, by = "FIPS")

education <- read_excel("raw_data/Education.xls") %>%
  filter(State != "PR") %>%
  rename("no_hs_2014_18" =
           "Percent of adults with less than a high school diploma, 2014-18", "no_hs_1970" = "Percent of adults with less than a high school diploma, 1970") %>%
  mutate(hs_2014_18 = 100 - `no_hs_2014_18`, hs_1970 = 100 - no_hs_1970) %>%
  select(1:7, hs_2014_18, hs_1970)

combined <- inner_join(combined, education, by = c("FIPS" = "FIPS Code")) %>%
  rename(deaths = `Deaths from COVID-19`, cases = `Cases of COVID-19`, med_sal = 
           `Median Salary`, density = `Population Density`) %>%
  mutate(log_pop = log(Population+1), log_cases = log(cases+1), log_density = log(density+1))

model <- stan_glm(
  data = combined,
  formula = log_cases ~ hs_2014_18 + I(med_sal/10000) + log_density,
  refresh = 0
)

table_1 <- tbl_regression(model, intercept = TRUE) %>%
  as_gt() %>%
  tab_header(title = "Test",
             subtitle = "Test")

# PLOTS FOR BIG PICTURE LOOK ON FIRST TAB OF SHINY
# SECOND TAB IS MODEL
# CONTROL FOR OTHER THINGS AND LOOK AT LOG
# PRINT OUT REGRESSION TABLE AND WALK THROUGH COEFFICIENTS (LAST PSET)
# USE MODEL TO PREDICT FOR SPECIFIC COUNTIES (AVG COUNTY SIZE/AVG GRAD RATE)


ui <- fluidPage(
  theme = shinytheme("cosmo"),
  navbarPage(
  "PK's Final Project",
  tabPanel(
    "Main",
    sidebarPanel(
      selectInput(
        inputId = "Predictor",
        # a name for the value you choose here
        label = "Choose the X Axis",
        # the name to display on the slider
        choices = clean_names$clean_names             
      ),
      selectInput(
        inputId = "Receiver",
        # a name for the value you choose here
        label = "Choose the Y Axis",
        # the name to display on the slider
        choices = clean_names$clean_names             
      ),
      selectInput(
        inputId = "Color",
        # a name for the value you choose here
        label = "Choose the Color Coding",
        # the name to display on the slider
        choices = clean_names$clean_names             
      ),
    ),
    mainPanel(
      textInput(
        inputId = "entered_text",
        # a name for the value you choose here
        label = "Place your title text here:",
        # a label above the text box
        value = "Example Title"                 
      ),
      
      textOutput("state_message"),
      # load a text object called "state_message"
      textOutput("size_message"),
      textOutput("color_message"),
      textOutput("text_message"),
      plotOutput("state_plot")
    )
  ),
  
  tabPanel(
    "Models",
    sidebarPanel(
      selectInput(
        inputId = "Predictor",
        # a name for the value you choose here
        label = "Choose the X Axis",
        # the name to display on the slider
        choices = clean_names$clean_names
      ),
      selectInput(
        inputId = "Receiver",
        # a name for the value you choose here
        label = "Choose the Y Axis",
        # the name to display on the slider
        choices = clean_names$clean_names             
      ),
      mainPanel(
        fluidRow(
          column(12,
                 gt_output('table')
          ))
        ),
      )),
  tabPanel(
    "About",
    h3(
      "Hello, my name is PK and this is my final project for GOV 50. I
             have decided to pursue my final project analyzing the effect of
             county education and wealth statistics on the impact of COVID-19
             in terms of cases and deaths. Here is a link to my github repo:
            https://github.com/pkumar1999/Gov50_FP.git"
    )
  )
))

server <- function(input, output, session) {
  # - Then, you use these named objects to update the data on your site via the input object.
  #
  #   -- render() functions are what show content that will change live on your site.
  #
  #   -- so here, renderText() is updating live text based on your choice.
  
  output$state_message <- renderText({
    paste0("This is the Predictor you chose: ", # this is just a string, so it will never change
           input$Predictor, "!")       # this is based on your input, selected_state defined above.
  })

  results <- reactive({
    combined
  })
  
  output$table <- render_gt(table_1)
  
  output$state_plot <- renderPlot({
    x_variable <- clean_names$names[clean_names$clean_names == input$Predictor]
    y_variable <- clean_names$names[clean_names$clean_names == input$Receiver]
    test_color <- clean_names$names[clean_names$clean_names == input$Color]
    results() %>%
      ggplot(aes(x = get(x_variable), y = get(y_variable), color = get(test_color))) +
      geom_point() +
      labs(title = input$entered_text,
           x = input$Predictor,
           y = input$Receiver,
           color = input$Color) +
      theme_bw()
  }) 
}

shinyApp(ui, server)
