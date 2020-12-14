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
library(broom.mixed)

risk_data <- read.csv("risk_data.csv")
x <-
  c("log_cases",
    "log_deaths",
    "med_sal",
    "log_density",
    "log_pop",
    "hs_2014_18",
    "hs_1970")
y <-
  c("log_cases",
    "log_deaths",
    "med_sal",
    "log_density",
    "log_pop",
    "hs_2014_18",
    "hs_1970")
color <-
  c("log_cases",
    "log_deaths",
    "med_sal",
    "log_density",
    "log_pop",
    "hs_2014_18",
    "hs_1970")

clean_names <- tibble(
  names = x,
  clean_names = c(
    "Log COVID-19 Cases",
    "Log COVID-19 Deaths",
    "Median Salary",
    "Log Population Density",
    "Log Population",
    "2014-2018 Grad Rates",
    "1970 Grad Rates"
  )
)

# Here, we have our list of choices for our graphs' predictors and recievers
# In order to make our drop down look neater, I created a tibble entitled
# clean_names by creating a tibble with both the variable name and
# and a corresponding clean_name.

COVID <- read_csv("raw_data/County-level-data_10_19_2020.csv") %>%
  rename("Insurance" = 
           "Insurance Type (Relevant for Clinical Data from Claims Only)") %>%
  filter(State != "District of Columbia", Insurance == "All") %>%
  rename(FIPS = `FIPS County Code`) %>%
  mutate(FIPS = ifelse(nchar(FIPS) == 4, paste("0", FIPS, sep = ""), FIPS))

# Data drawn from McKinsey's COVID dashboard. Cleaned up with filters, renames,
# And the important paste function that works to make a 4-digit FIPS code into a
# 5 digit FIPS code. This is important because we are using the FIPS code
# as a reference number and need the values of each county
# to be equal in all of our situations.

county_wealth <- read_rds("county_wealth.rds")

# This is data finding mean salary by county from census data. I originally
# was going directly from the tidycensus package, but found it was easier
# to download the data onto my computer and simply read it in. This was
# important since the API key was not working.

test <- county_wealth %>%
  separate(NAME, c('County', 'State'), sep = ",") %>%
  rename("Median Salary" = "estimate") %>%
  select(c("County", "State", "Median Salary")) %>%
  mutate(State = trimws(State, which = c("both")))

# We wanted to clean this graph up just so it has County, State, and Median
# Salary. We used the trimws to clean up the statenames which was
# problematic since there were spaces on both sides of our statename.

Med_Sal <-
  left_join(test,
            fips_codes,
            by = c("County" = "county", "State" = "state_name")) %>%
  mutate(FIPS = paste(state_code, county_code, sep = ""))

# FIPS Codes are weird because they have a state number and a county number,
# so we needed to paste everything together to make the FIPS code as our
# continuous reference number (like an ID).

combined <- inner_join(COVID, Med_Sal, by = "FIPS")

education <- read_excel("raw_data/Education.xls") %>%
  filter(State != "PR") %>%
  rename("no_hs_2014_18" =
           "Percent of adults with less than a high school diploma, 2014-18",
"no_hs_1970" = "Percent of adults with less than a high school diploma, 1970") %>%
  mutate(hs_2014_18 = 100 - `no_hs_2014_18`,
         hs_1970 = 100 - no_hs_1970) %>%
  select(1:7, hs_2014_18, hs_1970)

# Here was the challenge of adding the data of graduation data, which was
# already hard to find. We used 100 - the percentage to get percent of adults
# who had achieved some educational threshold. I started off this project
# looking at SAT scores to measure educational threshold by county but
# school districts don't align exactly with individual counties, where we
# had COVID data for.

combined <-
  inner_join(combined, education, by = c("FIPS" = "FIPS Code")) %>%
  rename(
    deaths = `Deaths from COVID-19`,
    cases = `Cases of COVID-19`,
    med_sal =
      `Median Salary`,
    density = `Population Density`
  ) %>%
  mutate(
    log_pop = log(Population + 1),
    log_cases = log(cases + 1),
    log_deaths = log(deaths + 1),
    log_density = log(density + 1)
  )

# More data cleaning. A neat trick was
# using Log(+1) helps us create a more separated graph that helps separate 
# points a little further.


model <- stan_glm(
  data = combined,
  formula = log_cases ~ hs_2014_18 + I(med_sal / 10000) + log_density,
  refresh = 0
)


# Creating our model with stan_glm! Our predictors are 2014-2018 graduation
# rates, median salary divided by 10000 and log density. We use the I() to
# make our salary an integer and divide by 10000 to have a more significant
# result as each dollar is unlikely to correspond to a significant increase
# or decrease in COVID cases.

table_1 <- tbl_regression(model, intercept = TRUE) %>%
  as_gt() %>%
  tab_header(title = "COVID-19 Cases Model",
             subtitle = "An Analysis by County")

ui <- fluidPage(theme = shinytheme("cosmo"),
                navbarPage(
                  "COVID-19: A County by County Analysis",
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
                    mainPanel(fluidRow(column(
                      12,
                      gt_output('table')
                    )),
                    
                    p(
                      "Here we have our model which takes a look at COVID-19
                      Cases by county, nationally,
                      as predicted by High School graduation 
                      rates between 2014-2018, Median Salary, and the 
                      population density of the county.
                      
                      Our beta values represent the median COVID-19 Case
                      percentile by county, as predicted by median graduation
                      rate percentile, median salary percentile, and median
                      population density percentile. The 95% Confidence 
                      Interval explains that about 95% of the true observations 
                      should lie within the 95% confidence interval of 
                      their respective posterior probability distributions"
                    ),
                    fluidRow(column(
                      12,
                      gt_output('table2')
                    )),
                    
                    p("Here, we have our model looking specifically at my home
                      state of Georgia. We see that our confidence intervals
                      for graduation rates and salary levels include or are
                      close to 0 
                      suggesting weaker significance. However, density
                      is shown to have a higher beta value with a confidence
                      interval not including 0, suggesting its potential
                      importance as a predictor for COVID cases."
                      
                    ),
                    
                    fluidRow(column(
                      12,
                      gt_output('table3')
                    )),
                    p("Here, we have our model looking specifically at COVID
                    cases in Massachusetts. We see that our model has all
                      predictors with confidence intervals outside of 0,
                      suggesting that each of the predictors may be
                      significant. We see compared to Georgia our model
                      differs in that our beta values for graduation rates
                      and salary levels seem to be significant predictors. This
                      is an example of how these three predictors may be
                      confounded by a plethora of other variables including
                      political party of the state, average age in the state,
                      etc. These could be additional variables to be
                      added into our model at a later time."
                    )
                    
                    ),
                  ),
                  tabPanel(
                    "About",
                    h3("Hi! I'm PK"),
                    p("I'm a senior in Mather House studying 
                      Applied Mathematics-Biology with a secondary in
                      Psychology. I really wanted to answer a question
                      with my project, namely what factors affect
                      the number of COVID cases in every county."),
                    
      p("COVID-19 is probably the last thing you want to think about, so I did it
      for you! Today, I have drawn county-data from the McKinsey Center from
      Global Benefit COVID Dashboard to analyze the data surrounding each county
      of the United States. I have furthermore drawn graduation and census data
      to look at how these predictors may correlate to COVID Cases and Deaths
      throughout the country. The graduation data comes from the Department
      of Education and our census data comes from the tidycensus package.
      Here we have interactive graphs that
      you may work with to understand how COVID interacts with these predictors
      and furthermore have a model that looks at GA, MA, and the Nation's 
      counties with respect to the predictors of 2014-18 graduation rates,
      median salary, and population density.
      Here is a link to my github repo:", a("PK's REPO", 
                                href = "https://github.com/pkumar1999/Gov50_FP")
                    )
                  )
                ))

server <- function(input, output, session) {
  
  # - Then, you use these named objects to update the data on your site via the 
  #input object.
  # render() functions are what show content that will change live on your site.
  # so here, renderText() is updating live text based on your choice.
  
  output$state_message <- renderText({
    paste0("This is the Predictor you chose: ",
           input$Predictor, "!")
  })
  
  # I left the original comments in because I still sometimes get confused
  # what server and ui do, but just letting y'all know I didn't comment the
  # above lines that were included.
  
  results <- reactive({
    combined
  })
  
  georgia_data <- combined %>%
    filter(`State Code` == "GA")
  
  mass_data <- combined %>%
    filter(`State Code` == "MA")
  
  ga_model <-
    stan_glm(
      data = georgia_data,
      formula = log_cases ~ hs_2014_18 + I(med_sal / 10000) + log_density,
      refresh = 0
    )
  
  ma_model <-
    stan_glm(
      data = mass_data,
      formula = log_cases ~ hs_2014_18 + I(med_sal / 10000) + log_density,
      refresh = 0
    )
  
  table_2 <- tbl_regression(ga_model, intercept = TRUE) %>%
    as_gt() %>%
    tab_header(title = "COVID-19 Cases State Model",
               subtitle = paste("An Analysis of Georgia"))
  
  table_3 <- tbl_regression(ma_model, intercept = TRUE) %>%
    as_gt() %>%
    tab_header(title = "COVID-19 Cases State Model",
               subtitle = paste("An Analysis of Massachusetts"))
  
  
  output$table <- render_gt(table_1)
  output$table2 <- render_gt(table_2)
  output$table3 <- render_gt(table_3)
  
  # Rendering 3 separate tables to be included in our model tab. Render_gt
  # was from pset-7 and was a LIFE SAVER. I still could not figure out
  # how to change the rownames though which was somewhat annoying.

  output$state_plot <- renderPlot({
    x_variable <-
      clean_names$names[clean_names$clean_names == input$Predictor]
    y_variable <-
      clean_names$names[clean_names$clean_names == input$Receiver]
    test_color <-
      clean_names$names[clean_names$clean_names == input$Color]
    
    # So this code seems complicated but it basically assigns our variable name
    # a clean_name and works to output the clean_name in our dropdown list.
    # This ultimately allows us to work with the original data, but keep our
    # visuals looking clean.
    
    results() %>%
      ggplot(aes(
        x = get(x_variable),
        y = get(y_variable),
        color = get(test_color)
        
        # you use the get() function to grab a value from a consistently
        # changeable variable. Here we grab the x_variable/y_var/color name
        # and are able to output a graph as opposed to working through
        # a reactive type (Wasn't working before).
        
      )) +
      geom_point() +
      labs(
        title = input$entered_text,
        x = input$Predictor,
        y = input$Receiver,
        color = input$Color
      ) +
      theme_bw()
  })
}

shinyApp(ui, server)
