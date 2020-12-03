library(tidycensus)
library(tidyverse)
library(readxl)
library(usmap)

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
  rename("no_hs_2014_18" = "Percent of adults with less than a high school 
         diploma, 2014-18", "no_hs_1970" = "Percent of adults with less than a high school diploma, 1970") %>%
  mutate(hs_2014_18 = 100 - `no_hs_2014_18`, hs_1970 = 100 - no_hs_1970) %>%
  select(1:7, hs_2014_18, hs_1970)

ggplot(data = COVID, aes(COVID$`Cases of COVID-19`, COVID$Population, color = 
         COVID$`Deaths from COVID-19`)) + geom_point()

       