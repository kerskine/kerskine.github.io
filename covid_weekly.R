# Weekly Covid-19 Data based on Age

library(tidyverse) # because, tidy
library(lubridate) # helpful in manipulating date and time data
library(readxl) # read excel files

# The weekly Covid-19 data from MassDPH is now included in the daily spreadsheet

# Load the tibble (dataframe) and add a variable for over or under age 60. Covid-19 is more serious
# for people over 60

agebreak <- read_excel("./data.xlsx", sheet = "AgeLast2Weeks") %>% 
                filter(Age != "Unknown") %>% 
                mutate(Risk = case_when(
                        Age == "0-19" ~ "Age < 60", 
                        Age == "20-29" ~ "Age < 60", 
                        Age == "30-39" ~ "Age < 60", 
                        Age == "40-49" ~ "Age < 60", 
                        Age == "50-59" ~ "Age < 60", 
                        Age == "60-69" ~ "Age > 60",
                        Age == "70-79" ~ "Age > 60",
                        Age == "80+" ~ "Age > 60"))

# Create a variable used for graph titles 

age_asof_date <- agebreak[[nrow(agebreak), 1]]

# Graphs: the data is based on two week previous data

# Infections by Age/Risk Graphs

# Graph 1: Infections by Age Group

agebreak %>% ggplot(aes(x = Date, y = Cases_Last2Weeks, color = Age)) + geom_line() + 
                geom_point() + 
                 labs(x = "Date", y = "Positive Cases Two Weeks Prior", 
                        title = "Positive Covid-19 Cases by Age Group",
                        subtitle = paste("Source: Mass Dept of Public Health", age_asof_date), 
                        caption = "Keith Erskine") +
                ggsave("week_infec_age.png",
                        device = "png",
                        path = "./images",
                        dpi = "screen")

# Graph 2: Infections by Risk Group

agebreak %>% group_by(Risk, Date) %>% summarise(blah = sum(Cases_Last2Weeks)) %>% 
                ggplot(aes(x = Date, y = blah, color = Risk)) + geom_line() + 
                        geom_point() + 
                        labs(x = "Date", y = "Positive Cases Two Weeks Prior", 
                        title = "Positive Covid-19 Cases by Mortality Risk",
                        subtitle = paste("Source: Mass Dept of Public Health", age_asof_date), 
                        caption = "Keith Erskine") +
                ggsave("week_infec_risk.png",
                        device = "png",
                        path = "./images",
                        dpi = "screen")

# Hospitalizations by Age/Risk Graphs

# Graph 3: Hospitalizations by Age Group

agebreak %>% ggplot(aes(x = Date, y = Hospitalized_Last2Weeks, color = Age)) + geom_line() + 
        geom_point() + 
        labs(x = "Date", y = "Hospitalizations Two Weeks Prior", 
             title = "Hospitalizations from Covid-19 Cases by Age Group",
             subtitle = paste("Source: Mass Dept of Public Health", age_asof_date), 
             caption = "Keith Erskine") +
        ggsave("week_hospital_age.png",
               device = "png",
               path = "./images",
               dpi = "screen")


# Graph 4: Hospitalizations by Risk Group

agebreak %>% group_by(Risk, Date) %>% summarise(htrend = sum(Hospitalized_Last2Weeks)) %>% 
        ggplot(aes(x = Date, y = htrend, color = Risk)) + geom_line() + 
        geom_point() + 
        labs(x = "Date", y = "Hospitalizations Two Weeks Prior", 
             title = "Hospitalizations from Covid-19 by Mortality Risk",
             subtitle = paste("Source: Mass Dept of Public Health", age_asof_date), 
             caption = "Keith Erskine") +
        ggsave("week_hospital_risk.png",
               device = "png",
               path = "./images",
               dpi = "screen")


# Deaths by Age/Risk Groups

# Graph 5: Deaths by age group

agebreak %>% ggplot(aes(x = Date, y = Deaths_Last2Weeks, color = Age)) + geom_line() + 
        geom_point() + 
        labs(x = "Date", y = "Deaths Two Weeks Prior", 
             title = "Deaths from Covid-19 Cases by Age Group",
             subtitle = paste("Source: Mass Dept of Public Health", age_asof_date), 
             caption = "Keith Erskine") +
        ggsave("week_death_age.png",
                device = "png",
                path = "./images",
                dpi = "screen")
        
# Graph 6: Deaths by Risk Group

agebreak %>% group_by(Risk, Date) %>% summarise(dtrend = sum(Deaths_Last2Weeks)) %>% 
                ggplot(aes(x = Date, y = dtrend, color = Risk)) + geom_line() + 
                geom_point() + 
                labs(x = "Date", y = "Deaths Two Weeks Prior", 
                        title = "Deaths from Covid-19 by Mortality Risk",
                        subtitle = paste("Source: Mass Dept of Public Health", age_asof_date), 
                        caption = "Keith Erskine") +
                ggsave("week_death_risk.png",
                        device = "png",
                        path = "./images",
                        dpi = "screen")



