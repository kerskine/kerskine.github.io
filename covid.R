library(tidyverse)
library(lubridate)
library(readxl)

# Download xlsx files from Mass Dept of Public Health

download.file("https://www.mass.gov/doc/covid-19-raw-data-february-4-2021/download", "./data.xlsx")


# Read and clean cases

cases <- read_excel("./data.xlsx", sheet = "Cases (Report Date)") %>% 
                select(1:3) 
        names(cases) <- c("date", "pos_total", "pos_new")


# Read and clean hospital

hospital <- read_excel("./data.xlsx", sheet = "Hospitalization from Hospitals") %>% 
                select(1:3, 5, 6)
                names(hospital) <- c("date", "hos_total", "hos_new", "icu_total", "icu_new")
        
# Read and clean deaths

deaths <- read_excel("./data.xlsx", sheet = "DeathsReported (Report Date)") %>% 
                select(1:3)
                names(deaths) <- c("date", "dea_total", "dea_new")
        


# join tibbles 

model_data <- left_join(cases, hospital)
model_data <- left_join(model_data, deaths)

# mutate date field of model_data in order to control x-axis ticks in graphs

model_data <- model_data %>% mutate(date = as.Date(date))

# Create the "as of" date for the graph titles. This will be the last date in model_data

asof_date <- model_data[[nrow(model_data), 1]]

# Daily case, hospital and icu graph

model_data %>% select(date, pos_new, hos_total, icu_total) %>% 
                pivot_longer(c("pos_new", "hos_total", "icu_total"), names_to = "type", 
                                values_to = "cases") %>% 
                ggplot(aes(x = date, y = cases, color = type)) +
                geom_line(alpha = 0.4) + geom_smooth(method = "gam",se = FALSE) + 
                scale_x_date(breaks = "1 month", date_labels = "%b%y") +
                labs(x = "Date", y = "Cases", 
                     title = "Comparison of New Daily Covid-19 Cases with Hospital and ICU Census",
                     subtitle = paste("Source: Mass Dept of Public Health", asof_date), 
                     caption = "Keith Erskine") + 
                scale_color_discrete(name = "Type of Case", 
                                    labels = c("Hospital Census", "ICU Census", "New Positive Cases")) +
                ggsave("pos_hosp_icu.png",
                       device = "png",
                       path = "./images",
                       dpi = "screen")

# Daily case and deaths graph

model_data %>% select(date, pos_new, dea_new) %>% 
        pivot_longer(c("pos_new", "dea_new"), names_to = "type", 
                     values_to = "cases") %>% 
        ggplot(aes(x = date, y = cases, color = type)) +
        geom_point(alpha = 0.3) + geom_smooth(method = "gam", se = FALSE) +  
        scale_x_date(breaks = "1 month", date_labels = "%b%y") +
        labs(x = "Date", y = "Cases", 
             title = "New Daily Covid-19 Cases and Reported Deaths",
             subtitle = paste("Source: Mass Dept of Public Health", asof_date), 
             caption = "Keith Erskine") + 
        scale_color_discrete(name = "Type of Case", 
                             labels = c("New Reported Deaths", "New Positive Cases")) + 
        ggsave("pos_dea.png",
               device = "png",
               path = "./images",
               dpi = "screen")

# Alt graph - Hospital, ICU, and deaths

model_data %>% select(date, hos_total, icu_total, dea_new) %>% 
        pivot_longer(c("hos_total", "icu_total", "dea_new"), names_to = "type", 
                     values_to = "cases") %>% 
        ggplot(aes(x = date, y = cases, color = type)) +
        geom_line(alpha = 0.4) + geom_smooth(method = "gam", se = FALSE) + 
        scale_x_date(breaks = "1 month", date_labels = "%b%y") + 
        labs(x = "Date", y = "Cases", 
             title = "Comparison of Hospital and ICU Census with Daily Reported Deaths",
             subtitle = paste("Source: Mass Dept of Public Health", asof_date), 
             caption = "Keith Erskine") + 
        scale_color_discrete(name = "Type of Case", labels = c("New Reported Deaths", 
                                                               "Hospital Census",
                                                               "ICU Census")) + 
        ggsave("hos_icu_dea.png",
               device = "png",
               path = "./images",
               dpi = "screen")


# Yet another

model_data %>% select(date, hos_new, icu_new) %>% 
        filter(date >= as_date("2020-09-01")) %>% 
        pivot_longer(c("hos_new", "icu_new"), names_to = "type", 
                     values_to = "cases") %>% 
        ggplot(aes(x = date, y = cases, color = type)) +
        geom_line(alpha = 0.4) + geom_smooth(method = "gam", se = FALSE) + 
        scale_x_date(breaks = "1 month", date_labels = "%b%y") + 
        labs(x = "Date", y = "Change in Number of Cases", 
             title = "Net Change in Hospital and ICU Census Covid-19 Cases \nfrom Sept. 01, 2020",
             subtitle = paste("Source: Mass Dept of Public Health", asof_date), 
             caption = "Keith Erskine") + 
        scale_color_discrete(name = "Type of Case", labels = c("Change in Hospital Census",
                                                               "Change in ICU Census")) +
        ggsave("hosp_icu_since01sep.png",
               device = "png",
               path = "./images",
               dpi = "screen")

# Deaths

model_data %>% ggplot(aes(x = date, y = dea_new)) + 
        geom_point(color = "red", alpha = 0.3) + geom_smooth(method = "gam", se = FALSE, color = "red") + 
        scale_x_date(breaks = "1 month", date_labels = "%b%y") + 
        labs(x = "Date", y = "Deaths", 
             title = "Deaths Due to Covid-19",
             subtitle = paste("Source: Mass Dept of Public Health", asof_date), 
             caption = "Keith Erskine") +
        ggsave("dea.png",
                device = "png",
                path = "./images",
                dpi = "screen")
