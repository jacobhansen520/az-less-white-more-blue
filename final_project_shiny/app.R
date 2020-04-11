#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(readxl)
library(readr)
library(janitor)

reg_2017 <- read.csv("2017-01-01.csv", skip = 4) %>% 
    clean_names() %>% 
    filter(date_period == "43847") %>% 
    head(15) %>% 
    mutate(year = 2017) %>% 
    select(county, year, democratic, republican)

reg_2018 <- read.csv("2018-01-01.csv", skip = 4) %>% 
    clean_names() %>% 
    filter(date_period == "43848") %>% 
    head(15) %>% 
    mutate(year = 2018) %>% 
    select(county, year, democratic, republican)

reg_2019 <- read.csv("2019-01-01.csv", skip = 5) %>% 
    clean_names() %>% 
    filter(date_period == "43849") %>% 
    head(15) %>% 
    mutate(year = 2019) %>% 
    select(county, year, democratic, republican)

reg_2020 <- read.csv("2020-01-21.csv", skip = 5) %>% 
    clean_names() %>% 
    filter(date_period == "43850") %>% 
    head(15) %>% 
    mutate(year = 2020) %>% 
    select(county, year, democratic, republican)

reg_2017_to_2020 <- reg_2017 %>% 
    full_join(reg_2018) %>% 
    full_join(reg_2019) %>% 
    full_join(reg_2020) %>% 
    arrange(county) %>% 
    pivot_longer(names_to = "party",
                 values_to = "registration",
                 cols = c(democratic:republican))

reg_plot <- function(x){
    reg_2017_to_2020 %>% 
        filter(county == x) %>% 
        ggplot(aes(x = year, y = registration, color = party)) +
        geom_line(show.legend = FALSE, size = 0.54) +
        scale_color_manual(values = c("blue", "red")) +
        labs(x = "Year",
             y = "Registration",
             title = "Party Registration Figures",
             subtitle = "For Selected Arizona County",
             caption = "Source: Arizona Secretary of State") +
        theme_fivethirtyeight()
}

ui <- fluidPage(
    
    titlePanel("Arizona Voter Registration Figures"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("county",
                        "Select a County:",
                        choices = c("Apache",
                                    "Cochise",
                                    "Coconino",
                                    "Gila",
                                    "Graham",
                                    "Greenlee",
                                    "Maricopa",
                                    "Mohave",
                                    "Navajo",
                                    "Pima",
                                    "Pinal",
                                    "Santa Cruz",
                                    "Yavapai",
                                    "Yuma"),
                        selected = "Apache")
        ),
        
        mainPanel(
            plotOutput("selected_county")
        )
    )
)

server <- function(input, output) {
    
    output$selected_county <- renderPlot({
        
        reg_plot(input$county)
        
    })
}

shinyApp(ui = ui, server = server)
