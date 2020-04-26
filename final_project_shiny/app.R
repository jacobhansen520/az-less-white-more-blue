#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(readxl)
library(readr)
library(readr)
library(janitor)
library(ggplot2)
library(ggthemes)
library(stringr)
library(gt)
library(plotly)
library(tidycensus)
census_api_key("bc6535753047a343a531d92be52cfa8b40d17182", install = TRUE, overwrite = TRUE)

arizona <- get_acs(geography = "county",
                   state = "AZ",
                   variables = "DP05_0071PE",
                   year = 2018,
                   geometry = TRUE)

data <- read_rds("data_4-24.rds")

ui <- fluidPage(
    
    titlePanel("Effect of Demographic Trends on Arizona Politics"),
    h4("From 2010 through 2018"),
    
    navbarPage("",
            tabPanel(h4("Voter Registration in Arizona by County"),
                fixedRow(column(3, verticalLayout(
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
                                                    multiple = FALSE), br(),
                                      plotOutput("reg_plot", width = "450", height = "330"), br(),
                                      plotOutput("vote_plot", width = "450", height = "330"))), br(),
                         column(5, plotlyOutput("arizona_pop_perc", height = "750")), br(),
                         column(4, plotOutput("perc_hisp_change_perc", height = "750")))), 
            
            tabPanel(h4("Demographic Changes in Arizona by County")),
            
            tabPanel(h4("Is Increased Hispanic Population Correlated with Increased Democratic Vote Share?"),
              gt_output("correlation")),
            
            tabPanel(h4("About"),
                     htmlOutput("about"))
))

server <- function(input, output) {
    
    output$reg_plot <- renderPlot({
                data %>% 
                  filter(county == input$county) %>% 
                  ggplot(aes(x = year, y = percentage_reg, color = party)) +
                  geom_line(show.legend = FALSE, size = 1.0) +
                  scale_color_manual(values = c("blue3", "red2")) +
                  labs(x = "Year",
                       y = "Registration",
                       title = "Party Registration Percentages",
                       subtitle = "Selected County in Arizona",
                       caption = "Sources: MIT Election Data + Science Lab,
                    Arizona Secretary of State,
                    U.S. Census Bureau") +
                  theme_hc() +
                  scale_x_continuous(breaks = seq(2010, 2018, by = 2))
    })
        
    output$vote_plot <- renderPlot({
                data %>% 
                  filter(county == input$county) %>%
                  ggplot(aes(x = year, y = percentage_votes, color = party)) +
                  geom_line(show.legend = FALSE, size = 1.0) +
                  scale_color_manual(values = c("blue3", "red2")) +
                  labs(x = "Year",
                       y = "Percentage of Vote",
                       title = "Vote Percentage for Top-Ballot Candidates",
                       subtitle = "Selected County in Arizona",
                       caption = "Sources: MIT Election Data + Science Lab,
                    Arizona Secretary of State,
                    U.S. Census Bureau") +
                  theme_hc() +
                  scale_x_continuous(breaks = seq(2010, 2018, by = 2))
    })
    
    output$arizona_pop_perc <- renderPlotly({
          ggplotly(ggplot(data = arizona, aes(text = paste(NAME, "<br>",
                                              "Hispanic Percentage:", estimate,"%"))) +
                           geom_sf(aes(fill = estimate)) +
                           theme_map() +
                           scale_fill_viridis_c(direction = -1, option = "plasma") +
                           labs(title = "Hispanic or Latinx Percent of Arizona Population",
                                fill = "Percentage"),
                  tooltip = c("text"))
    })
    
    output$perc_hisp_change_perc <- renderPlot({
                data %>% 
                  filter(year == 2010 | year == 2018) %>% 
                  filter(party == "democrat") %>% 
                  select(county, year, hisp_perc) %>% 
                  pivot_wider(names_from = year, values_from = hisp_perc, names_prefix = "perc_") %>% 
                  group_by(county) %>% 
                  summarize(perc_change_hisp_perc = perc_2018 - perc_2010) %>% 
                  ggplot(aes(x = reorder(county, perc_change_hisp_perc), y = perc_change_hisp_perc)) +
                  geom_col(fill = "navyblue") +
                  theme_classic() +
                  labs(x = NULL,
                       y = "Change in Hispanic Percentage of Population",
                       title = "Hispanic Populations Have Grown Faster \n Than County Populations Overall") +
                  coord_flip()
    })
    
    output$reg_changes_county <- renderPlot({
                data %>% 
                  filter(year == 2010 | year == 2018) %>% 
                  select(county, year, party, registration) %>% 
                  pivot_wider(names_from = party, values_from = registration) %>% 
                  mutate(difference = democrat - republican) %>% 
                  select(county, year, difference) %>% 
                  pivot_wider(names_from = year, values_from = difference, names_prefix = "diff_") %>% 
                  mutate(diff_change = diff_2018 - diff_2010) %>%
                  ggplot(aes(x = reorder(county, diff_change), y = diff_change, fill = diff_change < 0)) +
                  geom_col(show.legend = FALSE) +
                  theme_classic() +
                  labs(x = NULL,
                       y = "Registration Gains Relative to Other Party",
                       title = "Where Has Each Party Improved?",
                       subtitle = "Between 2010 and 2018") +
                  coord_flip() +
                  scale_fill_manual(values = c("blue3", "red2"))
    })
    
    output$correlation <- render_gt({
                perc_change_votes <- data %>% 
                  filter(year == 2010 | year == 2018) %>% 
                  filter(party == "democrat") %>% 
                  select(county, year, percentage_votes) %>% 
                  pivot_wider(names_from = "year", values_from = "percentage_votes", names_prefix = "votes_") %>% 
                  mutate(perc_change_votes = votes_2018 - votes_2010) %>% 
                  select(county, perc_change_votes)
                
                data %>% 
                  filter(year == 2010 | year == 2018) %>% 
                  filter(party == "democrat") %>% 
                  select(county, year, hisp_perc) %>% 
                  pivot_wider(names_from = "year", values_from = "hisp_perc", names_prefix = "hisp_perc_") %>% 
                  mutate(perc_change_hisp = hisp_perc_2018 - hisp_perc_2010) %>% 
                  select(county, perc_change_hisp) %>% 
                  full_join(perc_change_votes, by = "county") %>%
                  gt() %>% 
                  tab_header(title = "Effect on Change in Hispanic Population on Democratic Vote Share",
                             subtitle = "From 2010 to 2018") %>% 
                  tab_spanner(label = "Correlation = -0.058", columns = TRUE) %>% 
                  cols_align(align = "center", columns = TRUE) %>% 
                  fmt_number(columns = vars(perc_change_votes), decimals = 2) %>% 
                  cols_label(county = "County",
                             perc_change_hisp = "Change in Hispanic Share of Population (%)",
                             perc_change_votes = "Change in Democratic Vote Share (%)")
    })
    
    output$about <- renderText ({
      
      "Arizona's electoral landscape seems to be eternally shifting, with the state's prospects of 'going blue' re-emerging every election cycle.
      This leftward shift is routinely attributed to changing demographics, specifically the state's large and growing Hispanic and Latinx populations.
      In this project, I sought to determine whether there was any relationship between an increased percentage of the population being Hispanic or Latinx
      and the vote share won by Democrats in Arizona's fifteen counties. While this relationship may be strengthening as of late, I see little evidence for
      its validity over the period from 2010 through 2018, with urban/rural sorting appearing to play a larger role in electoral changes."
      
    })
}

shinyApp(ui = ui, server = server)
