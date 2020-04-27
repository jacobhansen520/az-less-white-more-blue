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
library(broom)
library(tidycensus)
census_api_key("bc6535753047a343a531d92be52cfa8b40d17182", install = TRUE, overwrite = TRUE)

arizona <- get_acs(geography = "county",
                   state = "AZ",
                   variables = "DP05_0071PE",
                   year = 2018,
                   geometry = TRUE)

data <- read_rds("data_4-24.rds")

az_county_results <- data %>% 
  select(county, year, party, percentage_votes) %>% 
  pivot_wider(names_from = party, values_from = percentage_votes) %>% 
  mutate(difference = democrat - republican) %>% 
  mutate(county_full = paste(county, "County, Arizona")) %>% 
  select(county_full, year, democrat, republican, difference)

arizona_county_results_map <- arizona %>% 
  full_join(az_county_results, by = c("NAME" = "county_full")) %>% 
  mutate(democrat_rounded = round(democrat, digits = 2)) %>% 
  mutate(republican_rounded = round(republican, digits = 2))

y <- data %>% 
  filter(party == "democrat") %>%
  group_by(year) %>% 
  summarize(total_hisp_pop = sum(hisp_pop), 
            total_pop = sum(total_pop),
            total_dem_votes = sum(candidatevotes),
            total_votes = sum(totalvotes)) %>% 
  mutate(hisp_perc_az = (total_hisp_pop / total_pop) * 100) %>% 
  mutate(dem_perc_az = (total_dem_votes / total_votes) * 100) %>% 
  mutate(county = "OVERALL") %>% 
  select(county, year, hisp_perc_az, dem_perc_az)

data_dems <- data %>% 
  filter(party == "democrat")

model_1 <- lm(percentage_votes ~ hisp_perc + county,
              data = data_dems)

ui <- fluidPage(
    
    titlePanel("Is Arizona More Blue Because It's Less White?"),
    h4("Demographic Effects on Election Outcomes, 2010-2018"),
    
    navbarPage("",
            tabPanel(h4("Arizona Politics"),
                 fixedRow(column(3, verticalLayout(
                   selectInput("county1",
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
                     plotOutput("vote_plot", width = "450", height = "330"))),
                   column(5, verticalLayout(
                     selectInput("year1",
                                 "Select an Election Year:",
                                 choices = c("2010",
                                             "2012",
                                             "2014",
                                             "2016",
                                             "2018"),
                                 multiple = FALSE), br(),
                     plotlyOutput("arizona_county_results_map", height = "700"))),
                   column(4, verticalLayout(
                     selectInput("year2",
                                 "Relative Registration Gains Since...",
                                 choices = c("2010",
                                             "2012",
                                             "2014",
                                             "2016"),
                                 multiple = FALSE), br(),
                      plotOutput("reg_changes_county", height = "700"))))),
            
            tabPanel(h4("Arizona Demographics"),
                fixedRow(column(3, gt_output("hisp_pop_2018")), br(),
                         column(5, plotlyOutput("arizona_pop_perc", height = "750")), br(),
                         column(4, plotOutput("perc_hisp_change_perc", height = "750")))),
            
            tabPanel(h4("Is Increased Hispanic Population Correlated with Increased Democratic Vote Share?"),
                fixedRow(column(4, gt_output("correlation_county")),
                         column(4, gt_output("model_gt")),
                         column(4, verticalLayout(
                           selectInput("county2",
                                       "In This County:",
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
                           numericInput("hisp_pop",
                                        "With This Hispanic Population:",
                                        value = "5",
                                        min = 0,
                                        max = 100,
                                        step = 0.1), br(),
                           gt_output("prediction"), br(),
                           h3("About"),br(),
                              htmlOutput("about")))))
            
))

server <- function(input, output) {
    
    output$reg_plot <- renderPlot({
                data %>% 
                  filter(county == input$county1) %>% 
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
                  filter(county == input$county1) %>%
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
                           labs(title = "Hispanic Population in Arizona by County",
                                fill = "Percentage"),
                  tooltip = c("text"))
    })
    
    output$arizona_county_results_map <- renderPlotly({
          ggplotly(ggplot(data = arizona_county_results_map %>% 
                                  filter(year == input$year1), 
                                  aes(text = paste(NAME, "<br>",
                                                   "Democratic Percentage:", democrat_rounded,"%", "<br>",
                                                   "Republican Percentage:", republican_rounded,"%"))) +
                           geom_sf(aes(fill = difference < 0), show.legend = FALSE) +
                           theme_map() +
                           scale_fill_manual(values = c("blue3", "red2")) +
                           labs(title = "Arizona Top-Ballot Election Results By County",
                                fill = "Percentage"),
                           tooltip = c("text")) %>% 
                           layout(showlegend = FALSE)
    })
    
    output$hisp_pop_2018 <- render_gt({ 
          data %>% 
            filter(year == 2018) %>% 
            filter(party == "democrat") %>% 
            select(county, hisp_perc) %>% 
            arrange(desc(hisp_perc)) %>% 
            gt() %>% 
            tab_header(title = "Hispanic Populations by County") %>% 
            tab_spanner(label = "2018 American Community Survey", columns = TRUE) %>% 
            tab_footnote(footnote = "Data from U.S. Census Bureau", cells_title()) %>% 
            cols_label(county = "County",
                       hisp_perc = "Percentage") %>% 
            cols_align(align = "center", columns = TRUE)
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
                       title = "Since 2010, Hispanic Populations Have Grown \n Faster Than County Populations Overall") +
                  coord_flip()
    })
    
    output$reg_changes_county <- renderPlot({
                data %>%
                  select(county, year, party, registration) %>% 
                  pivot_wider(names_from = party, values_from = registration) %>% 
                  mutate(difference = democrat - republican) %>% 
                  select(county, year, difference) %>% 
                  pivot_wider(names_from = year, values_from = difference, names_prefix = "diff_") %>% 
                  mutate(diff_from_2010 = diff_2018 - diff_2010,
                         diff_from_2012 = diff_2018 - diff_2012,
                         diff_from_2014 = diff_2018 - diff_2014,
                         diff_from_2016 = diff_2018 - diff_2016) %>%
                  select(county, diff_from_2010, diff_from_2012, diff_from_2014, diff_from_2016) %>% 
                  pivot_longer(diff_from_2010:diff_from_2016, names_to = "year", values_to = "difference",
                               names_prefix = "diff_from_") %>% 
                  filter(year == input$year2) %>% 
                  ggplot(aes(x = reorder(county, difference), y = difference, fill = difference < 0)) +
                  geom_col(show.legend = FALSE) +
                  theme_classic() +
                  labs(x = NULL,
                       y = NULL) +
                  coord_flip() +
                  scale_fill_manual(values = c("blue3", "red2"))
    })
    
    output$correlation_change <- render_gt({
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
    
    output$correlation_county <- render_gt({
                data %>% 
                  filter(party == "democrat") %>% 
                  full_join(y, by = c("county",
                                      "year",
                                      "hisp_perc" = "hisp_perc_az",
                                      "percentage_votes" = "dem_perc_az")) %>% 
                  group_by(county) %>% 
                  mutate(correlation = cor(hisp_perc, percentage_votes)) %>% 
                  filter(year == 2018) %>% 
                  select(county, correlation) %>% 
                  ungroup() %>%
                  arrange(desc(correlation)) %>% 
                  gt() %>% 
                  tab_header(title = "Correlation of Hispanic Population and Democratic Vote Share") %>% 
                  tab_spanner(label = "From 2010 to 2018", columns = TRUE) %>% 
                  cols_align(align = "center", columns = TRUE) %>% 
                  cols_label(county = "County",
                             correlation = "Correlation") %>% 
                  tab_footnote(footnote = "Data from U.S. Census Bureau and Arizona Secretary of State",
                               cells_title())
    })
    
    output$model_gt <- render_gt({
                model_1 %>% 
                  tidy(conf.int = TRUE) %>% 
                  select(term, estimate, conf.low, conf.high) %>% 
                  gt() %>% 
                  tab_header(title = "Efffect of Hispanic Population Change on Democratic Vote Share") %>% 
                  tab_spanner(label = "Default Estimate is Apache County", columns = TRUE) %>% 
                  cols_label(term = "Variable",
                             estimate = "Estimate",
                             conf.low = "Lower Bound",
                             conf.high = "Upper Bound") %>% 
                  tab_footnote(footnote = "Data from U.S. Census Bureau and Arizona Secretary of State",
                               cells_title())
    })
    
    output$prediction <- render_gt({
                predict(model_1, newdata = tibble(hisp_perc = input$hisp_pop, county = input$county2)) %>% 
                  gt() %>% 
                  cols_label(value = "Dem %")
    })
    
    output$about <- renderText({
      
      "<p>Arizona's electoral landscape seems to be eternally shifting, with the state's prospects of 'going blue' re-emerging every election cycle.
      This leftward shift is routinely attributed to changing demographics, specifically the state's large and growing Hispanic and Latinx populations. </br> </br>
      
      In this project, I sought to determine whether there was any relationship between an increased percentage of the population being Hispanic or Latinx
      and the vote share won by Democrats in Arizona's fifteen counties. I found that the correlation between Hispanic population and Democratic vote share
      is extremely strong in Arizona's biggest counties, and much weaker in smaller counties. This leads to a strong correlation statewide. </br> </br>
      
      Furthermore, increases in Hispanic populations in every Arizona county should be expected to lead to higher Democratic vote shares statewide in upcoming elections.</p></b> <br/>
      
      By: Jacob Hansen <br/><br/>
      
      Data: MIT Election Data + Science Lab, <br/>
      <ul>Arizona Secretary of State, </br>
      U.S. Census Bureau </ul>"
      
    })
}

shinyApp(ui = ui, server = server)
