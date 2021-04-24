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
library(dplyr)
library(janitor)
library(primer.data)
library(rstanarm)
library(readxl)
library(lubridate)
library(ggthemes)
library(patchwork)
library(ggplot2)
library(hrbrthemes)
library(ggdist)
library(shinythemes)
library(tidycensus)

# total case vax data
total_case_vax <- read_csv("final_data/total_case_vax.csv")
perc_vax_city <- read_csv("final_data/perc_vax_city.csv")

ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage(
                    "Covid Vaccines and Economic Status",
                    
 # ____________________________________________________________________________
                    
    tabPanel(
     "Introduction",
                        
    mainPanel(
    h3("Learning from Covid Vaccinations in Israel"),
                            
    h5("Vaccines are the long-term solution to the Covid-19 pandemic, which has completely transformed \
      the world for over a year and has led to significant suffering. As vaccines are rapidly \
      becoming available globally, we can learn from the process of vaccination in Israel. \
      Israel was the first country to offer vaccines to everyone and has been leading the \
      world, with the highest percentage of inoculated citizens. The Israeli centralized \
      and socialized heatlhcare system compels every citizen to join one of a small \
      number of competing HMOs. Vaccines are thus free and offered to every Israeli citizen. \
      Therefore, it seems reasonable that socio-economic status would not affect vaccination \
      rates. However, this project reveals that the data tells a different story.\
      Cities and towns with a lower socio-economic status had a significantly lower \
      percentage of vaccinated citizens."),
    h5("I created a model to predict the percentage \
      of vaccinated in cities of different socio-economic status. The primary factors\
      taken into account was the socio-economic status of cities and towns in Israel and \
      the vaccination percentages. Countries have varying \
      healthcare systems and vaccination availabilities, however the model is useful for \
      policy makers and healthcare institions to be well informed and prepared for their country's \
      vaccination process. As you gear up with vaccines, I hope you find this page useful \
      to devise plans that ensure rapid widespread vaccination."),
     h5("To ensure widespread Covid-19 vaccination, we need to pay close attention socio-economic disparities.")
                            
     )),
 
 tabPanel("Covid Cases",
          fluidPage(
              titlePanel("Covid Cases and Vaccination"),
              sidebarLayout(
                  sidebarPanel(
                      selectInput(
                          "plot_type",
                          "Plot Type",
                          c("New cases" = "new_cases", 
                            "Vaccination" = "vaccine_plot",
                            "Relationship" = "relation_plot")
                      )),
                  mainPanel(plotOutput("new_case_plot")))
          ))
 
 ),

# ____________________________________________________________________________
                
tabPanel(
    "Covid Cases",

        
    ))

server <- function(input, output) {
    total_case_vax <- read_csv("final_data/total_case_vax.csv") 
    perc_vax_city <- read_csv("final_data/perc_vax_city.csv")
        output$new_case_plot <- renderPlot({
            # Generate type based on input$plot_type from ui
            if (input$plot_type == "new_cases") {
                total_case_vax <- read_csv("final_data/total_case_vax.csv")
                total_case_vax %>% 
                    ggplot(aes(x = date,
                               y = new_cases)) + 
                    geom_line(size=0.8, color=rgb(0.1, 0.6, 0.9, 1)) +
                    labs(title = "Daily Number of New Covid Cases in Israel",
                         subtitle = "Number of covid cases decreased when 50% of population was vaccinated",
                         x = "Date",
                         y = "Daily Number of New Covid Cases",
                         caption = "Source: Israel Ministry of Health") +
                    geom_label(x=as.Date("2020-12-19"), y = 5000, 
                               label = "Vaccination Starts", color = "black", size = 2) +
                    geom_label(x=as.Date("2021-03-19"), y = -100, 
                               label = "50% Vaccinated", color = "black", size = 2) +
                    geom_label(x=as.Date("2021-02-09"), y = 6000, 
                               label = "25% Vaccinated", color = "black", size = 2)
                
            }
            else if (input$plot_type == "vaccine_plot") {
                total_case_vax <- read_csv("final_data/total_case_vax.csv")
                total_case_vax %>% 
                    filter(date > as.Date("2020-12-01")) %>% 
                    pivot_longer(cols = c(first_dose, second_dose), 
                                 names_to = "type", values_to = "value") %>% 
                    ggplot(aes(x = date,
                               y = value,
                               color = type)) + 
                    geom_line(size=0.8) +
                    scale_y_continuous(labels = scales::number_format(accuracy = 100)) +
                    labs(title = "Number of Israelis Vaccinated: First and Second Dose",
                         subtitle = "Number of daily vaccinations increased less rapidly March",
                         x = "Date",
                         y = "Number of Vaccinatated",
                         caption = "Source: Israel Ministry of Health") +
                    geom_label(x=as.Date("2020-12-19"), y = -100, 
                               label = "First Vaccines", color = "black", size = 2) +
                    geom_label(x=as.Date("2021-03-19"), y = 4526000, 
                               label = "50% of Population Vaccinated", color = "black", size = 2) +
                    scale_color_discrete(name = "Vaccine Dose", labels = c("First Dose", "Second Dose")) +
                    theme_ipsum()

            }
            else if (input$plot_type == "relation_plot") {
                perc_vax_city <- read_csv("final_data/perc_vax_city.csv")
                perc_vax_city %>% 
                    mutate_at(vars(-c("city")), as.numeric) %>% 
                    arrange(perc_first_dose) %>% 
                    select(city, perc_second_dose, active_per_10000) %>% 
                    ggplot(aes(x = perc_second_dose, 
                               y = active_per_10000)) +
                    geom_point() +
                    geom_smooth(method = "lm", 
                                formula = y ~ x) +
                    labs(title = "Active Cases per 10,000 People and Vaccination Percentages",
                         subtitle = "A negative relationship exists between active cases and vaccination percentage",
                         x = "Percent Vaccinated Second Dose in Given City",
                         y = "Number of Active Cases per 10,000 People in Given City",
                         caption = "Source: Israel Health Ministry") +
                    theme_ipsum()
                
            }
            
 
 
   })
}


shinyApp(ui, server)















# Draw the histogram with the specified number of bins
#         
#         hist(x, col = 'darkgray', border = 'white')
#     })
# }



# ui <- navbarPage(
#     "Final Project Title",
#     tabPanel("Model",
#              fluidPage(
#                  titlePanel("Model Title"),
#                  sidebarLayout(
#                      sidebarPanel(
#                          selectInput(
#                              "plot_type",
#                              "Plot Type",
#                              c("Option A" = "a", "Option B" = "b")
#                          )),
#                      mainPanel(plotOutput("line_plot")))
#              )),
#     tabPanel("Discussion",
#              titlePanel("Discussion Title"),
#              p("Tour of the modeling choices you made and 
#               an explanation of why you made them")),
#     tabPanel("About", 
#              titlePanel("About"),
#              h3("Project Background and Motivations"),
#              p("Hello, this is where I talk about my project."),
#              h3("About Me"),
#              p("My name is ______ and I study ______. 
#              You can reach me at ______@college.harvard.edu.")))
# 
# server <- function(input, output) {
#     output$line_plot <- renderPlot({
#         # Generate type based on input$plot_type from ui
#         
#         ifelse(
#             input$plot_type == "a",
#             
#             # If input$plot_type is "a", plot histogram of "waiting" column 
#             # from the faithful dataframe
#             
#             x   <- faithful[, 2],
#             
#             # If input$plot_type is "b", plot histogram of "eruptions" column
#             # from the faithful dataframe
#             
#             x   <- faithful[, 1]
#         )
#         
#         # Draw the histogram with the specified number of bins
#         
#         hist(x, col = 'darkgray', border = 'white')
