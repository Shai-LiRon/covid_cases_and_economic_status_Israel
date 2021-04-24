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
 
 tabPanel("Model",
          fluidPage(
              titlePanel("Model Title"),
              sidebarLayout(
                  sidebarPanel(
                      selectInput(
                          "plot_type",
                          "Plot Type",
                          c("0-19 First Dose" = "a", "Total First Dose" = "b")
                      )),
                  mainPanel(imageOutput("map")))
          ))
 
 ),

# ____________________________________________________________________________
                
tabPanel(
    "Covid Cases",

        plotOutput("new_case_plot")
    )
)
server <- function(input, output) {
        output$new_case_plot <- renderPlot({
            # Generate type based on input$plot_type from ui

  
                total_case_vax %>% 
                    ggplot(aes(x = date,
                               y = new_cases)) + 
                    geom_line(size=0.8, color=rgb(0.1, 0.6, 0.9, 1)) +
                    labs(title = "Daily Number of New Covid Cases in Israel",
                         subtitle = "Number of covid cases decreased when 50% of population was vaccinated",
                         x = "Date",
                         y = "Daily Number of New Covid Cases",
                         caption = "Source: Israel Ministry of Health") +
                    theme_ipsum() +
                    geom_label(x=as.Date("2020-12-19"), y = 5000, 
                               label = "Vaccination Starts", color = "black", size = 2) +
                    geom_label(x=as.Date("2021-03-19"), y = -100, 
                               label = "50% Vaccinated", color = "black", size = 2) +
                    geom_label(x=as.Date("2021-02-09"), y = 6000, 
                               label = "25% Vaccinated", color = "black", size = 2)
                
                 

 
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
