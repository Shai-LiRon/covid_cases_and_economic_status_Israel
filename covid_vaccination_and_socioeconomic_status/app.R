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
library(janitor)
library(primer.data)
library(readxl)
library(lubridate)
library(ggthemes)
library(patchwork)
library(ggplot2)
library(hrbrthemes)
library(ggdist)
library(shinythemes)
library(tidycensus)
library(maps)
library(geojsonio)
library(ggplot2)
library(mapproj)
library(leaflet)
library(RColorBrewer)
source("script_final.R")
source("r_code.R")

# total case vax data
# total_case_vax <- read_csv("final_data/total_case_vax.csv")
# perc_vax_city <- read_csv("final_data/perc_vax_city.csv")
# perc_vax_city <- read_csv("final_data/perc_vax_city.csv")  
# total_case_vax <- read_csv("final_data/total_case_vax.csv") 

ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage(
                    "Covid Vaccines and Economic Status",
                    
 # ____________________________________________________________________________
                    
    tabPanel(
     "Introduction",
                        
    
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
     h5("To ensure widespread Covid-19 vaccination, we need to pay close attention socio-economic disparities."),
    br(),
    h3("Vaccination Percentages in Different Cities"),
    leafletOutput("vax_map"),
    br(),
    h3("Active Cases in Different Cities"),
    leafletOutput("active_map")
    
    
     ),
 
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
          )),
 
 tabPanel("Economic Model",
     titlePanel("Socio-Economic Status and Vaccination"),
     h3("How does socio-economic rank affect city vaccination rate?"),
      p("This page explores the connection between socio-economic 
        rank of different cities in Israel and the percent of vaccinated citizens. 
        The socio-economic rank is measured on a scale of 1 to 10 
        (the highest rank, richest cities, are 10).
        This achieved through the allocation of local authorities into 10 
        homogeneous groups which are not equally sized. This is done by means 
        of cluster analysis, so that the variance within clusters
        is minimized and the variance between clusters is maximized."),
      p("The first graph displays the actual data, showing a clear relationship 
      between the socio-economic ranks of different cities and the vaccination 
      percentages in those cities.", strong("The higher the socio-economic cluster rank, 
      the higher the percentage of vaccinated citizens."), "I modeled this
      relationship using a regression to predict the vaccination rates for cities
      for which no data is available."),
      br(),
      plotOutput("econ_plot"),
     p("The Equation for the Regression Model:"),
     withMathJax('$$ Percent_/Vaccinated_i = \\beta_0 + \\beta_1Economic_Rank +
                           \\epsilon_i $$'),
     br(),
     imageOutput("general_posterior", width="600px")

      ),
        
 
 tabPanel("About", 
          titlePanel("About"),
          h3("Background"),
          h5("This project is about understanding how socio-economic disparities influence vaccination rates. \
             Given the global widescaled impact of Covid-19, learning from different countries \
             is essential to rapidly increase the vaccination rates. "),
          h3("Data"),
          h5("Data was provided by the Israeli Health Ministry. The data used was a number of diverse \
          datasets, including vaccinaiton rate, socio-economic standard data and data on vaccination by day."),
          h3("Acknowledgements"),
          h5("Thank you to Preceptor Kane, Jessica, and the rest of the GOV 1005 course staff for their assistance,\
              paitence, and enthusiasm."),
          h3("Author"),
          h5("Shai-Li Ron is a student at Harvard College concetrating in economics. \
            She has been living in Israel during the pandemic and she sees the data from the \
            vaccination process in Israel as an opportunity to allow other countries to learn.
                Email: shailiron@college.harvard.edu"),
          h5("Github Repo:"), tags$a(href="https://github.com/Shai-LiRon/final_gov_project",
                                     "https://github.com/Shai-LiRon/final_gov_project")
 )
 
 
 ),

# ____________________________________________________________________________
                
tabPanel(
    "Covid Cases",

        
    ),



)

server <- function(input, output) {

   
 
    output$vax_map <- renderLeaflet(
      {vax_map}
    )
    
    
    output$active_map <- renderLeaflet(
      {active_map}
    )
    
    output$econ_plot <- renderPlot(
      {econ_vax_rate_plot}
    )
    
    output$general_posterior <- renderImage({
          list(
             src = "econ_posterior.png",
             width = 500,
             height = 500,
             alt = "econ_posterior")
    })
      
    output$new_case_plot <- renderPlot({
            # Generate type based on input$plot_type from ui
            if (input$plot_type == "new_cases") {
              {new_cases}
            }
            else if (input$plot_type == "vaccine_plot") {
                
              {vaccine_plot}
              
            }
            else if (input$plot_type == "relation_plot") {
              {relationship_plot}
            }
            
 
 
   })
        

        
  
}


shinyApp(ui, server)








# tabPanel("Economic Model",
#          fluidPage(
#            titlePanel("Socio-Economic Status and Vaccination"),
#            sidebarLayout(
#              sidebarPanel(
#                selectInput(
#                  "real_predict",
#                  "Select Data or Prediction",
#                  c("Actual Data" = "econ_plot", "Prediction" = "econ_model")
#                )),
#              mainPanel(imageOutput("economic"))),
#            br(),


# output$economic <- renderImage({
#   if(input$real_predict == "econ_plot"){            
#     list(
#       src = "econ_plot.png",
#       width = 500,
#       height = 500,
#       alt = "vaccination")
#   }                                        
#   else if(input$real_predict == "econ_model"){
#     list(
#       src = "econ_posterior.png",
#       width = 500,
#       height = 500,
#       alt = "Posterior")
#   }
# })