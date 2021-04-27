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
      percentages in those cities.", strong("The higher the socio-economic 
      cluster rank, the higher the percentage of vaccinated citizens."), 
      "I modeled this relationship using a regression to predict the vaccination 
      rates for cities for which no data is available."),
      br(),
      plotOutput("econ_plot"),
     p("The Equation for the Regression Model:"),
     withMathJax('$$ Percent_/Vaccinated_i = \\beta_0 + \\beta_1Economic_/Rank +
                           \\epsilon_i $$'),
     br(),
     fluidRow(
       column(8,
              imageOutput("general_posterior", 
                          width = "600px")),
       column(4,
              p("This is a model for the percent of vaccinated in any given city,
              given its socio-economic rank.", strong("Cities with a higher
              socioeconomic rank were predicted to have higher vaccination 
              percentages."), "For example, cities with in the socio-economic 
              rank cluster 10, are predicted to have a median of around 75% 
              vaccinated. Cities with a socio economic cluster rank of 5 are 
              predicted to have aound 53% vaccinated. Cities with rank 1 will 
              only have a median of around 36% vaccinated. The dark blue part of 
              the plot represents the likely outcome as it is within the 95%
              confidence interval, while the red part is the standard error. 
              The model is useful in predicting the vaccination rates in cities of 
       different socio-economic ranks in Israel. The population is representative
       of cities in Israel for which we do not have data. While each city differs
       in certain characteristics, all cities in Israel have the same healthcare 
       options and so it is reasonable to assume that everyone had access to
       vaccines. We can learn that that there is a very clear relationship between
       the economic rank of cities and the vaccination percentages. This model can
       be used to predict vaccination percentages in cities for which we do not 
       have vaccinaiton data but know socio-economic rank. It is not representative 
       of other countries, as there is great variation and many other factors that 
       should be taken into account (e.g. healthcare system, education etc.), 
       however it is possible to learn from this trend and prepare for such
       discrepancies in other countries undergoing vaccination process currently.")),
       br(),
       h3("Discussion:"),
       p("The model is limited by a number of factors which are not included. 
       Such as the distance to a vaccination center might vary between cities.
       It is important to consider that perhaps some individuals had to
       commute further to obtain a vaccine. For example, in the Negev area (south
       of Israel) where there are many Beduin towns, there are fewer healthcare 
       centers, which could have been a factor causing smaller vaccination rates. 
       For example, Umm Batim and Hura, Bedouin towns in the Southern District 
       of Israel, have the lowest vaccination rates. However, Kfar Habad, a Jewish 
       orthodox town where there is a vaccination center also had significantly
       low vaccination rates. Perhaps another factor that needs to be considered
       is the level of education in those areas and the relationship between 
       minority population vaccination rates. Futhemore, the data used is from 
       the beginning of April, when the rate of vaccination in Israel decreased 
       substantially. It is therefore resonable to assume that by this stage 
       most Israeli's who wanted to get vaccinated 
       had done so. However, it is important to consider that perhaps people who
       live in areas with a low socio-economic rank got vaccinated at a later 
       stage, which is perhaps a limitation of the model as the vaccination 
       process is still occuring. Nonetheless, since the rate of vaccination
       has slowed substantially, this is a limitation but does not invalide the 
       model."),
       h3("Conclusions:"),
       p("It is clear that there is a relationship between the socio-economic
       rank of cities and the vaccination percentage. However, given the fact that
       vaccinations were free and available to all, socio-economic standard may 
       exhibit a situation of a third variable problem. Perhaps the common 
       correlation between the socio-economic standard and vaccination percentages 
       of cities can be explained by factors such as education, distance to 
       vaccination center or by the type of population, e.g. minority groups. 
       These factors deserve further study. Nonetheless it is clear that countries
       seeking to have maximal population vaccination rapidly need to consider 
       other factors apart from obtaining vaccines. Clearly, there are differences
       between vaccination percentages and socio-economic rank is a good measure
       to predict where fewer people will get vaccinated. Though, there can be a 
       large number of causes that lead cities with a lower economic standard to 
       have a smaller percentage of vaccination. To try and eliminate these 
       problems countries should consider making the vaccines as cheap as possible
       so that everyone can afford them, educating the population on the safety
       of these vaccines, ensuring minimcal beurocratic procedures and proximity
        of vaccination centers.")
       
     )
     

      ),
        
 
 tabPanel("About", 
          # titlePanel("About"),
          h3("Background"),
          h5("This project is about understanding how socio-economic disparities
          influence vaccination rates.  Given the global widescale impact of 
          Covid-19, learning from different countries is essential to rapidly 
          increase the vaccination rates. I was particularly interested in this
          topic because I've seen the negative impact of covid-19 on my community
          and followed the news closely to learn about how different countries 
          overcame the pandemic. Living in Israel allowed me to observe the
          vaccination of the population and to experience first hand the 
          normalization of life post pandemic. However, I was surprised to hear
          that there was vast differences in the vaccination rates of cities, 
          while availability was the same in all areas in Israel. There were
          people who despite the great suffering of the pandemic were choosing not
          to get vaccinated, particularly in areas where the socio-economic rank
          was relatively lower. This sparked my interest for the subject and 
          given my general interest in economics I decided I want to investigate 
          this relationship using data-science."),
          h3("Data"),
          h5("Data was provided by the Israeli Health Ministry. The data 
          included a number of different datasets: general Israeli vaccinaiton 
          rates, city socio-economic standard data, Israeli vaccination by day 
          data and a dataset with coordinates of different Israeli cities for
          the maps."),
          h3("Acknowledgements"),
          h5("Thank you to Preceptor Kane, Jessica, Shaked and the rest of the 
          GOV 1005 course for their assistance, paitence, and enthusiasm."),
          h3("Author"),
          h5("Shai-Li Ron is a student at Harvard College concetrating in 
          economics. She has been living in Israel during the pandemic and saw
          this project as an opportunity to analyse data in Hebrew on the 
          vaccination process in Israel to allow other countries to learn from
          the challanges Israel faced in fighting the pandemic with vaccines.
                Email: shailiron@college.harvard.edu"),
          h5("Github Repo:"), 
          tags$a(href="https://github.com/Shai-LiRon/final_gov_project",
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







