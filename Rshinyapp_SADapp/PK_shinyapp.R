#libraries <- c("tidyverse","ggplot2","shiny","shinydashboard","xgxr","dplyr","DBI","tidyr","stringr","ggpubr","gridExtra","xgxr","shinydashboard","highcharter","stringr","lubridate")
#install.packages(libraries)
library(DBI)
library(dplyr)
library(lubridate)
library(stringr)
library(highcharter)
library(shiny)
library(shinydashboard) 
library(ggplot2)
library(tidyr)
library(xgxr)
library(gridExtra)


#load all datasets and plots saved from markdown file
load("PKIL5.RData")
xgx_theme_set()
###################ui functions ####################

header <- dashboardHeader(title = "PK exploratory analysis")

sidebar <- dashboardSidebar(
  sidebarMenu(
    
    menuItem("Theophylline PK data analysis", tabName = "tab_dose",
             startExpanded = FALSE
    ),
    menuItem("IL-5 analysis", tabName = "tab_second",
             startExpanded = FALSE
    )
  )
)

body <- dashboardBody(
  tabItems(
    #dashboard content
    
    ######KPI tab
    tabItem(tabName ='tab_dose',
            fluidRow(
              
              column(width = 12,
                     valueBoxOutput('header_dose_1'),  
              ),
              
              column(width = 12,
                     
                     plotOutput(outputId = "output_dose_1"),
                     
              ),
              
              column(width = 12,
                     valueBoxOutput('header_dose_2'),    
              ),
              
              column(width = 12,
                     
                     plotOutput(outputId = "output_dose_2"),
                     
              ),
              
              column(width = 12,
                     valueBoxOutput('header_dose_3'),    
              ),
              
              column(width = 12,
                     
                     plotOutput(outputId = "output_dose_3"),
                     
              ),
              ##
              column(width = 12,
                     valueBoxOutput('header_dose_4'),    
              ),
              
              column(width = 12,
                     
                     plotOutput(outputId = "output_dose_4"),
                     
              ),
              
              column(width = 12,
                     valueBoxOutput('header_dose_5'),    
              ),
              
              column(width = 12,
                     
                     plotOutput(outputId = "output_dose_5"),
                     
              ),
              
              ##
              column(width = 12,
                     valueBoxOutput('header_dose_6'),    
              ),
              
              column(width = 12,
                     
                     plotOutput(outputId = "output_dose_6"),
                     
              ),
              
              # column(width = 12,
              # 
              #        
              # )
            )
    ),
    
    tabItem(tabName ='tab_second',
            fluidRow( column(width = 12,
                             valueBoxOutput('header_dose_7'),    
            ),
            
            column(width = 12,
                   
                   plotOutput(outputId = "output_dose_7"),
                   
            )
            )
    )
    
    
    
  )
  
)

ui <- dashboardPage(header, sidebar, body)

###################server  functions ####################
server <- function(input, output, session) {
  
  ##header outputs below
  
  #output$header_dose_1 <- renderValueBox({valueBox(tags$p("Theophylline concentrations by gender", style = "font-size: 65%;"), "",  width = 8, color = "green")})
  output$header_dose_1 <- renderValueBox({
    valueBox("Effect of gender", width=5,color="navy",subtitle="")
  })
  
  output$header_dose_2 <- renderValueBox({
    valueBox("Theophylline concentrations over time", width=5,color="navy",subtitle="(Dose shown in tertiles)")
  })
  
  output$header_dose_3 <- renderValueBox({
    valueBox("Normalized Theophylline concentrations over time", width=5,color="navy",subtitle="(Dose shown in tertiles)")
  })
  #output$header_dose_4 <- renderValueBox({valueBox(tags$p("<Name Of Fourth Graph>", style = "font-size: 65%;"), "",  width = 12, color = "navy")})
  output$header_dose_5 <- renderValueBox({valueBox(tags$p("AUC analysis and estimates of variability", style = "font-size: 65%;"), "",  width = 12, color = "navy")})
  
  output$header_dose_6 <- renderValueBox({valueBox(tags$p("Correlation of AUC with Cmax and dose", style = "font-size: 65%;"), "",  width = 12, color = "navy")})
  output$header_dose_7 <- renderValueBox({valueBox(tags$p("Correlation of IL-5 inhibition with PK parameters", style = "font-size: 65%;"), "",  width = 12, color = "navy")})
  
  ##graph outputs below
  output$output_dose_1 <- renderPlot({
    
    ggcomb_1<-grid.arrange(gg1,tableGrob(By_gen_theophconc_sum),nrow=2,heights=c(1.5,.75))
    print(ggcomb_1)
  })
  
  output$output_dose_2 <- renderPlot({
    
    
    
    print(gg2)
  })
  
  output$output_dose_3 <- renderPlot({
    
    
    print(gg3)
  })
  
  #output$output_dose_4 <- renderPlot({
  #   
  #   
  #   print(gg4)
  # })
  
  output$output_dose_5 <- renderPlot({
    
    
    p=grid.arrange(gg4,gg4,ncol=2)
    print(p)
  })  
  
  
  output$output_dose_6 <- renderPlot({
    
    
    p=grid.arrange(gg6,gg7,ncol=2)
    print(p)
  }) 
  
  output$output_dose_7 <- renderPlot({
    
    
    z=grid.arrange(gg8,gg9,gg10,nrow=2)
    print(z)
  })  
  
  
}


shinyApp(ui, server)