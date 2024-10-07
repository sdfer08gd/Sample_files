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


options(scipen = 999)

###################ingest data####################
SAD <-readr::read_csv ('Single_Ascending_Dose_Dataset2.csv')

pkpd_data <-SAD

###################clean data ####################

#ensure dataset has all the necessary columns
pkpd_data = pkpd_data %>%
  mutate(ID      = ID,     #ID   column
         TIME    = TIME,   #TIME column name, time relative to first dose 
         NOMTIME = NOMTIME,#NOMINAL TIME column name
         EVID    = EVID   ,#EVENT ID, >=1 is dose, otherwise measurement
         LIDV    = LIDV,   #DEPENDENT VARIABLE column name
         CENS    = CENS,   #CENSORING column name
         CMT     = CMT,    #COMPARTMENT column
         DOSE    = DOSE,   #DOSE column here (numeric value)
         TRTACT  = TRTACT, #DOSE REGIMEN column here (character, with units),
         LIDV_NORM = LIDV/DOSE,
         LIDV_UNIT    = ifelse(CMT==2, "ng/ml", NA )
  )

#create a factor for the treatment variable for plotting
pkpd_data = pkpd_data %>%
  arrange(DOSE) %>%
  mutate(TRTACT_low2high = factor(TRTACT, levels = unique(TRTACT)),
         TRTACT_high2low = factor(TRTACT, levels = rev(unique(TRTACT)))) %>%
  select(-TRTACT)

#create pk dataset
pk_data <- pkpd_data %>%
  filter(CMT==2)

#perform NCA, for additional plots
NCA = pk_data %>%
  group_by(ID, DOSE) %>%
  filter(!is.na(LIDV)) %>%
  summarize(AUC_last = caTools::trapz(TIME,LIDV),
            Cmax     = max(LIDV),
            SEX      = SEX[1], #this part just keeps the SEX and WEIGHTB covariates
            WEIGHTB  = WEIGHTB[1]) %>%
  gather(PARAM, VALUE,-c(ID, DOSE, SEX, WEIGHTB)) %>%
  ungroup() %>%
  mutate(VALUE_NORM = VALUE/DOSE)

#units and labels
time_units_dataset = "hours"
time_units_plot    = "days"
trtact_label       = "Dose"
dose_label         = "Dose (mg)"
conc_units         = "ng/ml"
AUC_units          = paste0("h.", conc_units)
conc_label         = paste0("Concentration (", conc_units, ")") 
concnorm_label     = paste0("Normalized Concentration (", conc_units, ")/mg")


###################ui functions ####################

header <- dashboardHeader(title = "SAD")

sidebar <- dashboardSidebar(
  sidebarMenu(
    
    menuItem("Dose", tabName = "tab_dose",
             startExpanded = FALSE
    ),
    menuItem("Second Tab", tabName = "tab_second",
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
              
              
              column(width = 12,
                     valueBoxOutput('header_dose_4'),    
              ),
              
              column(width = 12,

                     plotOutput(outputId = "output_dose_4"),
                     
              ),

              # column(width = 12,
              # 
              #        
              # )
            )
    ),
    
    tabItem(tabName ='tab_second',
            fluidRow(
            )
    )
    
    
    
  )
  
)

ui <- dashboardPage(header, sidebar, body)

###################server  functions ####################
server <- function(input, output, session) {
  
  ##header outputs below
  
  output$header_dose_1 <- renderValueBox({valueBox(tags$p("<Name Of First Graph>", style = "font-size: 65%;"), "",  width = 12, color = "navy")})
  
  output$header_dose_2 <- renderValueBox({valueBox(tags$p("<Name Of Second Graph>", style = "font-size: 65%;"), "",  width = 12, color = "navy")})
  
  output$header_dose_3 <- renderValueBox({valueBox(tags$p("<Name Of Third Graph>", style = "font-size: 65%;"), "",  width = 12, color = "navy")})
  
  output$header_dose_4 <- renderValueBox({valueBox(tags$p("<Name Of Fourth Graph>", style = "font-size: 65%;"), "",  width = 12, color = "navy")})
  
  
  ##graph outputs below
  output$output_dose_1 <- renderPlot({
    
    gg <- ggplot(data = pk_data, aes(x = NOMTIME, y = LIDV, group= TRTACT_high2low, color = TRTACT_high2low)) 
    gg <- gg + xgx_stat_ci(conf_level = .95)
    gg <- gg + xgx_scale_y_log10() 
    gg <- gg + xgx_scale_x_time_units(units_dataset = time_units_dataset, 
                                      units_plot    = time_units_plot)
    gg <- gg + labs(y=conc_label,color = trtact_label)
    
    print(gg)
  })
  
  output$output_dose_2 <- renderPlot({
    
    gg <- ggplot(data = pk_data, aes(x = TIME, y = LIDV))
    gg <- gg + geom_line(aes(group = ID), color = rgb(0.5,0.5,0.5), size = 1, alpha = 0.3)  
    gg <- gg + geom_point(aes(color = factor(CENS), shape = factor(CENS), alpha = 0.3), size = 2, alpha = 0.3)
    gg <- gg + scale_shape_manual(values=c(1,8))
    gg <- gg + scale_color_manual(values=c("grey50","red"))
    gg <- gg + xgx_stat_ci(aes(x = NOMTIME, color=NULL, group=NULL), conf_level = 0.95)
    gg <- gg + xgx_scale_y_log10()
    gg <- gg + xgx_scale_x_time_units(units_dataset = time_units_dataset, 
                                      units_plot    = time_units_plot)
    gg <- gg + labs(y=conc_label,color = trtact_label)
    gg <- gg + theme(legend.position="none") + facet_grid(.~TRTACT_low2high)
    
    print(gg)
  })
  
  output$output_dose_3 <- renderPlot({
    
    gg <- ggplot(data = pk_data, 
                 aes(x = NOMTIME, y = LIDV_NORM, 
                     group = TRTACT_high2low, color = TRTACT_high2low))
    gg <- gg + xgx_stat_ci(conf_level = 0.95, alpha = 0.5, position = position_dodge(1))
    gg <- gg + xgx_scale_y_log10()
    gg <- gg + xgx_scale_x_time_units(units_dataset = time_units_dataset, 
                                      units_plot    = time_units_plot)
    gg <- gg + labs(y=concnorm_label, color = trtact_label)
    
    print(gg)
  })
  
  output$output_dose_4 <- renderPlot({
    
    gg <- ggplot(data = pk_data, aes(x = TIME, y = LIDV)) 
    gg <- gg + geom_line(aes(group = ID, color = factor(TRTACT_high2low)), size = 1, alpha = 0.5) 
    gg <- gg + geom_point(data = pk_data %>% filter(CENS==0), aes(color = TRTACT_high2low), size = 2, alpha = 0.5)
    gg <- gg + geom_point(data = pk_data %>% filter(CENS==1), color="red",  shape=8, size = 2, alpha = 0.5)
    gg <- gg + xgx_scale_y_log10()
    gg <- gg + xgx_scale_x_time_units(units_dataset = time_units_dataset, 
                                      units_plot    = time_units_plot)
    gg <- gg + labs(y = conc_label, color = trtact_label)
    
    print(gg)
  })
  
  
  
  
}


shinyApp(ui, server)