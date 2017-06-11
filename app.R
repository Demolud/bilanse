#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application
ui <- fluidPage(
   
   # Application title
     fluidPage(
          includeMarkdown("Bilanse.rmd")
     ),

   fluidRow( column (1), column (11,
         selectInput("bins",
                     "   Wybór kraju",
                     choices = list("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EA","EA12",
                                    "EA19", "EE", "EL", "ES", "EU15", "EU28", "FI", "FR",
                                    "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MK", "NL",
                                    "NO", "PL", "PT", "RO", "RS", "SE", "SI", "SK", "UK"),
                     selected = "EU28")
      )),
      
      # Show a plot of the generated distribution
      fluidRow(
         plotOutput("distPlot")
      ),
   fluidPage(
        includeMarkdown("Bilanse-aneks.rmd")
   )
   )


library(eurostat)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)

gdp<-get_eurostat("nama_10_gdp") %>% 
     filter(time >= "2002-01-01") %>%
     filter(time <= "2016-01-01") %>%
     filter(unit=="CP_MNAC") %>%
     subset(.,na_item %in% c("P3_S13", "P31_S14_S15", "P51G", "P52_P53",
                             "P6", "P7", "D1", "B2A3G", "D2X3")) %>%
     select(geo, time, na_item, values)

#SP=D1+B2A3G
#I=P51G+P52_P53
#G=P3_S13
#T=D2X3
#E=P6
#Im=P7
#
#(SP-I)=(G-T)+(E-IM)

tmp<-spread(gdp,na_item, values) %>%
     mutate (.,SP=D1+B2A3G-P31_S14_S15) %>%
     mutate (.,I=P51G+P52_P53) %>%
     mutate (., dSP=SP-I) %>%
     mutate (., dG=P3_S13-D2X3) %>%
     mutate (., dE=P6-P7) %>%
     mutate (., ver=dSP-(dG+dE))

res<-select(tmp, geo, time, dSP, dG, dE, ver)
# Define server logic required to draw a histogram
server <- function(input, output) {
     
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
        x<-filter(res, geo==input$bins)
        x$dG<--x$dG
        x$dE<--x$dE
        x1<-gather(x, na_item, values, 3:5)
        x1$na_item<-factor(x1$na_item, levels = c("dSP", "dG", "dE"), labels = c("prywatnego", "publicznego", "zagranicy"))
        
      
        ggplot(x1, aes(x = time, y = values, group = na_item)) +
             geom_col(aes(fill = na_item), position = "stack") +
             theme_hc() + scale_fill_brewer(palette="Set2") +
             scale_y_continuous(labels=function(x)x/1000) +
             labs(fill = "Bilans sektora: ") + ylab("mld") + xlab("rok") +
             ggtitle (levels(x1$geo)[as.numeric(as.character(x1[1,1]))])
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

