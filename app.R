

library(shiny)

# Define UI for application
ui <- fluidPage(theme = "bootstrap.css",
     
## Pierwsza czesc opisu    
     fluidPage(
          includeMarkdown("Bilanse.Rmd") 
     ),
#######################

     fluidRow( column (1), column (11,
                                   selectInput("bins",
                                               "   Wybór kraju",
                                               choices = list("Austria", "Belgia", "Bułgaria", "Chorwacja", "Cypr", "Czechy", "Dania", "Estonia",
                                                              "Finlandia", "Francja", "Grecja", "Hiszpania", "Holandia", "Irlandia",
                                                              "Litwa", "Luksemburg", "Łotwa", "Malta", "Niemcy", "Polska", "Portugalia", "Rumunia", 
                                                              "Szwecja", "Słowenia", "Słowacja", "Węgry", "Wlk. Brytania", "Włochy",
                                                              "Strefa Euro",  "UE 15", "UE 28"),
                                               selected = "UE 28")
     )),
     
     # Show a plot of the generated distribution
     fluidRow(
          plotOutput("distPlot")
     ),
## Druga czesc opisu
     fluidPage(
          includeMarkdown("Bilanse-aneks.Rmd")
     )
#######################
)


library(eurostat)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)

## Tlumaczenie skrotow krajow z ang. na polski

countries <- cbind(c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EA",
                "EE", "EL", "ES", "EU15", "EU28", "FI", "FR",
                "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MK", "NL",
                "PL", "PT", "RO", "SE", "SI", "SK", "UK"),
                c("Austria", "Belgia", "Bułgaria", "Cypr", "Czechy", "Niemcy",
                  "Dania", "Strefa Euro", "Estonia", "Grecja", "Hiszpania", "UE 15",
                  "UE 28", "Finlandia", "Francja", "Chorwacja", "Węgry", "Irlandia",
                  "Włochy", "Litwa", "Luksemburg", "Łotwa", "Malta", "Holandia", "Polska",
                  "Portugalia", "Rumunia", "Szwecja", "Słowenia", "Słowacja", "Wlk. Brytania")
                    )
colnames(countries) <- c("geo", "geo1")

rok<- paste0(format(Sys.Date(), "%Y"),"-01-01") # potrzebne do aktualizacji danych

## pobieranie danych 
res <- read.csv("data.csv", fileEncoding = "UTF-8")
res$time <- as.Date(res$time)
res$rok <- as.Date(res$rok)
#load("data.RData")
aktualny <- Sys.Date()
stary <- as.Date(res$rok[1])

if (aktualny-stary>91) {
     gdp<-get_eurostat("nama_10_gdp") %>% 
          filter(time >= "2002-01-01") %>%
          filter(time <= rok) %>%  # zapewnia pobieranie najnowszych danych
          filter(unit=="CP_MNAC") %>%
          subset(.,na_item %in% c("P3_S13", "P31_S14_S15", "P51G", "P52_P53",
                                  "P6", "P7", "D1", "B2A3G", "D2X3")) %>%
          select(geo, time, na_item, values)
     gdp<-right_join(gdp, as.data.frame(countries))
     gdp$geo<-as.factor(gdp$geo)
     
     ### Obliczanie bilansow sektorowych ################
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
          mutate (., dG=-(P3_S13-D2X3)) %>%
          mutate (., dE=-(P6-P7)) %>%
          mutate (., ver=dSP-(dG+dE))
     
     ##################
     
     res<-select(tmp, geo1, time, dSP, dG, dE, ver) # dataframe z panstwami i bilansami
     res$rok <- Sys.Date()
     
     write.csv(res, file = "data.csv", fileEncoding = "UTF-8")
     #save(res, file = "data.RData")
} else {
     #load("data.RData")
     res <- read.csv("data.csv", fileEncoding = "UTF-8")
     res$time <- as.Date(res$time)
     res$rok <- as.Date(res$rok)
     res <- select(res, -rok)
}

# Define server logic required to draw a graph
server <- function(input, output) {
     
     output$distPlot <- renderPlot({
          x<-filter(res, geo1==input$bins)
          x1<-gather(x, na_item, values, 3:5)
          x1$na_item<-factor(x1$na_item, levels = c("dSP", "dG", "dE"), labels = c("prywatnego", "publicznego", "zagranicy"))
          
          
          ggplot(x1, aes(x = time, y = values, group = na_item)) +
               geom_col(aes(fill = na_item), position = "stack") +
               theme_hc() + scale_fill_brewer(palette="Set2") +
               scale_y_continuous(labels=function(x)x/1000) +
               labs(fill = "Bilans sektora: ") + ylab("mld") + xlab("rok") +
               ggtitle (levels(x1$geo1)[as.numeric(as.character(x1[1,1]))]) +
               theme(plot.background = element_rect(fill = "#fcfcfc"),
                     legend.background = element_rect(fill = "#fcfcfc"))
     })
}

# Run the application 
shinyApp(ui = ui, server = server)
