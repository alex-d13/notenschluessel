#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# rsconnect::deployApp('~/Documents/projects/notenschluessel/')

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Notenschlüssel Berechner für Kathi"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("max_points",
                        "Maximale Punktzahl",
                        min = 6,
                        max = 100,
                        value = 30),
            hr(),
            h4('Gib hier die Prozentschritte der einzelnen Noten ein, getrennt durch einen Bindestrich:'),
            p('Die Werte werden immer als Halboffenes Intervall gelesen, d.h. der erste Wert ist ausgeschlossen, der zweite eingeschlossen (Beispiel: ]85-70]). Aussnahme ist immer Note 1, da sie mit 100% beginnen muss und damit beide Werte eingeschlossen sind.'),
            textInput('grade1',
                      'Note 1 (%)', value = '100-85', placeholder = '100-85'),
            textInput('grade2',
                      'Note 2 (%)', value = '85-70', placeholder = '85-70'),
            textInput('grade3',
                      'Note 3 (%)', value = '70-55', placeholder = '70-55'),
            textInput('grade4',
                      'Note 4 (%)', value = '55-40', placeholder = '55-40'),
            textInput('grade5',
                      'Note 5 (%)', value = '40-20', placeholder = '40-20'),
            textInput('grade6',
                      'Note 6 (%)', value = '20-0', placeholder = '20-0'),
            p('Vorsicht: Aktuell wird noch nicht geprüft, ob Lücken zwischen den einzelnen Notenintervalle sind. Ist aber work in progress ;)')
        ),

        # Show a plot of the generated distribution
        mainPanel(
           dataTableOutput("grading_table"),
           hr(),
           plotOutput('grading_table_plot'),
           hr(),
           fluidRow(
             column(5),
             column(5, box(title='References', status = 'primary', solidHeader = T,
                           p('blabla'), width = 4))
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$grading_table <- renderDataTable({
      df <- calc_grades(input)
      df %>% select('Note', 'unterer.Grenzwert', 'oberer.Grenzwert')
    })
    
    output$grading_table_plot <- renderPlot({
      df <- calc_grades(input)
      df$Punkte <- df[['oberer.Grenzwert']] - df[['unterer.Grenzwert']]
      
      ggplot(df, aes(x=Note, y=Punkte))+
        geom_col(fill = '#e76f51')+
        ggtitle('Anzahl der vorhandenen Punkte pro Note')+
        theme_bw()+
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold"))+
        scale_x_continuous("Note", labels = as.character(df$Note), breaks = df$Note)
    })
}

calc_grades <- function(input){
  df <- data.frame('Note'=c(1:6), 
                   'oberer Grenzwert'=rep(input$max_points, 6), 
                   'unterer Grenzwert'=rep(input$max_points, 6))
  
  #sorry for ugly mess, had to be quick 
  df[1,3] <- plyr::round_any(input$max_points * (as.numeric(strsplit(input$grade1,'-')[[1]][2]) / 100), 0.5, f = ceiling)
  df[2,2] <- plyr::round_any(input$max_points * (as.numeric(strsplit(input$grade2,'-')[[1]][1]) / 100), 0.5, f = floor)
  if(df[1,3] == df[2,2]){
    df[2,2] = df[2,2] - 0.5
  }
  
  df[2,3] <- plyr::round_any(input$max_points * (as.numeric(strsplit(input$grade2,'-')[[1]][2]) / 100), 0.5, f = ceiling)
  df[3,2] <- plyr::round_any(input$max_points * (as.numeric(strsplit(input$grade3,'-')[[1]][1]) / 100), 0.5, f = floor)
  if(df[2,3] == df[3,2]){
    df[3,2] = df[3,2] - 0.5
  }
  
  df[3,3] <- plyr::round_any(input$max_points * (as.numeric(strsplit(input$grade3,'-')[[1]][2]) / 100), 0.5, f = ceiling)
  df[4,2] <- plyr::round_any(input$max_points * (as.numeric(strsplit(input$grade4,'-')[[1]][1]) / 100), 0.5, f = floor)
  if(df[3,3] == df[4,2]){
    df[4,2] = df[4,2] - 0.5
  }
  
  df[4,3] <- plyr::round_any(input$max_points * (as.numeric(strsplit(input$grade4,'-')[[1]][2]) / 100), 0.5, f = ceiling)
  df[5,2] <- plyr::round_any(input$max_points * (as.numeric(strsplit(input$grade5,'-')[[1]][1]) / 100), 0.5, f = floor)
  if(df[4,3] == df[4,2]){
    df[4,2] = df[4,2] - 0.5
  }
  
  df[5,3] <- plyr::round_any(input$max_points * (as.numeric(strsplit(input$grade5,'-')[[1]][2]) / 100), 0.5, f = ceiling)
  df[6,2] <- plyr::round_any(input$max_points * (as.numeric(strsplit(input$grade6,'-')[[1]][1]) / 100), 0.5, f = floor)
  if(df[5,3] == df[6,2]){
    df[6,2] = df[6,2] - 0.5
  }
  
  df[6,3] <- 0
  return(df)
}

# Run the application 
shinyApp(ui = ui, server = server)
