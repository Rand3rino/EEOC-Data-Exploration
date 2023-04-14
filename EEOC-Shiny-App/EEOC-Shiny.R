library(shiny)
library(dplyr)

eeoc <- read_csv("https://github.com/Rand3rino/EEOC-Data-Exploration/blob/master/EEOC.csv?raw=true") 
eeoc <- eeoc %>% filter(!is.na(Workers))

# Define UI ----
ui <- fluidPage(
  titlePanel("EEOC Data Exploration"),
  sidebarLayout(
    sidebarPanel(
      helpText("Select the data filters that you would like to apply to the model. You can also choose the model type you'd like to use."),
      sliderInput("years", 
                  label = "Years",
                  min = min(eeoc$Year), max = max(eeoc$Year), 
                  value = c(min(eeoc$Year),max(eeoc$Year))),
      selectInput("varIndGroup", 
                  label = "Industry Group",
                  choices = c("All Industry Groups", unique(eeoc$`Industry Group`)),
                  selected = "All Industry Groups"),
      selectInput("varInd", 
                  label = "Industry",
                  choices = c("All Industries", unique(eeoc$Industry)),
                  selected = "All Industries"),
      selectInput("varSex", 
                  label = "Sex",
                  choices = c(unique(eeoc$Sex)),
                  selected = "All Sexes"),
      selectInput("varRace", 
                  label = "Race",
                  choices = c(unique(eeoc$Race)),
                  selected = "All Races"),
      selectInput("varProfession", 
                  label = "Profession",
                  choices = c(unique(eeoc$Profession)),
                  selected = "All Jobs")
      ),
    mainPanel(tableOutput("table"))
  )
)


# Define server logic ----
server <- function(input, output) {
  
    output$table <-renderTable(
      data <- eeoc %>% 
                 filter(Sex == input$varSex & Race == input$varRace) %>% 
                 #input$varSex) %>% 
                 group_by(Year) %>% 
                 dplyr::summarise(Worker = sum(Workers))
      )
}

# Run the app ----
shinyApp(ui = ui, server = server)