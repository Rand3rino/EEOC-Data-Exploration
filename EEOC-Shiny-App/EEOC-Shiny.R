library(shiny)

eeoc <- read_csv("https://github.com/Rand3rino/EEOC-Data-Exploration/blob/master/EEOC.csv?raw=true")

# Define UI ----
ui <- fluidPage(
  titlePanel("EEOC Data Exploration"),
  sidebarLayout(
    sidebarPanel(
      helpText("Select your desired model function, 
                   target variable, and explanatory variables."),
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
    mainPanel()
  )
)


# Define server logic ----
server <- function(input, output) {
}

# Run the app ----
shinyApp(ui = ui, server = server)