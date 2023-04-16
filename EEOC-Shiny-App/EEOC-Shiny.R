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
      sliderInput("varYears", 
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
    mainPanel(
      tableOutput("table"),
      verbatimTextOutput("lm_model_summary"),
      verbatimTextOutput("log_model_summary")
      )
  )
)


# Define server logic ----
server <- function(input, output) {
  

  
    output$table <-renderTable({
      eeoc_transformed <- eeoc %>%
        filter(Sex == input$varSex & Race == input$varRace & Profession == input$varProfession ) %>%
        filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
        # filter_if(input$varIndGroup != "All Industry Groups", `Industry Group` == input$varIndGroup) %>%
        # filter_if(input$varInd == "All Industries", Industry == input$varInd) %>%
        group_by(Year) %>%
        dplyr::summarise(Workers = as.integer(sum(Workers)))
      })

    output$lm_model_summary <- renderPrint({
      eeoc_transformed <- eeoc %>%
        filter(Sex == input$varSex & Race == input$varRace & Profession == input$varProfession ) %>%
        filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
        # filter_if(input$varIndGroup != "All Industry Groups", `Industry Group` == input$varIndGroup) %>%
        # filter_if(input$varInd == "All Industries", Industry == input$varInd) %>%
        group_by(Year) %>%
        dplyr::summarise(Workers = as.integer(sum(Workers)))
      
      model <- lm(Workers ~ Year, data = eeoc_transformed)
      
      summary(model)
    })
    
    output$log_model_summary <- renderPrint({
      eeoc_transformed <- eeoc %>%
        filter(Sex == input$varSex & Race == input$varRace & Profession == input$varProfession ) %>%
        filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
        # filter_if(input$varIndGroup != "All Industry Groups", `Industry Group` == input$varIndGroup) %>%
        # filter_if(input$varInd == "All Industries", Industry == input$varInd) %>%
        group_by(Year) %>%
        dplyr::summarise(Workers = as.integer(sum(Workers)))
      
      model <- glm(Workers ~ Year, data = eeoc_transformed, family = "poisson")
      
      summary(model)
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)