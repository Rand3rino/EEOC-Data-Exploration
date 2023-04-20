library(shiny)
library(dplyr)

# Formatting Scientific Notation
# https://rpubs.com/techanswers88/remove-scientific-notation-in-ggplot
options(scipen = 999)
library(scales)

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
      br(),
      strong("Target Population"),
      selectInput("varIndGroup1", 
                  label = "Industry Group",
                  choices = c("All Industry Groups", unique(eeoc$`Industry Group`)),
                  selected = "All Industry Groups"),
      selectInput("varInd1", 
                  label = "Industry",
                  choices = c("All Industries", unique(eeoc$Industry)),
                  selected = "All Industries"),
      selectInput("varSex1", 
                  label = "Sex",
                  choices = c(unique(eeoc$Sex)),
                  selected = "All Sexes"),
      selectInput("varRace1", 
                  label = "Race",
                  choices = c(unique(eeoc$Race)),
                  selected = "All Races"),
      selectInput("varProfession1", 
                  label = "Profession",
                  choices = c(unique(eeoc$Profession)),
                  selected = "All Jobs"),
      br(),
      strong("Explanatory Population"),
      selectInput("varIndGroup2", 
                  label = "Industry Group",
                  choices = c("All Industry Groups", unique(eeoc$`Industry Group`)),
                  selected = "All Industry Groups"),
      selectInput("varInd2", 
                  label = "Industry",
                  choices = c("All Industries", unique(eeoc$Industry)),
                  selected = "All Industries"),
      selectInput("varSex2", 
                  label = "Sex",
                  choices = c(unique(eeoc$Sex)),
                  selected = "All Sexes"),
      selectInput("varRace2", 
                  label = "Race",
                  choices = c(unique(eeoc$Race)),
                  selected = "All Races"),
      selectInput("varProfession2", 
                  label = "Profession",
                  choices = c(unique(eeoc$Profession)),
                  selected = "All Jobs")
      ),
    mainPanel(
      plotOutput("plot"),
      tableOutput("table"),
      textOutput("lm_text_summary"),
      verbatimTextOutput("lm_model_summary")
      #verbatimTextOutput("log_model_summary")
      )
  )
)


# Define server logic ----
server <- function(input, output) {
  
    output$plot <- renderPlot ({
    if (input$varInd1 == "All Industries" & input$varIndGroup1 == "All Industry Groups") {
      eeoc_transformed1 <- eeoc %>%
        filter(Sex == input$varSex1 & Race == input$varRace1 & Profession == input$varProfession1 ) %>%
        filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
        mutate(Year = as.integer(Year)) %>% 
        group_by(Year) %>%
        dplyr::summarise(Target = as.integer(sum(Workers)))
    } else if (input$varInd1 == "All Industries") {
      eeoc_transformed1 <- eeoc %>%
        filter(Sex == input$varSex1 & Race == input$varRace1 & Profession == input$varProfession1 ) %>%
        filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
        mutate(Year = as.integer(Year)) %>% 
        filter(`Industry Group` == input$varIndGroup1) %>% 
        group_by(Year) %>%
        dplyr::summarise(Target = as.integer(sum(Workers)))
    } else {
      eeoc_transformed1 <- eeoc %>%
        filter(Sex == input$varSex1 & Race == input$varRace1 & Profession == input$varProfession1 ) %>%
        filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
        filter(`Industry Group` == input$varIndGroup1 & Industry == input$varInd1) %>% 
        mutate(Year = as.integer(Year)) %>% 
        group_by(Year) %>%
        dplyr::summarise(Target = as.integer(sum(Workers)))
    }
    
    if (input$varInd2 == "All Industries" & input$varIndGroup2 == "All Industry Groups") {
      eeoc_transformed2 <- eeoc %>%
        filter(Sex == input$varSex2 & Race == input$varRace2 & Profession == input$varProfession2 ) %>%
        filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
        mutate(Year = as.integer(Year)) %>% 
        group_by(Year) %>%
        dplyr::summarise(Explanatory = as.integer(sum(Workers)))
    } else if (input$varInd2 == "All Industries") {
      eeoc_transformed2 <- eeoc %>%
        filter(Sex == input$varSex2 & Race == input$varRace2 & Profession == input$varProfession2 ) %>%
        filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
        filter(`Industry Group` == input$varIndGroup2) %>% 
        mutate(Year = as.integer(Year)) %>% 
        group_by(Year) %>%
        dplyr::summarise(Explanatory = as.integer(sum(Workers)))
    } else {
      eeoc_transformed2 <- eeoc %>%
        filter(Sex == input$varSex2 & Race == input$varRace2 & Profession == input$varProfession2 ) %>%
        filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
        filter(`Industry Group` == input$varIndGroup2 & Industry == input$varInd2) %>% 
        mutate(Year = as.integer(Year)) %>% 
        group_by(Year) %>%
        dplyr::summarise(Explanatory = as.integer(sum(Workers)))
    }
    
    eeoc_combined <- full_join(eeoc_transformed1, eeoc_transformed2, by = "Year")
    
    eeoc_combined %>% 
      ggplot(aes(x = Explanatory, y = Target)) +
      geom_point() + 
      ggtitle("Target vs Explanatory EEO Plot") +
      scale_y_continuous(labels = scales::comma) + 
      scale_x_continuous(labels = scales::comma) +
      theme_bw()
  })
  
    output$table <-renderTable({
      if (input$varInd1 == "All Industries" & input$varIndGroup1 == "All Industry Groups") {
        eeoc_transformed1 <- eeoc %>%
          filter(Sex == input$varSex1 & Race == input$varRace1 & Profession == input$varProfession1 ) %>%
          filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
          mutate(Year = as.integer(Year)) %>% 
          group_by(Year) %>%
          dplyr::summarise(Target = as.integer(sum(Workers)))
      } else if (input$varInd1 == "All Industries") {
        eeoc_transformed1 <- eeoc %>%
          filter(Sex == input$varSex1 & Race == input$varRace1 & Profession == input$varProfession1 ) %>%
          filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
          mutate(Year = as.integer(Year)) %>% 
          filter(`Industry Group` == input$varIndGroup1) %>% 
          group_by(Year) %>%
          dplyr::summarise(Target = as.integer(sum(Workers)))
      } else {
        eeoc_transformed1 <- eeoc %>%
          filter(Sex == input$varSex1 & Race == input$varRace1 & Profession == input$varProfession1 ) %>%
          filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
          filter(`Industry Group` == input$varIndGroup1 & Industry == input$varInd1) %>% 
          mutate(Year = as.integer(Year)) %>% 
          group_by(Year) %>%
          dplyr::summarise(Target = as.integer(sum(Workers)))
      }
      
      if (input$varInd2 == "All Industries" & input$varIndGroup2 == "All Industry Groups") {
        eeoc_transformed2 <- eeoc %>%
          filter(Sex == input$varSex2 & Race == input$varRace2 & Profession == input$varProfession2 ) %>%
          filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
          mutate(Year = as.integer(Year)) %>% 
          group_by(Year) %>%
          dplyr::summarise(Explanatory = as.integer(sum(Workers)))
      } else if (input$varInd2 == "All Industries") {
        eeoc_transformed2 <- eeoc %>%
          filter(Sex == input$varSex2 & Race == input$varRace2 & Profession == input$varProfession2 ) %>%
          filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
          filter(`Industry Group` == input$varIndGroup2) %>% 
          mutate(Year = as.integer(Year)) %>% 
          group_by(Year) %>%
          dplyr::summarise(Explanatory = as.integer(sum(Workers)))
      } else {
        eeoc_transformed2 <- eeoc %>%
          filter(Sex == input$varSex2 & Race == input$varRace2 & Profession == input$varProfession2 ) %>%
          filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
          filter(`Industry Group` == input$varIndGroup2 & Industry == input$varInd2) %>% 
          mutate(Year = as.integer(Year)) %>% 
          group_by(Year) %>%
          dplyr::summarise(Explanatory = as.integer(sum(Workers)))
      }
      
      eeoc_combined <- full_join(eeoc_transformed1, eeoc_transformed2, by = "Year")

      })

    output$lm_text_summary <- renderPrint({
      if (input$varInd1 == "All Industries" & input$varIndGroup1 == "All Industry Groups") {
        eeoc_transformed1 <- eeoc %>%
          filter(Sex == input$varSex1 & Race == input$varRace1 & Profession == input$varProfession1 ) %>%
          filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
          mutate(Year = as.integer(Year)) %>% 
          group_by(Year) %>%
          dplyr::summarise(Target = as.integer(sum(Workers)))
      } else if (input$varInd1 == "All Industries") {
        eeoc_transformed1 <- eeoc %>%
          filter(Sex == input$varSex1 & Race == input$varRace1 & Profession == input$varProfession1 ) %>%
          filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
          mutate(Year = as.integer(Year)) %>% 
          filter(`Industry Group` == input$varIndGroup1) %>% 
          group_by(Year) %>%
          dplyr::summarise(Target = as.integer(sum(Workers)))
      } else {
        eeoc_transformed1 <- eeoc %>%
          filter(Sex == input$varSex1 & Race == input$varRace1 & Profession == input$varProfession1 ) %>%
          filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
          filter(`Industry Group` == input$varIndGroup1 & Industry == input$varInd1) %>% 
          mutate(Year = as.integer(Year)) %>% 
          group_by(Year) %>%
          dplyr::summarise(Target = as.integer(sum(Workers)))
      }
      
      if (input$varInd2 == "All Industries" & input$varIndGroup2 == "All Industry Groups") {
        eeoc_transformed2 <- eeoc %>%
          filter(Sex == input$varSex2 & Race == input$varRace2 & Profession == input$varProfession2 ) %>%
          filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
          mutate(Year = as.integer(Year)) %>% 
          group_by(Year) %>%
          dplyr::summarise(Explanatory = as.integer(sum(Workers)))
      } else if (input$varInd2 == "All Industries") {
        eeoc_transformed2 <- eeoc %>%
          filter(Sex == input$varSex2 & Race == input$varRace2 & Profession == input$varProfession2 ) %>%
          filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
          filter(`Industry Group` == input$varIndGroup2) %>% 
          mutate(Year = as.integer(Year)) %>% 
          group_by(Year) %>%
          dplyr::summarise(Explanatory = as.integer(sum(Workers)))
      } else {
        eeoc_transformed2 <- eeoc %>%
          filter(Sex == input$varSex2 & Race == input$varRace2 & Profession == input$varProfession2 ) %>%
          filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
          filter(`Industry Group` == input$varIndGroup2 & Industry == input$varInd2) %>% 
          mutate(Year = as.integer(Year)) %>% 
          group_by(Year) %>%
          dplyr::summarise(Explanatory = as.integer(sum(Workers)))
      }
      
      eeoc_combined <- full_join(eeoc_transformed1, eeoc_transformed2, by = "Year")
      
      model <- lm(Target ~ Explanatory, data = eeoc_combined)
      
      # Getting Model Coefficients 
      # https://stackoverflow.com/questions/57051942/extracting-the-coefficients-from-the-model-summary-to-make-a-equation
      
      paste("Using a linear model, the target population of ", input$varSex1, ", ", input$varRace1, " working as ", input$varProfession1, " in ", input$varIndGroup1, "(", input$varInd1, ")",
            " is ", 100 * round(summary(model)$coef[2,1], 2), "% of the explantory population of ", 
            input$varSex2, ", ", input$varRace2, " working as ", input$varProfession2, " in ", input$varIndGroup2, "(", input$varInd2, ")",
            " and an additional ", round(summary(model)$coef[1,1], 0), " people.", sep = "" )
      
    })
    
    output$lm_model_summary <- renderPrint({
      if (input$varInd1 == "All Industries" & input$varIndGroup1 == "All Industry Groups") {
        eeoc_transformed1 <- eeoc %>%
          filter(Sex == input$varSex1 & Race == input$varRace1 & Profession == input$varProfession1 ) %>%
          filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
          mutate(Year = as.integer(Year)) %>% 
          group_by(Year) %>%
          dplyr::summarise(Target = as.integer(sum(Workers)))
      } else if (input$varInd1 == "All Industries") {
        eeoc_transformed1 <- eeoc %>%
          filter(Sex == input$varSex1 & Race == input$varRace1 & Profession == input$varProfession1 ) %>%
          filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
          mutate(Year = as.integer(Year)) %>% 
          filter(`Industry Group` == input$varIndGroup1) %>% 
          group_by(Year) %>%
          dplyr::summarise(Target = as.integer(sum(Workers)))
      } else {
        eeoc_transformed1 <- eeoc %>%
          filter(Sex == input$varSex1 & Race == input$varRace1 & Profession == input$varProfession1 ) %>%
          filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
          filter(`Industry Group` == input$varIndGroup1 & Industry == input$varInd1) %>% 
          mutate(Year = as.integer(Year)) %>% 
          group_by(Year) %>%
          dplyr::summarise(Target = as.integer(sum(Workers)))
      }
      
      if (input$varInd2 == "All Industries" & input$varIndGroup2 == "All Industry Groups") {
        eeoc_transformed2 <- eeoc %>%
          filter(Sex == input$varSex2 & Race == input$varRace2 & Profession == input$varProfession2 ) %>%
          filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
          mutate(Year = as.integer(Year)) %>% 
          group_by(Year) %>%
          dplyr::summarise(Explanatory = as.integer(sum(Workers)))
      } else if (input$varInd2 == "All Industries") {
        eeoc_transformed2 <- eeoc %>%
          filter(Sex == input$varSex2 & Race == input$varRace2 & Profession == input$varProfession2 ) %>%
          filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
          filter(`Industry Group` == input$varIndGroup2) %>% 
          mutate(Year = as.integer(Year)) %>% 
          group_by(Year) %>%
          dplyr::summarise(Explanatory = as.integer(sum(Workers)))
      } else {
        eeoc_transformed2 <- eeoc %>%
          filter(Sex == input$varSex2 & Race == input$varRace2 & Profession == input$varProfession2 ) %>%
          filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
          filter(`Industry Group` == input$varIndGroup2 & Industry == input$varInd2) %>% 
          mutate(Year = as.integer(Year)) %>% 
          group_by(Year) %>%
          dplyr::summarise(Explanatory = as.integer(sum(Workers)))
      }
      
      eeoc_combined <- full_join(eeoc_transformed1, eeoc_transformed2, by = "Year")
      
      model <- lm(Target ~ Explanatory, data = eeoc_combined)
      
      summary(model)
    })
    
    # output$log_model_summary <- renderPrint({
    #   if (input$varInd1 == "All Industries" & input$varIndGroup1 == "All Industry Groups") {
    #     eeoc_transformed1 <- eeoc %>%
    #       filter(Sex == input$varSex1 & Race == input$varRace1 & Profession == input$varProfession1 ) %>%
    #       filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
    #       mutate(Year = as.integer(Year)) %>% 
    #       group_by(Year) %>%
    #       dplyr::summarise(Target = as.integer(sum(Workers)))
    #   } else if (input$varInd1 == "All Industries") {
    #     eeoc_transformed1 <- eeoc %>%
    #       filter(Sex == input$varSex1 & Race == input$varRace1 & Profession == input$varProfession1 ) %>%
    #       filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
    #       mutate(Year = as.integer(Year)) %>% 
    #       filter(`Industry Group` == input$varIndGroup1) %>% 
    #       group_by(Year) %>%
    #       dplyr::summarise(Target = as.integer(sum(Workers)))
    #   } else {
    #     eeoc_transformed1 <- eeoc %>%
    #       filter(Sex == input$varSex1 & Race == input$varRace1 & Profession == input$varProfession1 ) %>%
    #       filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
    #       filter(`Industry Group` == input$varIndGroup1 & Industry == input$varInd1) %>% 
    #       mutate(Year = as.integer(Year)) %>% 
    #       group_by(Year) %>%
    #       dplyr::summarise(Target = as.integer(sum(Workers)))
    #   }
    #   
    #   if (input$varInd2 == "All Industries" & input$varIndGroup2 == "All Industry Groups") {
    #     eeoc_transformed2 <- eeoc %>%
    #       filter(Sex == input$varSex2 & Race == input$varRace2 & Profession == input$varProfession2 ) %>%
    #       filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
    #       mutate(Year = as.integer(Year)) %>% 
    #       group_by(Year) %>%
    #       dplyr::summarise(Explanatory = as.integer(sum(Workers)))
    #   } else if (input$varInd2 == "All Industries") {
    #     eeoc_transformed2 <- eeoc %>%
    #       filter(Sex == input$varSex2 & Race == input$varRace2 & Profession == input$varProfession2 ) %>%
    #       filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
    #       filter(`Industry Group` == input$varIndGroup2) %>% 
    #       mutate(Year = as.integer(Year)) %>% 
    #       group_by(Year) %>%
    #       dplyr::summarise(Explanatory = as.integer(sum(Workers)))
    #   } else {
    #     eeoc_transformed2 <- eeoc %>%
    #       filter(Sex == input$varSex2 & Race == input$varRace2 & Profession == input$varProfession2 ) %>%
    #       filter(Year >= min(input$varYears) & Year <= max(input$varYears)) %>%
    #       filter(`Industry Group` == input$varIndGroup2 & Industry == input$varInd2) %>% 
    #       mutate(Year = as.integer(Year)) %>% 
    #       group_by(Year) %>%
    #       dplyr::summarise(Explanatory = as.integer(sum(Workers)))
    #   }
    #   
    #   eeoc_combined <- full_join(eeoc_transformed1, eeoc_transformed2, by = "Year")
    #   
    #   model <- glm(Target ~ Explanatory, data = eeoc_combined, family = "poisson")
    #   
    #   summary(model)
    # })
}

# Run the app ----
shinyApp(ui = ui, server = server)