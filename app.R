# maiappend

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(ComplexHeatmap)
library(circlize)
library(shinyWidgets)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "ProteomiXplorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Expression Explorer", tabName = "explorer", icon = icon("chart-line")),
      menuItem("Compare Proteins", tabName = "compare", icon = icon("layer-group")),
      menuItem("PCA & UMAP", tabName = "dimred", icon = icon("project-diagram")),
      menuItem("Tissue Mapper", tabName = "mapper", icon = icon("map")),
      menuItem("Quality Control", tabName = "qc", icon = icon("microscope"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "explorer",
              fluidPage(
                pickerInput("gene", "Select Protein/Gene:", choices = NULL, options = list(`live-search` = TRUE)),
                plotlyOutput("expressionPlot"),
                DTOutput("sampleInfo")
              )
      ),
      tabItem(tabName = "compare",
              fluidPage(
                pickerInput("genes", "Select up to 5 Genes:", choices = NULL, multiple = TRUE, options = list(`max-options` = 5, `live-search` = TRUE)),
                plotlyOutput("heatmapPlot"),
                plotlyOutput("corrPlot")
              )
      ),
      tabItem(tabName = "dimred",
              fluidPage(
                radioButtons("dimred_method", "Choose Method:", choices = c("PCA", "UMAP"), inline = TRUE),
                pickerInput("colorBy", "Color by:", choices = c("Tissue", "Selected Gene Expression")),
                plotlyOutput("dimredPlot")
              )
      ),
      tabItem(tabName = "mapper",
              fluidPage(
                plotlyOutput("bubblePlot")
              )
      ),
      tabItem(tabName = "qc",
              fluidPage(
                plotOutput("missingPlot"),
                plotOutput("peptideDist")
              )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Placeholder data load (replace with actual data loading logic)
  data <- reactive({
    # Simulate a dummy dataset
    read.csv("your_cleaned_expression_data.csv")
  })
  
  observe({
    updatePickerInput(session, "gene", choices = unique(data()$Gene_Symbol))
    updatePickerInput(session, "genes", choices = unique(data()$Gene_Symbol))
  })
  
  output$expressionPlot <- renderPlotly({
    req(input$gene)
    df <- data() %>% filter(Gene_Symbol == input$gene)
    df_long <- df %>% pivot_longer(cols = starts_with("TenPx"), names_to = "Sample", values_to = "Expression")
    p <- ggplot(df_long, aes(x = Sample, y = Expression)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = paste("Expression of", input$gene)) +
      theme(axis.text.x = element_text(angle = 90))
    ggplotly(p)
  })
  
  output$sampleInfo <- renderDT({
    req(input$gene)
    df <- data() %>% filter(Gene_Symbol == input$gene)
    datatable(df)
  })
  
  output$heatmapPlot <- renderPlotly({
    req(input$genes)
    df <- data() %>% filter(Gene_Symbol %in% input$genes)
    mat <- df %>% select(Gene_Symbol, starts_with("TenPx")) %>% column_to_rownames("Gene_Symbol") %>% as.matrix()
    heatmaply::heatmaply(mat, scale = "row")
  })
  
  output$corrPlot <- renderPlotly({
    req(input$genes)
    df <- data() %>% filter(Gene_Symbol %in% input$genes)
    mat <- df %>% select(Gene_Symbol, starts_with("TenPx")) %>% column_to_rownames("Gene_Symbol") %>% t() %>% cor()
    heatmaply::heatmaply(mat)
  })
  
  output$dimredPlot <- renderPlotly({
    # Placeholder for PCA/UMAP logic based on full expression matrix
    NULL
  })
  
  output$bubblePlot <- renderPlotly({
    # Placeholder for Tissue Mapper
    NULL
  })
  
  output$missingPlot <- renderPlot({
    df <- data()
    missing_data <- df %>% select(starts_with("TenPx")) %>% summarise_all(~sum(is.na(.)))
    barplot(unlist(missing_data), las = 2, main = "Missing Data per Sample")
  })
  
  output$peptideDist <- renderPlot({
    df <- data()
    peptides <- df %>% select(contains("Peptides")) %>% pivot_longer(everything(), names_to = "Sample", values_to = "Peptides")
    ggplot(peptides, aes(x = Peptides)) +
      geom_histogram(bins = 50, fill = "steelblue") +
      theme_minimal() +
      labs(title = "Peptide Count Distribution")
  })
}

shinyApp(ui, server)




