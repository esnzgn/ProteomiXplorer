source("sys_req.R")  # Load required libraries

# Load and clean data
raw_data <- readxl::read_xlsx("mmc2.xlsx", sheet = 2)

# Identify expression columns (numeric peptide counts)
peptide_cols <- grep("^TenPx\\d+_Peptides$", colnames(raw_data), value = TRUE)

# Identify sample meta columns (tissue + cell line)
meta_cols <- grep("_TenPx\\d+$", colnames(raw_data), value = TRUE)

# Combine Gene_Symbol + expression data + meta info
data_cleaned <- raw_data %>%
  select(Gene_Symbol, all_of(peptide_cols), all_of(meta_cols)) %>%
  filter(rowSums(across(all_of(peptide_cols)), na.rm = TRUE) > 0)

# Melt data for visualization
long_data <- data_cleaned %>%
  pivot_longer(-Gene_Symbol, names_to = "Sample", values_to = "Peptides") %>%
  mutate(
    Type = ifelse(grepl("Peptides$", Sample), "Peptide", "Meta"),
    TenPx = stringr::str_extract(Sample, "TenPx\\d+"),
    CellLine = ifelse(Type == "Meta", gsub("_TenPx\\d+", "", Sample), NA)
  )

# Join expression & metadata
expr_data <- long_data %>% filter(Type == "Peptide")
meta_data <- long_data %>% filter(Type == "Meta") %>% 
  select(TenPx, MetaValue = Peptides, CellLine) %>%
  distinct()

plot_data <- expr_data %>%
  left_join(meta_data, by = "TenPx") %>%
  filter(!is.na(MetaValue), !is.na(Peptides))

# Extract unique inputs
all_genes <- unique(plot_data$Gene_Symbol)
all_tissues <- sort(unique(plot_data$MetaValue))
all_cells <- sort(unique(plot_data$CellLine))

# UI
ui <- dashboardPage(
  dashboardHeader(title = "ProteomiXplorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Expression Viewer", tabName = "viewer", icon = icon("chart-bar"))
    ),
    selectInput("gene", "Select Protein/Gene:", choices = all_genes),
    selectInput("tissue", "Filter by Tissue:", choices = c("All", all_tissues), selected = "All"),
    selectInput("cell", "Filter by Cell Line:", choices = c("All", all_cells), selected = "All")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "viewer",
              fluidRow(
                box(title = "Expression Boxplot", width = 12, plotlyOutput("genePlot")),
                box(title = "Expression Table", width = 12, DTOutput("dataTable"))
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df <- plot_data %>% filter(Gene_Symbol == input$gene)
    
    if (input$tissue != "All") {
      df <- df %>% filter(MetaValue == input$tissue)
    }
    if (input$cell != "All") {
      df <- df %>% filter(CellLine == input$cell)
    }
    df
  })
  
  output$genePlot <- renderPlotly({
    req(filtered_data())
    p <- ggplot(filtered_data(), aes(x = CellLine, y = Peptides)) +
      geom_boxplot(fill = "steelblue") +
      theme_minimal() +
      labs(title = paste("Expression of", input$gene), y = "Peptides", x = "") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  output$dataTable <- renderDT({
    filtered_data() %>% select(Gene_Symbol, CellLine, MetaValue, Peptides)
  })
}

# Run app
shinyApp(ui, server)
