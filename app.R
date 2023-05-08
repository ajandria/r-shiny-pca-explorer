library(shiny)
library(readxl)
library(plotly)
library(dplyr)
library(ggplot2)
library(shinyjqui)

# Set maximum upload size to 500 MB
options(shiny.maxRequestSize = 500 * 1024^2)

# UI
ui <- fluidPage(
  titlePanel("PCA Plotter"),
  sidebarLayout(
    sidebarPanel(
      # File inputs for count matrix and metadata
      fileInput("norm_count_matrix", "Upload count matrix (Excel file)"),
      fileInput("metadata", "Upload metadata (Excel file, optional)")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("2D PCA Plot",
                 jqui_resizable(
                   plotlyOutput("pca2d", height = "600px", width = "100%")
                 )
        ),
        tabPanel("3D PCA Plot",
                 jqui_resizable(
                   plotlyOutput("pca3d", height = "600px", width = "100%")
                 )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive function to load count matrix
  normalized_pca_2d <- reactive({
    req(input$norm_count_matrix)
    count_matrix <- read_excel(input$norm_count_matrix$datapath) %>%
      tibble::column_to_rownames(var = 'Geneid') %>%
      t()
    prcomp(count_matrix, rank. = 2)
  })
  
  
  # Reactive function to load count matrix
  normalized_pca_3d <- reactive({
    req(input$norm_count_matrix)
    count_matrix <- read_excel(input$norm_count_matrix$datapath) %>%
      tibble::column_to_rownames(var = 'Geneid') %>%
      t()
    prcomp(count_matrix, rank. = 3)
  })
  
  # Reactive function to load metadata
  metadata <- reactive({
    req(input$metadata)
    read_excel(input$metadata$datapath)
  })
  
  # Render 2D PCA plot
  output$pca2d <- renderPlotly({
    normalized_pca_2d <- req(normalized_pca_2d())
    metadata <- req(metadata())
    
    # Create label for PCA showing % of explained variance
    normalized_pca_2d_sum <- summary(normalized_pca_2d)
    normalized_pca_2d_sum_total <- sum(normalized_pca_2d_sum$importance[2, 1:2]) * 100
    
    # Create data frame with sample names and metadata
    pca2d_info <- data.frame(sample = rownames(normalized_pca_2d$x)) %>%
      left_join(metadata)
    
    # Create PCA plot
    normalized_pca2d_plot_all <- ggplot(data.frame(normalized_pca_2d$x),
                                        aes(PC1, PC2, color = pca2d_info$group, label = pca2d_info$sample)) +
      geom_jitter(size = 3) +
      theme_minimal()
    
    # Convert PCA plot to be interactive
    normalized_pca2d_plot_all_i <- normalized_pca2d_plot_all %>%
      ggplotly() %>%
      layout(margin = list(t = 75))
    
    return(normalized_pca2d_plot_all_i)
  })
  
  # Render 3D PCA plot
  output$pca3d <- renderPlotly({
    normalized_pca_3d <- req(normalized_pca_3d())
    metadata <- req(metadata())
    
    # Create label for PCA showing % of explained variance
    normalized_pca_3d_sum <- summary(normalized_pca_3d)
    normalized_pca_3d_sum_total <- sum(normalized_pca_3d_sum$importance[2, 1:3]) * 100
    
    # Create data frame with sample names and metadata
    pca3d_info <- data.frame(sample = rownames(normalized_pca_3d$x)) %>%
      left_join(metadata)
    
    View(pca3d_info)
    print(rownames(normalized_pca_3d$x))
    
    Condition_3d <- pca3d_info$group
    Sample_3d <- pca3d_info$sample
    
    # Create PCA plot
    normalized_pca3d_plot_all_i <-
      plot_ly(
        data.frame(normalized_pca_3d$x),
        text = ~ rownames(normalized_pca_3d$x),
        x = ~ PC1,
        y = ~ PC2,
        z = ~ PC3,
        color = ~ Condition_3d
      ) %>%
      add_markers() %>%
      layout(scene = list(
        xaxis = list(title = paste(
          "PC1 -",
          round(normalized_pca_3d_sum$importance[2, 1], 3)
        )),
        yaxis = list(title = paste(
          "PC2 -",
          round(normalized_pca_3d_sum$importance[2, 2], 3)
        )),
        zaxis = list(title = paste(
          "PC3 -",
          round(normalized_pca_3d_sum$importance[2, 3], 3)
        ))
      ))
    
    
    return(normalized_pca3d_plot_all_i)
  })
  
}



# Run Shiny app
shinyApp(ui = ui, server = server)
