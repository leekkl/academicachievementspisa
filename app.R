
library(readxl)
library(ggplot2)
library(ggiraph)
library(patchwork) 
library(shiny)
library(plotly)
library(rsconnect)

rsconnect::writeManifest()

PISA_dataset_mean <- read_xlsx("PISA_dataset3_mean.xlsx")

ui <- fluidPage(
  titlePanel("Academic achievements among 15-year olds, by country"),
  fluidPage(
    mainPanel(
      girafeOutput("my_plots")
    )
  )
)

server <- function(input, output, session) {
  
  output$my_plots<- renderGirafe({
    
    p1 <- ggplot(PISA_dataset_mean, aes(y = reorder(CNT, score_sci_avg, max), x = score_sci_avg, data_id = CNT, color = "red", fill = "red")) +
      geom_violin_interactive(aes(tooltip = paste0("Scaled density: ", round(after_stat(scaled), 5))), 
                              quantile.colour = "black",
                              draw_quantiles = c(0.25, 0.5, 0.75),
                              show.legend = FALSE) + 
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 5, color = "grey40"), # Small x-axis labels
            axis.text.y = element_text(size = 5, color = "grey40"),
            axis.title.x = element_text(size = 9),
            axis.title.y = element_text(size = 9),
            main.title = element_text(size = 10)
      ) +
      labs(x = "Average science scores among 15-year olds", y = "Country", title = "Violin plots of science scores by country, max ordered")
    
    p2 <- ggplot(PISA_dataset_mean, aes(y = reorder(CNT, score_sci_avg, mean), x = score_sci_avg, data_id = CNT, color = "red", fill = "red")) +
      geom_violin_interactive(aes(tooltip = paste0("Scaled density: ", round(after_stat(scaled), 5))), 
                              quantile.colour = "black",
                              draw_quantiles = c(0.25, 0.5, 0.75),
                              show.legend = FALSE) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 5, color = "grey40"), # Small x-axis labels
            axis.text.y = element_text(size = 5, color = "grey40"),
            axis.title.x = element_text(size = 9),
            axis.title.y = element_text(size = 9),
            main.title = element_text(size = 10)
      ) +
      labs(x = "Average science scores among 15-year olds", y = "Country", title = "Violin plots of science scores by country, mean ordered")
    
    p3 <- ggplot(PISA_dataset_mean, aes(y = reorder(CNT, score_read_avg, max), x = score_read_avg, data_id = CNT, color = "red", fill = "red")) +
      geom_violin_interactive(aes(tooltip = paste0("Scaled density: ", round(after_stat(scaled), 5))), 
                              quantile.colour = "black",
                              draw_quantiles = c(0.25, 0.5, 0.75),
                              show.legend = FALSE) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 5, color = "grey40"), # Small x-axis labels
            axis.text.y = element_text(size = 5, color = "grey40"),
            axis.title.x = element_text(size = 9),
            axis.title.y = element_text(size = 9),
            main.title = element_text(size = 10)
      ) +
      labs(x = "Average reading scores among 15-year olds", y = "Country", title = "Violin plots of reading scores by country, max ordered")
    
    p4 <- ggplot(PISA_dataset_mean, aes(y = reorder(CNT, score_read_avg, mean), x = score_read_avg, data_id = CNT, color = "red", fill = "red")) +
      geom_violin_interactive(aes(tooltip = paste0("Scaled density: ", round(after_stat(scaled), 5))), 
                              quantile.colour = "black",
                              draw_quantiles = c(0.25, 0.5, 0.75),
                              show.legend = FALSE) + 
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 5, color = "grey40"), # Small x-axis labels
            axis.text.y = element_text(size = 5, color = "grey40"),
            axis.title.x = element_text(size = 9),
            axis.title.y = element_text(size = 9),
            main.title = element_text(size = 10)
      ) +
      labs(x = "Average reading scores among 15-year olds", y = "Country", title = "Violin plots of reading scores by country, mean ordered")
    
    combined_plots <- (p1 + p2) / (p3 + p4)
    
    girafe(ggobj = combined_plots, width_svg = 10, height_svg = 10,
           options = list(opts_sizing(rescale = FALSE),
                          opts_hover(css = "stroke:orange; stroke-width:2px; opacity:.8;"),
                          opts_hover_inv(
                            css = "opacity: 0.3;" # Dim out non-hovered edges for extra contrast
                          ),
                          opts_selection(type = "single"),
                          reactive = TRUE
           ))
  })
}

shinyApp(ui, server)
