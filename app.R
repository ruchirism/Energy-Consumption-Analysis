#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(arrow)
library(dplyr)
library(tidyverse)
library(rpart)
library(caret)
library(tree)
library(tidyr)
library(ggplot2)
library(forcats)
library(RColorBrewer)

# Define UI
ui <- fluidPage(
  
  titlePanel("Energy Consumption Data Analysis"),
  
    mainPanel(
      #plotOutput("selected_plot")
      tabsetPanel(
        tabPanel("Total Energy Consumption by Income Levels", plotOutput("energy_income_plot")),
        tabPanel("Total Energy Consumption by Income and No. of Occupants", plotOutput("total_energy_income_occupants_plot")),
        tabPanel("Total Energy Consumption by Income and Insulation Level", plotOutput("total_energy_income_insulation_plot")),
        tabPanel("Total Energy Consumption by Income and Number of Bedrooms", plotOutput("total_energy_income_bedrooms_plot")),
        tabPanel("Change in Energy Consumption with Temperature Increase", plotOutput("energy_change_temperature_plot"))
      )
    )
  )

# Define server logic
server <- function(input, output) {
  
  # Read the content of the RMD file
  #getrmd_content <- readLines("FinalProjectIDS.Rmd")
  
  # Render the content
  #output$included_content <- renderUI({
   # includeHTML <- HTML(rmd_content)
    #includeHTML
  #})
  
  # Plot Heating Energy Consumption
  output$energy_income_plot <- renderPlot({
      ggplot() + 
      geom_col(aes(x= july_prediction$in.income_recs_2020, y = july_prediction$total_energy_consumption), fill = "turquoise") +
      ggtitle("Total Energy Consumption for different Income levels") +
      xlab("Income levels 2020") + ylab("Total Energy Consumption")
  })
  
  # Plot Cooling Energy Consumption
  output$total_energy_income_occupants_plot <- renderPlot({
    
    ggplot(july_prediction, aes(x = in.income_recs_2020, 
                                y = total_energy_consumption, 
                                fill = in.occupants)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_brewer(palette = "Spectral") + 
      labs(
        title = "Total Energy Consumption by Income and No. of Occupants",
        x = "Income Brackets 2020",
        y = "Total Energy Consumption",
        fill = "Number of Occupants"
      ) +
      theme_minimal(base_size = 14) + # Base size for readability
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12)
      ) +
      guides(fill = guide_legend(reverse = TRUE))
  })
  
  # Plot Total Energy Consumption by Income and Insulation Level
  output$total_energy_income_insulation_plot <- renderPlot({
    # Plot Total Energy Consumption by Income and Insulation Level
    ggplot(july_prediction, aes(x = fct_inorder(in.income_recs_2020), 
                                y = total_energy_consumption, 
                                fill = fct_rev(fct_inorder(in.insulation_ceiling)))) +
      geom_bar(stat = "identity", position = position_dodge(), width = 0.75) + 
      scale_fill_brewer(palette = "Spectral") + 
      labs(
        title = "Total Energy Consumption by Income and Insulation Level",
        x = "Income Brackets 2020",
        y = "Total Energy Consumption",
        fill = "Ceiling Insulation Level"
      ) +
      theme_minimal(base_size = 14) + # Increase base_size for larger text
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12)
      ) +
      guides(fill = guide_legend(nrow = 1, byrow = TRUE))
  })
  
  # Plot Total Energy Consumption by Income and Number of Bedrooms
  output$total_energy_income_bedrooms_plot <- renderPlot({
    # Plot Total Energy Consumption by Income and Number of Bedrooms
    ggplot(july_prediction_filtered, aes(x = in.income_recs_2020, 
                                         y = total_energy_consumption, 
                                         fill = in.bedrooms)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_brewer(palette = "Spectral") + 
      labs(
        title = "Total Energy Consumption by Income and No. of Bedrooms",
        x = "Income Brackets 2020",
        y = "Total Energy Consumption",
        fill = "Number of Bedrooms"
      ) +
      theme_minimal(base_size = 14) + 
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12)
      ) +
      guides(fill = guide_legend(reverse = TRUE))
  })
  
  # Plot Change in Energy Consumption with Temperature Increase
  output$energy_change_temperature_plot <- renderPlot({
    # Plot Change in Energy Consumption with Temperature Increase
    ggplot(county190long, aes(x = hour, y = energy_value, fill = energy_type)) +
      geom_col(position = "dodge") +
      labs(title = "Total vs Predicted Energy by Hour (with 5 degree increase)",
           x = "Hour of the Day",
           y = "Energy",
           fill = "Energy Type") +
      scale_fill_manual(values = c("lightgreen", "orange")) + 
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
