#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("ggpubr")

library(shiny)
library(ggplot2)
library(ggpubr)

labels <- c(
  "bytes_sum"="Cummulative read bytes", 
  "bytes"="Read bytes", 
  "iat"="Inter-arrival time (IAT)"
)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Website Fingerprints"),

    div(style = "display: flex;",
        HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'),
        selectInput("yaxis", "Choose a y variable", choices = list("Cummulative read bytes"="bytes_sum","Read bytes"="bytes","Inter-arrival time (IAT)"="iat")),
        
    ),
    div(style = "display: flex;",
      HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'),
      selectInput("domain1", "Choose domain 1", choices = list("www.facebook.com", "twitter.com", "www.youtube.com", "www.instagram.com", "www.google.com", "www.microsoft.com", "www.linkedin.com", "www.ohchr.org", "telegram.org")),
      HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'),
      HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), 
      selectInput("domain2", "Choose domain 2", choices = list("www.youtube.com", "twitter.com", "www.facebook.com", "www.instagram.com", "www.google.com", "www.microsoft.com", "www.linkedin.com", "www.ohchr.org", "telegram.org")),
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot", height=750, width=1070)
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
      output$distPlot <- renderPlot({
        yl <- c(0,10)
        if(input$yaxis == "bytes_sum"){
          yl <-c(0,30000)
        }
        if(input$yaxis == "bytes"){
          yl <-c(0,1750)
        }
        if(input$yaxis == "iat"){
          yl <-c(0,2)
        }
        
        name = paste("datasets/2023-10-14/packets_success_",input$domain1,".csv", sep = "", collapse=NULL)
        df <- read.csv(name)
        title = paste("2023-10-14,", input$domain1, "n =", nrow(df))
        p1 <- ggplot(df, aes(x=time, y=.data[[input$yaxis]]) ) +
        stat_density_2d(aes(fill = after_stat(nlevel)), geom = "polygon") +
        scale_fill_continuous(type = "viridis") +
        scale_y_continuous(limits = yl) +
        xlim(0,2) +
        theme_bw() + 
        ggtitle(title) +
        xlab("Time [s]") + 
        ylab(labels[input$yaxis]) + 
        theme_classic() +
        guides(fill=guide_legend(title="Density (normalized)")) +
        theme(axis.title = element_text(size = 12), legend.text = element_text(size = 14), legend.title = element_text(size = 18), plot.title = element_text(size = 14))
        
        name = paste("datasets/2023-11-12/packets_success_",input$domain1,".csv", sep = "", collapse=NULL)
        df <- read.csv(name)
        
        title = paste("2023-11-12", input$domain1, ", n =", nrow(df))
        p2 <- ggplot(df, aes(x=time, y=.data[[input$yaxis]]) ) +
          stat_density_2d(aes(fill = after_stat(nlevel)), geom = "polygon") +
          scale_fill_continuous(type = "viridis") +
          scale_y_continuous(limits = yl) +
          xlim(0,2) +
          theme_bw() + 
          ggtitle(title) +
          xlab("Time [s]") + 
          ylab(labels[input$yaxis]) + 
          theme_classic() +
          guides(fill=guide_legend(title="Density (normalized)")) +
          theme(axis.title = element_text(size = 12), legend.text = element_text(size = 14), legend.title = element_text(size = 18), plot.title = element_text(size = 14))
        
        name = paste("datasets/2023-10-14/packets_success_",input$domain2,".csv", sep = "", collapse=NULL)
        df <- read.csv(name)
        title = paste("2023-10-14,", input$domain2, "n =", nrow(df))
        p3 <- ggplot(df, aes(x=time, y=.data[[input$yaxis]]) ) +
          stat_density_2d(aes(fill = after_stat(nlevel)), geom = "polygon") +
          scale_fill_continuous(type = "viridis") +
          scale_y_continuous(limits = yl) +
          xlim(0,2) +
          theme_bw() + 
          ggtitle(title) +
          xlab("Time [s]") + 
          ylab(labels[input$yaxis]) + 
          theme_classic() +
          guides(fill=guide_legend(title="Density (normalized)")) +
          theme(axis.title = element_text(size = 12), legend.text = element_text(size = 14), legend.title = element_text(size = 18), plot.title = element_text(size = 14))
        
        name = paste("datasets/2023-11-12/packets_success_",input$domain2,".csv", sep = "", collapse=NULL)
        df <- read.csv(name)
        
        title = paste("2023-11-12", input$domain2, ", n =", nrow(df))
        p4 <- ggplot(df, aes(x=time, y=.data[[input$yaxis]]) ) +
          stat_density_2d(aes(fill = after_stat(nlevel)), geom = "polygon") +
          scale_fill_continuous(type = "viridis") +
          scale_y_continuous(limits = yl) +
          xlim(0,2) +
          theme_bw() + 
          ggtitle(title) +
          xlab("Time [s]") + 
          ylab(labels[input$yaxis]) +
          theme_classic() +
          guides(fill=guide_legend(title="Density (normalized)")) +
          theme(axis.title = element_text(size = 12), legend.text = element_text(size = 14), legend.title = element_text(size = 18), plot.title = element_text(size = 14))
        
        figure <- ggarrange(
          p1 + rremove("ylab") + rremove("xlab"), 
          p3 + rremove("ylab") + rremove("xlab"),
          p2 + rremove("ylab") + rremove("xlab"),
          p4 + rremove("ylab") + rremove("xlab"),
          ncol=2, 
          nrow=2, 
          labels = NULL,
          common.legend = TRUE, 
          legend="right"
        )
      annotate_figure(figure,
                      left = text_grob(labels[input$yaxis],  size = 18, rot = 90),
                      bottom = text_grob("Time [s]", size = 18)
      )
      })
      
}

# Run the application 
shinyApp(ui = ui, server = server)
