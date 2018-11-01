#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(dplyr)
library(knitr)
library(readxl)
library(scales)

cycle <- read_excel("hitforcycle copy.xls") %>% 
  mutate(League = case_when(Tm %in% (c("ANA", "BAL", "BOS", "CAL", "CHW", "CLE", "DET", "KCR", "LAA", "MIN", "NYY", "OAK", "SEA", "TBR", "TEX", "WSA", "TOR", "SLB", "PHA")) ~ "American",
                            Tm %in% (c("ARI", "ATL", "BRO", "CHC", "CIN", "COL", "LAD", "MON", "NYG", "NYM", "PHI", "PIT", "SDP", "SFG", "STL", "WSH", "WSN", "PBS", "BSN")) ~ "National",
                            #Two teams switched leagues: Houston and Milwaukee. I filter accordingly.
                            Tm == "HOU" || Date >40000 ~ "American",
                            Tm == "HOU" || Date <40000 ~ "National",
                            Tm == "MIL" || Date >35000~ "National",
                            Tm == "MIL" || Date <35000~ "American"))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Final Project Shiny Proof of Concept"),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      ggplot(cycle, aes(x=BOP, y=WPA, color=League)) +
        geom_point() +
        geom_jitter() +
        scale_x_continuous(breaks=pretty_breaks(9)) +
        ylab("Win Probability Added") +
        xlab("Batting Order Position") +
        geom_smooth() +
        ggtitle("Batting Order Has Little Impact on Win Probability", subtitle = "Of Players Who Hit for the Cycle")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

