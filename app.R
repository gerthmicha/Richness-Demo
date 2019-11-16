# Simple App to show the effect of abundance variation on various biodiversity measures. 
# Made for BIOL7002: Ecology for Conservation (Semesters 1 and 2 2019-2020) @Oxford Brookes   

library(shiny)
library(vegan)
library(MASS)
# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Species richness and diversity estimates"),
  fluidRow(
      column(3,
      sliderInput("S", "Species richness", 2, 50, 20, 1),
      sliderInput("var", "Variation in species abundance", 0.1, 10, 0.1, 0.1),
    ),

    # Show a plot of the generated distribution
        column(9,
      tabsetPanel(
        type = "tabs",
        tabPanel("Single Sample", plotOutput("Plot1")),
        tabPanel("Multiple Samples", 
                 sliderInput("N", "Number of samples (e.g., quadrats)", 2, 30, 5, 1),
                 plotOutput("Plot2"),)
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  r_comm <- reactive({
    comm <- sim_comm(input$S, input$var)
    comm
  })
  
  r_comm2 <- reactive({
      comm2 <- matrix(data = NA, nrow = input$N, ncol=input$S)
      for(i in 1:input$N){
        comm2[i,] <- sim_comm(input$S, input$var)
      }
      colnames(comm2) <- paste0("Sp", 1:input$S)
      comm2
  })

  sim_comm <- function(S, var = 1) {
    tmp <- matrix(rnegbin(S, 5, 1 / var), ncol = S, nrow = 1)
    colnames(tmp) <- paste0("Sp", 1:S)
    return(tmp)
  }
  

  output$Plot1 <- renderPlot(height = 800, {
    par(mfrow = c(2, 2), cex.main = 2.5, cex.axis = 1.5, cex.lab=1.5)
    barplot(r_comm(), main = "Species abundance")
    S <- specnumber(r_comm()) # observed number of species
    raremax <- min(rowSums(r_comm()))
    Srare <- rarefy(r_comm(), raremax)
    rarecurve(r_comm(), step = 5, sample = raremax, col = "blue", main = "Rarefaction Species Richness", ylim = c(0, input$S))
    res <- estimateR(r_comm())[c(1, 2, 4)]
    names(res) <- c("Obs", "Chao1", "ACE")
    barplot(t(res), horiz = TRUE, main = "Estimated richness", cex.names = 2.5, xlim = c(0, input$S+input$S*0.2))
    diversity(r_comm(), "simpson")
    shan1 <- diversity(r_comm(), "shannon")
    simp1 <- diversity(r_comm(), "simpson")
    indeces <- c(simp1, shan1)
    names(indeces) <- c("Simpson", "Shannon")
    barplot(indeces, horiz = TRUE, main = "Diversity indices", cex.names = 2.5)
  })
  
  output$Plot2 <- renderPlot(height = 800, {
      par(mfrow = c(2, 2), cex.main = 2.5, cex.axis = 1.5, cex.lab=1.5)
      barplot(r_comm2(), main = "Species abundance")
      S <- specnumber(r_comm2()) # observed number of species
      raremax <- min(rowSums(r_comm2()))
      Srare <- rarefy(r_comm2(), raremax)
      rarecurve(r_comm2(), step = 5, sample = raremax, col = "blue", main = "Rarefaction Species Richness", ylim = c(0, input$S))
      res <- t(specpool(r_comm2())[c(1, 2, 4, 7)])
      row.names(res) <- c("Obs", "Chao2", "JK", "BS")
      barplot(t(res), horiz = TRUE, main = "Estimated richness", cex.names = 2, xlim = c(0, input$S+input$S*0.2))
      diversity(r_comm(), "simpson")
      shan1 <- mean(diversity(r_comm2(), "shannon"))
      simp1 <- mean(diversity(r_comm2(), "simpson"))
      indeces <- c(simp1, shan1)
      names(indeces) <- c("Simpson", "Shannon")
      barplot(indeces, horiz = TRUE, main = "Diversity indices (mean)", cex.names = 2.5)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
