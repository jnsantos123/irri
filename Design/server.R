#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server
function(input, output, session) {
  
  output$factor <- renderTable({
    
    get_factors <- function(x){
      f <- matrix(nrow = 0, ncol = 2)
      f <- data.frame(f)
      for (i in 1:x){
        if ((x %% i) == 0){
          f[nrow(f)+1, 1] <- i
          f[nrow(f), 2] <- x/i
        }
      }
      names(f) <- c("Rows", "Columns")
      f[,2] <- as.integer(f[,2])
      f
    }
    get_factors(input$entry*input$rep)    
  })
  
  output$Area <- renderText({
    L <- input$len
    W <- input$wdt
    
    Area <- L*W
    Area
  })
  
  output$PlantCF <- renderTable({
    L <- input$len
    W <- input$wdt
    SR <- input$plntspcR
    SC <- input$plntspcC
    NPR <- L/SR
    NPC <- W/SC
    NPF <- NPR*NPC
    head <- c("Row", "Column", "Field")
    value <- c(NPR, NPC, NPF)
    value <- as.integer(value)
    table <- data.frame(head, value)
    names(table) <- NULL
    table
  })
  
  output$PDim <- renderTable({
    PlotR <- input$plntR
    PlotC <- input$plntC
    SR <- input$plntspcR
    SC <- input$plntspcC
    PlotL <- PlotR*SR
    PlotW <- PlotC*SC
    Area <- PlotL*PlotW
    head <- c("Length (m)", "Width (m)", "Area (m^2)")
    value <- c(PlotL, PlotW, Area)
    table <- data.frame(head, value)
    names(table) <- NULL
    table
    
  })
  
  output$Without <- renderTable({
    L <- input$len
    W <- input$wdt
    SR <- input$plntspcR
    SC <- input$plntspcC
    NPR <- L/SR
    NPC <- W/SC
    PlotR <- input$plntR
    PlotC <- input$plntC
    R <- NPR/PlotR
    C <- NPC/PlotC
    Fi <- R*C
    head <- c("Row", "Column", "Field")
    value <- c(R, C, Fi)
    value <- as.integer(value)
    table <- data.frame(head, value)
    names(table) <- NULL
    table
  })
  
  output$With <- renderTable({
    L <- input$len
    W <- input$wdt
    SR <- input$plntspcR
    SC <- input$plntspcC
    NPR <- L/SR
    NPC <- W/SC
    PlotR <- input$plntR
    PlotC <- input$plntC
    R <- NPR/(PlotR+1)
    C <- NPC/(PlotC+1)
    Fi <- R*C
    head <- c("Row", "Column", "Field")
    value <- c(R, C, Fi)
    value <- as.integer(value)
    table <- data.frame(head, value)
    names(table) <- NULL
    table
  })
  
  #Plot:Left-to-right, No replication
  output$Layout1 <- renderTable({
    R <- input$row
    C <- input$column
    table <- data.frame()
    for (i in 1:R){
      for (j in 1:C){
        if (i == 1 & j == 1){
          table[i,j] <- as.integer(1)
        }
        else if(j == 1){
          table[i,j] <- as.integer(table[i-1,C] + 1)
        }
        else {
          table[i,j] <- as.integer(table[i,j-1] + 1)
        }
      }
    }
    names(table) <- NULL
    table
  })
  
  #Plot:Serpentine, No replication
  output$Layout2 <- renderTable({
    R <- input$row
    C <- input$column
    table <- data.frame()
    for (i in 1:R){
      for (j in 1:C){
        if (i == 1 && j == 1){
          table[i,j] <- as.integer(1)
        }
        else if (i %% 2 != 0){
          if(i != 1 && j == 1){
            table[i,j] <- as.integer(table[i-1,1] + 1)
          }
          else {
            table[i,j] <- as.integer(table[i,j-1] + 1)
          }
        }
        else {
          if(j == 1){
            table[i,j] <- as.integer(C*i)
          }
          else{
            table[i,j] <- as.integer(table[i,j-1] - 1)
          }
        }
      }
    }
    names(table) <- NULL
    table
  })

  #Plot: Left-to Right, w/Replication: Top-to-Bottom
  output$Layout3 <- renderTable({
    R <- input$row
    C <- input$column
    N <- input$numRep
    table <- data.frame()
    k <- 1
    for (i in 1:(R+(N-1))){
      for (j in 1:C){
        if (i == 1 && j == 1){
          table[i,j] <- as.integer(1)
        }
        else if (i == ((k*(R/N))+k)){
          table[i,1:C] <- as.integer(0)
        }
        else if (i == ((k*(R/N))+k)+1 && j == 1){
          table[i,j] <- as.integer(table[i-2,C] + 1)
          k <- k + 1
        }
        else if(j == 1){
          table[i,j] <- as.integer(table[i-1,C] + 1)
        }
        else {
          table[i,j] <- as.integer(table[i,j-1] + 1)
        }
      }
    }
    names(table) <- NULL
    table
  })
  
  #Plot: Serpentine, w/Replication: Top-to-Bottom
  output$Layout4 <- renderTable({
    R <- input$row
    C <- input$column
    N <- input$numRep
    table <- data.frame()
    k <- 1
    for (i in 1:(R+(N-1))){
      for (j in 1:C){
        if (i == 1 && j == 1){
          table[i,j] <- as.integer(1)
        }
        else if (i == ((k*(R/N))+k)){
          table[i,1:C] <- as.integer(0)
        }
        else if (i == ((k*(R/N))+k)+1 && j == 1){
          table[i,j] <- as.integer((k*(R/N))*C + 1)
          k <- k + 1
        }
        else if (k == 1){
          if (i %% 2 == 0){
            if (j == 1){
              table[i,j] <- as.integer(C*i)
            }
            else {
              table[i,j] <- as.integer(table[i,j-1] - 1)
            }
          }
          else {
            if (j == 1){
              table[i,j] <- table[i,j] <- as.integer(table[i-1,1] + 1)
            }
            else {
              table[i,j] <- as.integer(table[i,j-1] + 1)
            }
          }
        }
        else if (k %% 2 != 0){
          if (i %% 2 == 0){
            if (j == 1){
              table[i,j] <- as.integer(C*(i-(k-1)))
            }
            else {
              table[i,j] <- as.integer(table[i,j-1] - 1)
            }
          }
          else {
            if (j == 1){
              table[i,j] <- table[i,j] <- as.integer(table[i-1,1] + 1)
            }
            else {
              table[i,j] <- as.integer(table[i,j-1] + 1)
            }
          }
        }
        else {
          if (i %% 2 != 0){
            if (j == 1){
              table[i,j] <- as.integer(C*(i-(k-1)))
            }
            else {
              table[i,j] <- as.integer(table[i,j-1] - 1)
            }
          }
          else {
            if (j == 1){
              table[i,j] <- table[i,j] <- as.integer(table[i-1,1] + 1)
            }
            else {
              table[i,j] <- as.integer(table[i,j-1] + 1)
            }
          }
        }
      }
    }
    names(table) <- NULL
    table
  })
  
  #Plot: Left-to Right, w/Replication: Left-to-Right
  output$Layout5 <- renderTable({
    R <- input$row
    C <- input$column
    N <- input$numRep
    k <- 1
    table <- data.frame()
    for (i in 1:R){
      for (j in 1:(C+(N-1))){
        if (j == 1 && k == N){
          k <- 1
        }
        if (j <= (C/N)){
          if (i == 1 && j == 1){
            table[i,j] <- as.integer(1)
          }
          else if (j == 1){
            table[i,j] <- as.integer(table[i-1,(C/N)] + 1)
          }
          else {
            table[i,j] <- as.integer(table[i,j-1] + 1)
          }
        }
        else if (j == ((k*(C/N))+k)){
          table[1:R,j] <- as.integer(0)
          k <- k + 1
        }
        else if (j <= ((k*(C/N))+(k-1)) && j >= (((k-1)*(C/N))+k)){
          if (i == 1 && j == (((k-1)*(C/N))+k)){
            table[i,j] <- as.integer((table[i,(C/N)]*(R*(k-1))) + 1)
          }
          else if (j == (((k-1)*(C/N))+k)){
            table[i,j] <- as.integer(table[i-1,((k*(C/N))+(k-1))] + 1)
          }
          else {
            table[i,j] <- as.integer(table[i,j-1] + 1)
          }
        }
      }
    }
    names(table) <- NULL
    table
  })

  #Plot: Serpentine, w/Replication: Left-to-Right
  output$Layout6 <- renderTable({
    R <- input$row
    C <- input$column
    N <- input$numRep
    k <- 1
    table <- data.frame()
    for (i in 1:R){
      for (j in 1:(C+(N-1))){
        if (j == 1 && k == N){
          k <- 1
        }
        if (i %% 2 != 0){
          if (j <= (C/N)){
            if (i == 1 && j == 1){
              table[i,j] <- as.integer(1)
            }
            else if (j == 1){
              table[i,j] <- as.integer(table[i-1,j] + 1)
            }
            else {
              table[i,j] <- as.integer(table[i,j-1] + 1)
            }
          }
          else if (j == ((k*(C/N))+k)){
            table[1:R,j] <- as.integer(0)
            k <- k + 1
          }
          else if (j <= ((k*(C/N))+(k-1)) && j >= (((k-1)*(C/N))+k)){
            if (i == 1 && j == (((k-1)*(C/N))+k)){
              table[i,j] <- as.integer((table[i,(C/N)]*(R*(k-1))) + 1)
            }
            else if (j == (((k-1)*(C/N))+k)){
              table[i,j] <- as.integer(table[i-1,j] + 1)
            }
            else {
              table[i,j] <- as.integer(table[i,j-1] + 1)
            }
          } 
        }
        else {
          if (j <= (C/N)){
            if (j == 1){
              table[i,j] <- as.integer((i*(C/N)))
            }
            else {
              table[i,j] <- as.integer(table[i,j-1] - 1)
            }
          }
          else if (j == ((k*(C/N))+k)){
            table[1:R,j] <- as.integer(0)
            k <- k + 1
          }
          else if (j <= ((k*(C/N))+(k-1)) && j >= (((k-1)*(C/N))+k)){
            if (j == (((k-1)*(C/N))+k)){
              table[i,j] <- as.integer((i+(R*(k-1)))*(C/N))
            }
            else {
              table[i,j] <- as.integer(table[i,j-1] - 1)
            }
          }
        }
      }
    }
    names(table) <- NULL
    table
  })
  
}