#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Install packages
# shiny package version 1.7.4.1
# shinydashboard version 0.7.2
# DT version 0.28
# dashboardthemes version 1.1.6

install.packages("shiny")
install.packages("shinydashboard")
install.packages("DT")
install.packages("dashboardthemes")

library(shiny)
library(shinydashboard)
library(DT)
library(dashboardthemes)

server <- function(input, output, session) {
  
  #Set reactive values for p rep group entries
  counter <- reactiveValues(n = 0)
  prevCount <- reactiveValues(n = 0)
  
  #Observes adding and removal of group entries
  observeEvent(input$add_btn, {
    counter$n <- counter$n + 1
    prevCount$n <- counter$n - 1
  })
  observeEvent(input$rm_btn, {
    if (counter$n > 0) {
      counter$n <- counter$n - 1
      prevCount$n <- counter$n + 1
    }
  })
  
  #Asks n numeric inputs based on the add and remove buttons (entry group)
  entryGroup <- reactive({
    
    n <- counter$n
    
    if (n > 0){
      
      if(prevCount$n > 0 ){
        vals = c()
        
        if (prevCount$n > n){
          lesscnt <- n
          isInc <- FALSE
        }
        else {
          lesscnt <- prevCount$n
          isInc <- TRUE
        }
        for (i in 1:lesscnt){
          inpid = paste0("textin", i)
          vals[i] = input[[inpid]]
        }
        if(isInc){
          vals <- c(vals, "Group Entry")
        }
        
        lapply(seq_len(n), function(i){
          numericInput(inputId = paste0("entryGroup", i),
                    label = paste0("Group Entry ", i), value = 1)
          
        })
      }
      else{
        lapply(seq_len(n), function(i){
          numericInput(inputId = paste0("entryGroup", i),
                    label = paste0("Group Entry ", i), value = 1)
        })
      }
    }
  })
  
  #Asks n numeric inputs based on the add and remove buttons (reps of entry group)
  repEntryGroup <- reactive({
    
    n <- counter$n
    
    if (n > 0){
      
      if(prevCount$n > 0 ){
        vals = c()
        
        if (prevCount$n > n){
          lesscnt <- n
          isInc <- FALSE
        }
        else {
          lesscnt <- prevCount$n
          isInc <- TRUE
        }
        for (i in 1:lesscnt){
          inpid = paste0("textin", i)
          vals[i] = input[[inpid]]
        }
        if(isInc){
          vals <- c(vals, "Group Entry")
        }
        
        lapply(seq_len(n), function(i){
          numericInput(inputId = paste0("repEntryGroup", i),
                       label = paste0("Replication ", i), value = 1)
        })
      }
      else{
        
        lapply(seq_len(n), function(i){
          numericInput(inputId = paste0("repEntryGroup", i),
                       label = paste0("Replication ", i), value = 1)
        })
      }    
    }
  })
  
  #converts the numeric input into a UI
  output$repGroupEntry_ui <- renderUI({ repEntryGroup() })
  output$groupEntry_ui <- renderUI({ entryGroup() })
  
  #PRep
  #waits for the action button "Save" to be clicked
  observeEvent(input$save2, {
    
    #Computes for the total number of entries based on group entry inputs
    output$totalEnt <- renderTable({
      
      totalP <- 0
      totalE <- 0
      n <- counter$n
      #isolate prevents reactions before clicking the action button
      #stores values into a vector
      ent <- sapply(1:n, function(i) {isolate(input[[paste0("entryGroup", i)]])})
      reps <- sapply(1:n, function(i) {isolate(input[[paste0("repEntryGroup", i)]])})
      for (i in 1:length(ent)){
        totalP <- as.integer(totalP + (ent[i]*reps[i]))
        totalE <- as.integer(totalE + (ent[i]))
      }
      
      table <- data.frame(totalE, totalP)
      names(table) <- c("Total Entries", "Total Plots")
      table
    })
    
    #Computes and prints a table for the possible row and column divisions
    output$factorPRep <- renderTable({
      
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
      total <- 0
      n <- counter$n
      ent <- sapply(1:n, function(i) {isolate(input[[paste0("entryGroup", i)]])})
      reps <- sapply(1:n, function(i) {isolate(input[[paste0("repEntryGroup", i)]])})
      for (i in 1:length(ent)){
        total <- total + (ent[i]*reps[i])
      }
      get_factors(total)
    })
  })

  #Alpha-Lattice
  #waits for the action button "Save" to be clicked  
  observeEvent(input$save1, {
    
    #Computes and prints a table for the possible row and column divisions
    #Computes for the possible block sizes with respect to the number of columns
    output$factor <- renderTable({
      
      get_factors <- function(x){
        f <- matrix(nrow = 0, ncol = 3)
        f <- data.frame(f)
        for (i in 1:x){
          if ((x %% i) == 0){
            f[nrow(f)+1, 1] <- i
            f[nrow(f), 2] <- x/i
          }
        }
        names(f) <- c("Rows", "Columns", "Possible Block Size")
        f[,2] <- as.integer(f[,2])
        for (i in 1:nrow(f)){
          for (j in 1:f[i,2]){
            if ((f[i,2] %% j) == 0){
              if (is.na(f[i,3]) == TRUE){
                f[i,3] <- as.character(j)
              }
              else {
                f[i,3] <- paste0(f[i,3], ", ", j)
              }
            }
          }
        }
        f
      }
      get_factors(isolate(input$entry*input$rep))
    })
  })
  
  #Direct Seeded
  observeEvent(input$save3, {
    
    output$fldA1 <- renderTable({
      L <- as.integer(isolate(input$len*input$fL))
      W <- as.integer(isolate(input$wdt*input$fW))
      Area <- L*W
      table <- data.frame(Area)
      names(table) <- c("Area (in square meters)")
      table
    })
    
    output$pltA1 <- renderTable({
      L <- isolate(input$rL)
      W <- isolate(input$pSRD*input$pRD)
      Area <- L*W
      table <- data.frame(Area)
      names(table) <- c("Plot Area (in square meters)")
      table
    })
    
    output$fld1 <- renderTable({
      L <- isolate(input$len*input$fL)
      W <- isolate(input$wdt*input$fW)
      PL <- isolate(input$rL)
      PW <- isolate(input$pSRD*input$pRD)
      FR <- as.integer(W/PL)
      FC <- as.integer(L/PW)
      table <- data.frame(FR, FC)
      names(table) <- c("Field Rows", "Field Columns")
      table
    })
  })
  
  #Transplanted
  observeEvent(input$save4, {
    
    output$fldA2 <- renderTable({
      L <- isolate(input$len*input$fL)
      W <- isolate(input$wdt*input$fW)
      Area <- L*W
      table <- data.frame(Area)
      names(table) <- c("Area (in square meters)")
      table
    })
    
    output$pltA2 <- renderTable({
      L <- isolate(input$pSC*input$pC)
      W <- isolate(input$pSRT*input$pRT)
      Area <- L*W
      table <- data.frame(Area)
      names(table) <- c("Plot Area (in square meters)")
      table
    })
    
    output$fld2 <- renderTable({
      L <- isolate(input$len*input$fL)
      W <- isolate(input$wdt*input$fW)
      PL <- isolate(input$pSC*input$pC)
      PW <- isolate(input$pSRT*input$pRT)
      FR <- as.integer(W/PL)
      FC <- as.integer(L/PW)
      table <- data.frame(FR, FC)
      names(table) <- c("Field Rows", "Field Columns")
      table
    })
  })
  
  #Choices for Direct Seeded
  observeEvent(input$save3, {
    if(input$design == "Alpha-Lattice"){
      x <- isolate(input$entry*input$rep)
      f <- matrix(nrow = 0, ncol = 2)
      f <- data.frame(f)
      for (i in 1:x){
        if ((x %% i) == 0){
          f[nrow(f)+1, 1] <- as.integer(i)
          f[nrow(f), 2] <- as.integer(x/i)
        }
      }
      names(f) <- c("Rows", "Columns")
      
      L <- isolate(input$len*input$fL)
      W <- isolate(input$wdt*input$fW)
      PL <- isolate(input$rL)
      PW <- isolate(input$pSRD*input$pRD)
      FR <- as.integer(W/PL)
      FC <- as.integer(L/PW)
      
      c <- c()
      for (i in 1:nrow(f)){
        if(f[i,1] <= FR && f[i,2] <= FC){
          c[length(c)+1] <- paste0(f[i,1], ", ", f[i,2])
        }
      }
      
      output$Choices <- renderUI({
        selectInput("choice", "Select number of row and column",
                    choices = c("(none)", sapply(1:length(c), function(i) {c[i]})),
                    selected = "(none)"
        )
      }) 
    }
    else if (input$design == "Partially Replicated"){
      x <- 0
      n <- counter$n
      ent <- sapply(1:n, function(i) {isolate(input[[paste0("entryGroup", i)]])})
      reps <- sapply(1:n, function(i) {isolate(input[[paste0("repEntryGroup", i)]])})
      for (i in 1:length(ent)){
        x <- x + (ent[i]*reps[i])
      }
      f <- matrix(nrow = 0, ncol = 2)
      f <- data.frame(f)
      for (i in 1:x){
        if ((x %% i) == 0){
          f[nrow(f)+1, 1] <- i
          f[nrow(f), 2] <- x/i
        }
      }
      names(f) <- c("Rows", "Columns")
      
      L <- isolate(input$len*input$fL)
      W <- isolate(input$wdt*input$fW)
      PL <- isolate(input$rL)
      PW <- isolate(input$pSRD*input$pRD)
      FR <- as.integer(W/PL)
      FC <- as.integer(L/PW)
      
      c <- c()
      for (i in 1:nrow(f)){
        if(f[i,1] <= FR && f[i,2] <= FC){
          c[length(c)+1] <- paste0(f[i,1], ", ", f[i,2])
        }
      }
      
      output$Choices <- renderUI({
        selectInput("choice", "Select number of row and column",
                    choices = c("(none)", sapply(1:length(c), function(i) {c[i]})),
                    selected = "(none)"
        )
      })
    }
  })
  
  #Choices for Transplanted
  observeEvent(input$save4, {
    if(input$design == "Alpha-Lattice"){
      x <- isolate(input$entry*input$rep)
      f <- matrix(nrow = 0, ncol = 2)
      f <- data.frame(f)
      for (i in 1:x){
        if ((x %% i) == 0){
          f[nrow(f)+1, 1] <- i
          f[nrow(f), 2] <- x/i
        }
      }
      names(f) <- c("Rows", "Columns")
      
      L <- isolate(input$len*input$fL)
      W <- isolate(input$wdt*input$fW)
      PL <- isolate(input$pSC*input$pC)
      PW <- isolate(input$pSRT*input$pRT)
      FR <- as.integer(W/PL)
      FC <- as.integer(L/PW)
      
      c <- c()
      for (i in 1:nrow(f)){
        if(f[i,1] <= FR && f[i,2] <= FC){
          c[length(c)+1] <- paste0(f[i,1], ", ", f[i,2])
        }
      }
      
      output$Choices <- renderUI({
        selectInput("choice", "Select number of row and column",
                    choices = c("(none)", sapply(1:length(c), function(i) {c[i]})),
                    selected = "(none)"
        )
      })
    }
    else if(input$design == "Partially Replicated"){
      x <- 0
      n <- counter$n
      ent <- sapply(1:n, function(i) {isolate(input[[paste0("entryGroup", i)]])})
      reps <- sapply(1:n, function(i) {isolate(input[[paste0("repEntryGroup", i)]])})
      for (i in 1:length(ent)){
        x <- x + (ent[i]*reps[i])
      }
      f <- matrix(nrow = 0, ncol = 2)
      f <- data.frame(f)
      for (i in 1:x){
        if ((x %% i) == 0){
          f[nrow(f)+1, 1] <- i
          f[nrow(f), 2] <- x/i
        }
      }
      names(f) <- c("Rows", "Columns")

      L <- isolate(input$len*input$fL)
      W <- isolate(input$wdt*input$fW)
      PL <- isolate(input$pSC*input$pC)
      PW <- isolate(input$pSRT*input$pRT)
      FR <- as.integer(W/PL)
      FC <- as.integer(L/PW)
      
      c <- c()
      for (i in 1:nrow(f)){
        if(f[i,1] <= FR && f[i,2] <= FC){
          c[length(c)+1] <- paste0(f[i,1], ", ", f[i,2])
        }
      }
      
      output$Choices <- renderUI({
        selectInput("choice", "Select number of row and column",
                    choices = c("(none)", sapply(1:length(c), function(i) {c[i]})),
                    selected = "(none)"
        )
      }) 
    }
  })
  
  #Layout for non replicated entries
  observeEvent(input$save6, {
    row <- as.integer(sub(",.*", "", isolate(input[["choice"]])))
    col <- as.integer(sub(".*, ", "", isolate(input[["choice"]])))
    
    output$approx1 <- renderTable({
      R <- row
      C <- col
      if (input$est == "Direct-seeded"){
        PL <- isolate(input$rL)
        PW <- isolate(input$pSRD*input$pRD)
      } else {
        PL <- isolate(input$pSC*input$pC)
        PW <- isolate(input$pSRT*input$pRT)
      }
      L <- C*PL
      W <- R*PW
      table <- data.frame(L,W)
      names(table) <- c("Approx. Length", "Approx. Width")
      table
    })
    
    output$Layout1 <- renderDataTable({
      R <- row
      C <- col
      table1 <- data.frame()
      table2 <- data.frame()
      #Left-to_Right
      for (i in 1:R){
        for (j in 1:C){
          if (i == 1 & j == 1){
            table1[i,j] <- as.integer(1)
          }
          else if(j == 1){
            table1[i,j] <- as.integer(table1[i-1,C] + 1)
          }
          else {
            table1[i,j] <- as.integer(table1[i,j-1] + 1)
          }
        }
      }
      #Row-Serpentine
      for (i in 1:R){
        for (j in 1:C){
          if (i == 1 && j == 1){
            table2[i,j] <- as.integer(1)
          }
          else if (i %% 2 != 0){
            if(i != 1 && j == 1){
              table2[i,j] <- as.integer(table2[i-1,1] + 1)
            }
            else {
              table2[i,j] <- as.integer(table2[i,j-1] + 1)
            }
          }
          else {
            if(j == 1){
              table2[i,j] <- as.integer(C*i)
            }
            else{
              table2[i,j] <- as.integer(table2[i,j-1] - 1)
            }
          }
        }
      }
      
      RN1 <- c()
      CN1 <- c()
      a <- 1
      b <- 1
      for(i in 1:ncol(table1)){
        CN1[i] <- paste0("col ", a)
        a <- a + 1
      }
      for(i in 1:nrow(table1)){
        RN1[i] <- paste0("row ", b)
        b <- b + 1
      }
      
      names(table1) <- CN1
      rownames(table1) <- RN1
      
      RN2 <- c()
      CN2 <- c()
      c <- 1
      d <- 1
      for(i in 1:ncol(table2)){
        CN2[i] <- paste0("col ", c)
        c <- c + 1
      }
      for(i in 1:nrow(table2)){
        RN2[i] <- paste0("row ", d)
        d <- d + 1
      }
      
      names(table2) <- CN2
      rownames(table2) <- RN2
      
      if (isolate(input$pOrder == "Row-Serpentine")){
        table2
      } else if (isolate(input$pOrder == "Left-to-Right")){
        table1
      }
    },
    options = list(
      paging = FALSE,
      dom = 't',
      ordering = F))
  })
  
  #Layout for replicated entries
  observeEvent(input$save5, {
    row <- as.integer(sub(",.*", "", isolate(input[["choice"]])))
    col <- as.integer(sub(".*, ", "", isolate(input[["choice"]])))
    repNum <- as.integer(isolate(input$rep))
    
    output$approx2 <- renderTable({
      R <- row
      C <- col
      if (input$est == "Direct-seeded"){
        PL <- isolate(input$rL)
        PW <- isolate(input$pSRD*input$pRD)
      } else {
        PL <- isolate(input$pSC*input$pC)
        PW <- isolate(input$pSRT*input$pRT)
      }
      L <- C*PL
      W <- R*PW
      table <- data.frame(L,W)
      names(table) <- c("Approx. Length", "Approx. Width")
      table
    })
    
    output$Layout2 <- renderDataTable({
      R <- row
      C <- col
      N <- repNum
      table1 <- data.frame()
      table2 <- data.frame()
      table3 <- data.frame()
      table4 <- data.frame()
      
      #Serpentine, left-to-right
      k <- 1
      for (i in 1:R){
        for (j in 1:(C+(N-1))){
          if (j == 1 && k == N){
            k <- 1
          }
          if (i %% 2 != 0){
            if (j <= (C/N)){
              if (i == 1 && j == 1){
                table1[i,j] <- as.integer(1)
              }
              else if (j == 1){
                table1[i,j] <- as.integer(table1[i-1,j] + 1)
              }
              else {
                table1[i,j] <- as.integer(table1[i,j-1] + 1)
              }
            }
            else if (j == ((k*(C/N))+k)){
              table1[1:R,j] <- as.integer(0)
              k <- k + 1
            }
            else if (j <= ((k*(C/N))+(k-1)) && j >= (((k-1)*(C/N))+k)){
              if (i == 1 && j == (((k-1)*(C/N))+k)){
                table1[i,j] <- as.integer((table1[i,(C/N)]*(R*(k-1))) + 1)
              }
              else if (j == (((k-1)*(C/N))+k)){
                table1[i,j] <- as.integer(table1[i-1,j] + 1)
              }
              else {
                table1[i,j] <- as.integer(table1[i,j-1] + 1)
              }
            } 
          }
          else {
            if (j <= (C/N)){
              if (j == 1){
                table1[i,j] <- as.integer((i*(C/N)))
              }
              else {
                table1[i,j] <- as.integer(table1[i,j-1] - 1)
              }
            }
            else if (j == ((k*(C/N))+k)){
              table1[1:R,j] <- as.integer(0)
              k <- k + 1
            }
            else if (j <= ((k*(C/N))+(k-1)) && j >= (((k-1)*(C/N))+k)){
              if (j == (((k-1)*(C/N))+k)){
                table1[i,j] <- as.integer((i+(R*(k-1)))*(C/N))
              }
              else {
                table1[i,j] <- as.integer(table1[i,j-1] - 1)
              }
            }
          }
        }
      }
      
      #Left-to-Right, Left-to-Right
      l <- 1
      for (i in 1:R){
        for (j in 1:(C+(N-1))){
          if (j == 1 && l == N){
            l <- 1
          }
          if (j <= (C/N)){
            if (i == 1 && j == 1){
              table2[i,j] <- as.integer(1)
            }
            else if (j == 1){
              table2[i,j] <- as.integer(table2[i-1,(C/N)] + 1)
            }
            else {
              table2[i,j] <- as.integer(table2[i,j-1] + 1)
            }
          }
          else if (j == ((l*(C/N))+l)){
            table2[1:R,j] <- as.integer(0)
            l <- l + 1
          }
          else if (j <= ((l*(C/N))+(l-1)) && j >= (((l-1)*(C/N))+l)){
            if (i == 1 && j == (((l-1)*(C/N))+l)){
              table2[i,j] <- as.integer((table2[i,(C/N)]*(R*(l-1))) + 1)
            }
            else if (j == (((l-1)*(C/N))+l)){
              table2[i,j] <- as.integer(table2[i-1,((l*(C/N))+(l-1))] + 1)
            }
            else {
              table2[i,j] <- as.integer(table2[i,j-1] + 1)
            }
          }
        }
      }
      
      #Serpentine, Top-to-Bottom
      m <- 1
      if ((R/N)%%2 != 0){
        for (i in 1:(R+(N-1))){
          for (j in 1:C){
            #First obs
            if (i == 1 && j == 1){
              table3[i,j] <- as.integer(1)
            } else if (i == ((R/N)*m) + m && j == C){
              #partitions
              table3[i,1:C] <- as.integer(0)
              m <- m + 1
            } else if (m != 1 && j == 1 && i == (((m-1)*(R/N))+m)){
              #first obs after partition
              table3[i,j] <- as.integer(table3[i-2,C] + 1)
            } else if (m%%2 == 0 && (i-m)%%2 != 0 && j == 1){
              #first column on odd number row after rep 1
              table3[i,j] <- as.integer(table3[i-1,1] + 1)
            } else if (m%%2 == 0 && j == 1){
              #first column on even number row after rep 1
              table3[i,j] <- as.integer(table3[i-1,C] + C)
            } else if (m%%2 != 0 && (i-m)%%2 == 0 && j == 1){
              #first column on odd number row after rep 1
              table3[i,j] <- as.integer(table3[i-1,1] + 1)
            } else if (m%%2 != 0 && j == 1){
              #first column on even number row after rep 1
              table3[i,j] <- as.integer(table3[i-1,C] + C)
            } else if (m == 1 && i%%2 == 0 && j == 1){
              #first column on even number row of rep 1
              table3[i,j] <- as.integer(table3[i-1,C] + C)
            } else if (m == 1 && j == 1){
              #first column on odd number row of rep 1
              table3[i,j] <- as.integer(table3[i-1,1] + 1)
            } else if(m == 1 && i%%2 == 0){
              #obs of even number row of rep 1
              table3[i,j] <- as.integer(table3[i,j-1] - 1)
            } else if(m%%2 == 0 && (i-m)%%2 == 0){
              #obs of even number row after rep 1
              table3[i,j] <- as.integer(table3[i,j-1] - 1)
            } else if(m%%2 != 0 && (i-m)%%2 != 0){
              #obs of even number row after rep 1
              table3[i,j] <- as.integer(table3[i,j-1] - 1)
            } else {
              table3[i,j] <- as.integer(table3[i,j-1] + 1)
            }
          }
        }
      } else {
        for (i in 1:(R+(N-1))){
          for (j in 1:C){
            if (i == 1 && j == 1){
              table3[i,j] <- as.integer(1)
            } else if (i == ((R/N)*m)+m && j == C){
              table3[i,1:C] <- as.integer(0)
              m <- m + 1
            } else if (m != 1 && j == 1 && i == ((R/N)*(m-1))+m){
              table3[i,j] <- as.integer(table3[i-2,1] + 1)
            } else if (j == 1 && (i-m)%%2 != 0){
              table3[i,j] <- as.integer(table3[i-1,C] + C)
            } else if (j == 1){
              table3[i,j] <- as.integer(table3[i-1,1] + 1)
            } else if ((i-m)%%2 != 0){
              table3[i,j] <- as.integer(table3[i,j-1] - 1)
            } else {
              table3[i,j] <- as.integer(table3[i,j-1] + 1)
            }
          }
        }
      }
      
      #Left-to-Right, Top-to-Bottom
      o <- 1
      for (i in 1:(R+(N-1))){
        for (j in 1:C){
          if (i == 1 && j == 1){
            table4[i,j] <- as.integer(1)
          }
          else if (i == ((o*(R/N))+o)){
            table4[i,1:C] <- as.integer(0)
          }
          else if (i == ((o*(R/N))+o)+1 && j == 1){
            table4[i,j] <- as.integer(table4[i-2,C] + 1)
            o <- o + 1
          }
          else if(j == 1){
            table4[i,j] <- as.integer(table4[i-1,C] + 1)
          }
          else {
            table4[i,j] <- as.integer(table4[i,j-1] + 1)
          }
        }
      }
      
      for (i in 1:ncol(table1)){
        table1[,i] <- as.character(table1[,i])
      }
      for (i in 1:ncol(table2)){
        table2[,i] <- as.character(table2[,i])
      }
      for (i in 1:ncol(table3)){
        table3[,i] <- as.character(table3[,i])
      }
      for (i in 1:ncol(table4)){
        table4[,i] <- as.character(table4[,i])
      }
      for (i in 1:nrow(table1)){
        for (j in 1:ncol(table1)){
          if (table1[i,j] == "0"){
            table1[i,j] <- "**"
          }
        }
      }
      for (i in 1:nrow(table2)){
        for (j in 1:ncol(table2)){
          if (table2[i,j] == "0"){
            table2[i,j] <- "**"
          }
        }
      }
      for (i in 1:nrow(table3)){
        for (j in 1:ncol(table3)){
          if (table3[i,j] == "0"){
            table3[i,j] <- "**"
          }
        }
      }
      for (i in 1:nrow(table4)){
        for (j in 1:ncol(table4)){
          if (table4[i,j] == "0"){
            table4[i,j] <- "**"
          }
        }
      }
      
      RN1 <- c()
      CN1 <- c()
      a <- 1
      b <- 1
      x <- 1
      for(i in 1:ncol(table1)){
        if(table1[1,i] == "**"){
          CN1[i] <- paste0("partition ", x)
          x <- x + 1
        }
        else {
          CN1[i] <- paste0("col ", a)
          a <- a + 1
        }
      }
      for(i in 1:nrow(table1)){
        RN1[i] <- paste0("row ", b)
        b <- b + 1
      }
      
      names(table1) <- CN1
      rownames(table1) <- RN1
      
      RN2 <- c()
      CN2 <- c()
      c <- 1
      d <- 1
      y <- 1
      for(i in 1:ncol(table2)){
        if(table2[1,i] == "**"){
          CN2[i] <- paste0("partition ", y)
          y <- y + 1
        }
        else {
          CN2[i] <- paste0("col ", c)
          c <- c + 1
        }
      }
      for(i in 1:nrow(table2)){
        RN2[i] <- paste0("row ", d)
        d <- d + 1
      }
      
      names(table2) <- CN2
      rownames(table2) <- RN2
      
      RN3 <- c()
      CN3 <- c()
      e <- 1
      f <- 1
      z <- 1
      for(i in 1:nrow(table3)){
        if(table3[i,1] == "**"){
          RN3[i] <- paste0("partition ", z)
          z <- z + 1
        }
        else {
          RN3[i] <- paste0("row ", e)
          e <- e + 1
        }
      }
      for(i in 1:ncol(table3)){
        CN3[i] <- paste0("col ", f)
        f <- f + 1
      }
      
      names(table3) <- CN3
      rownames(table3) <- RN3
      
      RN4 <- c()
      CN4 <- c()
      g <- 1
      h <- 1
      w <- 1
      for(i in 1:nrow(table4)){
        if(table4[i,1] == "**"){
          RN4[i] <- paste0("partition ", w)
          w <- w + 1
        }
        else {
          RN4[i] <- paste0("row ", g)
          g <- g + 1
        }
      }
      for(i in 1:ncol(table4)){
        CN4[i] <- paste0("col ", h)
        h <- h + 1
      }
      
      names(table4) <- CN4
      rownames(table4) <- RN4
      
      if (isolate(input$pOrder == "Row-Serpentine") && 
          isolate(input$rOrder == "Left-to-Right"))
      {
        table1
      } else if (isolate(input$pOrder == "Left-to-Right") &&
                 isolate(input$rOrder == "Left-to-Right"))        
      {
        table2
      }
      else if (isolate(input$pOrder == "Row-Serpentine") && 
               isolate(input$rOrder == "Top-to-Bottom"))
      {
        table3
      } else if (isolate(input$pOrder == "Left-to-Right") &&
                 isolate(input$rOrder == "Top-to-Bottom"))        
      {
        table4
      }
      
      
    },
    options = list(
      paging = FALSE,
      dom = 't',
      ordering = F))
  })
}

