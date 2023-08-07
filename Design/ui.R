#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application
fluidPage(

    # Application title
    titlePanel("Pre-Experiment Planning"),

    
    # Sidebar for inputs
    sidebarLayout(
      
      sidebarPanel(
        
        selectInput("feature", "Feature:",
                    choices = c("Field", "Layout")),
        
        conditionalPanel(
          
          condition = "input.feature == 'Field'",
          
          numericInput("entry", "Input number of entries:",
                       value = 1,
                       min = 1,
                       max = 100000,
                       step = 10),
          
          numericInput("rep", "Input number of replications:",
                       value = 1,
                       min = 1,
                       max = 1000,
                       step = 1),
          
          checkboxInput("field", "Check field dimensions",
                        value = FALSE),
          
          conditionalPanel(
            
            condition = "input.field == true",
            
            numericInput("len", "Input Field Length (in meters)",
                         value = 100,
                         min = 1,
                         max = 10000,
                         step = 10),
            
            numericInput("wdt", "Input Field Width (in meters)",
                         value = 100,
                         min = 1,
                         max = 10000,
                         step = 10),
            
            numericInput("plntspcR", "Input Plant Row Spacing (in meters)",
                         value = 0.2,
                         min = 0.01,
                         max = 2,
                         step = 0.05),
            
            numericInput("plntspcC", "Input Plant Column Spacing (in meters)",
                         value = 0.2,
                         min = 0.01,
                         max = 2,
                         step = 0.05),
            
            numericInput("plntR", "Input Number of Plants in a Row (per plot)",
                         value = 1,
                         min = 1,
                         max = 1000,
                         step = 1),
            
            numericInput("plntC", "Input Number of Plants in a Column (per plot)",
                         value = 1,
                         min = 1,
                         max = 1000,
                         step = 1),
            
            checkboxInput("hllbtwplt", "Hill between plots",
                          value = FALSE),
          ),
          
        ),
        
        conditionalPanel(
          
          condition = "input.feature == 'Layout'",
          
          numericInput("row", "Input Number of Plot Rows",
                       value = 1,
                       min = 1,
                       max = 1000,
                       step = 10),
          
          numericInput("column", "Input Number of Plot Columns",
                       value = 1,
                       min = 1,
                       max = 1000,
                       step = 10),
          
          checkboxInput("order", "Plot Order: Serpentine", 
                        value = FALSE),
          checkboxInput("repLog", "Replication",
                        value = FALSE),
          
          conditionalPanel(
            
            condition = "input.repLog == true",
            
            numericInput("numRep", "Input Number of Replication:",
                         value = 1,
                         min = 1,
                         max = 100,
                         step = 1),
            
            checkboxInput("repOrder", "Replication Order: Left-to-Right",
                          value = FALSE),
        ),
      ),
    ),
    
    
    # Show outputs
    mainPanel(
      
      conditionalPanel(
        
        condition = "input.feature == 'Field'",
        ("Possible Divisions for Entries"),
        
        tableOutput("factor"),
        
        conditionalPanel(
          
          condition = "input.field == true",
          ("Field Area (in square meters)"),
          
          textOutput("Area"),
          
          ("Number of Plants in"),
          
          tableOutput("PlantCF"),
          
          ("Plot Dimension"),
          
          tableOutput("PDim"),
          
          conditionalPanel(
            
            condition = "input.hllbtwplt == false",
            ("Number of Plots"),
            
            tableOutput("Without"),
          ),
          
          conditionalPanel(
            
            condition = "input.hllbtwplt == true",
            ("Number of Plots"),
            
            tableOutput("With"),
          ),
          
        ),
        
      ),
      
      conditionalPanel(
        
        condition = "input.feature == 'Layout'",
        
        conditionalPanel(
          
          condition = "input.order == false && input.repLog == false",
          ("PLOT LAYOUT"),
          
          tableOutput("Layout1"),
        ),
        
        conditionalPanel(
          
          condition = "input.order == true && input.repLog == false",
          ("PLOT LAYOUT"),
          
          tableOutput("Layout2"),
        ),
        
        conditionalPanel(
          
          condition = "input.order == false && input.repLog == true && input.repOrder == false",
          ("PLOT LAYOUT"),
          
          tableOutput("Layout3"),
        ),
        
        conditionalPanel(
          
          condition = "input.order == true && input.repLog == true && input.repOrder == false",
          ("PLOT LAYOUT"),
          
          tableOutput("Layout4"),
        ),
        
        conditionalPanel(
          
          condition = "input.order == false && input.repLog == true && input.repOrder == true",
          ("PLOT LAYOUT"),
          
          tableOutput("Layout5"),
        ),
        
        conditionalPanel(
          
          condition = "input.order == true && input.repLog == true && input.repOrder == true",
          ("PLOT LAYOUT"),
          
          tableOutput("Layout6"),
        )
      )
    )
  )
)
