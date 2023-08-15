#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(dashboardthemes)

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pre-Trial", tabName = "preTrial", icon = icon("clipboard"))
    )
  ),
  dashboardBody(
    
    #or use theme poor_mans_flatly
    shinyDashboardThemes(
      theme = "blue_gradient"
    ),
    
    tabItems(
      #Pre-trial tab
      tabItem(tabName = "preTrial",
        fluidRow(
          #Section for selecting experimental design
          #Input entry/reps depending on design
          box(
            width = 12,
            title = "Experimental Design",
            collapsible = TRUE,
            collapsed = TRUE,
            sidebarLayout(
              #Selecting experimental design
              sidebarPanel(
                selectInput("design", "Select Design",
                            choices = c("Alpha-Lattice", "Partially Replicated")),
                #Show panel if design is alpha-lattice
                conditionalPanel(
                  condition = "input.design == 'Alpha-Lattice'",
                  
                  numericInput("entry", "Number of entries:",
                               value = 100,
                               min = 0,
                               max = 100000,
                               step = 10),
                  numericInput("rep", "Number of replications:",
                               value = 2,
                               min = 2,
                               max = 1000,
                               step = 1),
                  actionButton("save1", "Save", icon = icon("floppy-disk")),
                ),
                #show panel if design is partially replicated
                conditionalPanel(
                  condition = "input.design == 'Partially Replicated'",
                  
                  actionButton("add_btn", "Add Group Entry"),
                  actionButton("rm_btn", "Remove Group Entry"),
                  #populated entry fields
                  fluidRow(
                    column(4,
                           uiOutput("groupEntry_ui"),
                    ),
                    column(5,
                           uiOutput("repGroupEntry_ui"),
                    ),
                  ),
                  actionButton("save2", "Save", icon = icon("floppy-disk")),
                ),
              ),
              mainPanel(
                #Factor output for alpha-lattice design
                conditionalPanel(
                  condition = "input.design == 'Alpha-Lattice'",
                  tableOutput("factor"),
                ),
                #Factor output for PRep design
                conditionalPanel(
                  condition = "input.design == 'Partially Replicated'",
                  tableOutput("totalEnt"),
                  tableOutput("factorPRep")
                )
              )
            )
          ),
          #Section for computing the field dimension
          #Field and Plot Area, Number of plots in a row/column
          box(
            width = 12,
            title = "Field Dimension",
            collapsible = TRUE,
            collapsed = TRUE,
            sidebarLayout(
              sidebarPanel(
                #Input for the field dimension and number of fields
                fluidRow(
                  column(5,
                    numericInput("len", "Field Length (in meters):",
                             value = 100,
                             min = 0,
                             max = 10000,
                             step = 10),
                    numericInput("wdt", "Field Width (in meters):",
                                 value = 100,
                                 min = 0,
                                 max = 10000,
                                 step = 10),
                  ),
                  column(6,
                    numericInput("fL", "Number of Fields (Horizontal):",
                             value = 1,
                             min = 0,
                             max = 100,
                             step = 1),
                    numericInput("fW", "Number of Fields (Vertical):",
                                 value = 1,
                                 min = 0,
                                 max = 100,
                                 step = 1),
                  ),
                ),
                #Select the planting method
                selectInput("est", "Establishment",
                            choices = c("Direct seeded", "Transplanted"),
                            selected = "Transplanted"),
                #Input for direct seeded method
                conditionalPanel(
                  condition = "input.est == 'Direct seeded'",
                  fluidRow(
                    column(5,
                      numericInput("pSRD", "Plot row spacing (in meters):",
                               value = 0.2,
                               min = 0,
                               max = 2,
                               step = 0.05),
                      numericInput("rL", "Plot row length (in meters):",
                                   value = 1,
                                   min = 0,
                                   max = 1000,
                                   step = 1),
                      actionButton("save3", "Save", icon = icon("floppy-disk"))
                    ),
                    column(6,
                      numericInput("pRD", "Number of plant rows (per plot):",
                               value = 1,
                               min = 0,
                               max = 1000,
                               step = 1)
                    )
                  )
                ),
                #Input for transplanted method
                conditionalPanel(
                  condition = "input.est == 'Transplanted'",
                  fluidRow(
                    column(5,
                      numericInput("pSRT", "Plot row spacing (in meters)",
                               value = 0.2,
                               min = 0,
                               max = 2,
                               step = 0.05),
                      numericInput("pSC", "Plot hill spacing (in meters)",
                                   value = 0.2,
                                   min = 0,
                                   max = 2,
                                   step = 0.05),
                      actionButton("save4", "Save", icon = icon("floppy-disk")),
                    ),
                    column(6, 
                      numericInput("pRT", "Number of plant rows (per plot)",
                               value = 1,
                               min = 0,
                               max = 1000,
                               step = 1),
                      numericInput("pC", "Number of plant hills (per plot)",
                               value = 1,
                               min = 0,
                               max = 1000,
                               step = 1),
                    )
                  )
                ),
              ),
              mainPanel(
                #Field and Plot Area, Number of plots in a row and column
                #Transplanted
                conditionalPanel(
                  condition = "input.est == 'Transplanted'",
                  tableOutput("fldA2"),
                  tableOutput("pltA2"),
                  tableOutput("fld2")
                ),
                #Field and Plot Area, Number of plots in a row and column
                #Direct Seeded
                conditionalPanel(
                  condition = "input.est == 'Direct seeded'",
                  tableOutput("fldA1"),
                  tableOutput("pltA1"),
                  tableOutput("fld1")
                ),
              )
            )
          ),
          #Section for creating a Layout for the experiment
          box(
            width = 12,
            title = "Field Layout",
            collapsible = TRUE,
            collapsed = TRUE,
            sidebarLayout(
              sidebarPanel(
                #Populate the choices based on the number of entries and field dim
                uiOutput("Choices"),
                #Selects the plot ordering to be implemented
                selectInput("pOrder", "Plot Ordering",
                            choices = c("Row-Serpentine", "Left-to-Right")),
                #If the design is alpha lattice, select rep order to be implemented
                conditionalPanel(
                  condition = "input.design == 'Alpha-Lattice'",
                  
                  selectInput("rOrder", "Replication Ordering",
                              choices = c("Left-to-Right", "Top-to-Bottom")),
                  actionButton("save5", "Save", icon = icon("floppy-disk")),
                ),
                
                conditionalPanel(
                  condition = "input.design == 'Partially Replicated'",
                  
                  actionButton("save6", "Save", icon = icon("floppy-disk")),
                )
              ),
              mainPanel(
                conditionalPanel(
                  condition = "input.design == 'Partially Replicated'",
                  
                  dataTableOutput("Layout1"), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                ),
                conditionalPanel(
                  condition = "input.design == 'Alpha-Lattice'",
                  
                  dataTableOutput("Layout2"), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                )
              )
            )
          )
        )
      )
    )
  )
)