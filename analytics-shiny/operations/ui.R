library(ggplot2, lib.loc="/home/washim/R/x86_64-pc-linux-gnu-library/3.2")
library(plotly, lib.loc="/home/washim/R/x86_64-pc-linux-gnu-library/3.2")
library(shinydashboard, lib.loc="/home/washim/R/x86_64-pc-linux-gnu-library/3.2")
library(dplyr, lib.loc="/home/washim/R/x86_64-pc-linux-gnu-library/3.2")
library(tidyr, lib.loc="/home/washim/R/x86_64-pc-linux-gnu-library/3.2")
library(dygraphs, lib.loc="/home/washim/R/x86_64-pc-linux-gnu-library/3.2")
library(xts, lib.loc="/home/washim/R/x86_64-pc-linux-gnu-library/3.2")

dashboardPage(
  dashboardHeader(title = "DataScienceVisual"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Drupal Portal Insights", tabName = "DrupalPortalInsights", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "DrupalPortalInsights",
        fluidRow(
          box(
            title = "How contents growing?",
            plotlyOutput("plotContentGrow"),
            verbatimTextOutput("event")
          ),
          box(
            title = "How users increasing?"
          ),
          box(
            title = "Predicted future trend"
          ),
          box(
            title = "Geo locations of portal access"
          )
        )
      )
    )
  )
)