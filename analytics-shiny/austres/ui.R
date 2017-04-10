.libPaths("/home/washim/R/x86_64-pc-linux-gnu-library/3.2")
library(shiny)
library(datasets)
library(dygraphs)
library(xts)
library(tidyverse)

shinyUI(fluidPage(
  dygraphOutput("dygraph")
))
