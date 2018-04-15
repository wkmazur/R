##################################################
# USER INTERFACE // Lesiak & Mazur GARCH modeler #
##################################################

#Potrzebne biblioteki
  #Jeśli zajdzie potrzeba instaluj
if (!require("shiny")) install.packages("shiny")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("dplyr")) install.packages("dplyr")
if (!require("xts")) install.packages("xts")
if (!require("rugarch")) install.packages("rugarch")
if (!require("dygraphs")) install.packages("dygraphs")
#Wczytaj biblioteki
library(shiny)
library(shinythemes)
library(dplyr)
library(plotrix)
library(xts)
library(rugarch)
library(dygraphs)

# Define UI for data upload app ----
ui <- tagList(
  #shinythemes::themeSelector(),
  navbarPage( theme=shinytheme("flatly"), 
    "Lesiak & Mazur GARCH modeler", 
#-------------------------------------------    
  #1 Zakładka wczytywanie danych
  tabPanel("Wczytywanie danych",
  
  sidebarPanel(
  
    #Wczytywanie pliku    
  fileInput("plik1", "Wybierz plik CSV",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line
      tags$hr(),
      
      # Input: Checkbox czy nagłowki kolumn
      checkboxInput("header", "Nagłówki", TRUE),
      
      # Input: Wybierz separator
      radioButtons("sep", "Separator",
                   choices = c(Przecinek = ",",
                               Średnik = ";",
                               Tabulator = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Wyświetlaj:",
                   choices = c(Head = "head",
                               Summary = "summary"),
                   selected = "head"),
      
      #Zakres danych
      dateRangeInput("dates", 
                     "Date range",
                     start = "2013-01-01",
                     end = as.character(Sys.Date()))
      
    ),
    
    # Main panel do wyświetlania outputów
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents"),
      
      #Wykres
     # plotOutput("wykres"),
      
    #  br(),      br(),
      
      #tutaj
      dygraphOutput("dygraph"),
      
      #2 new lines
      br(),
      br(),
      
      #Histogram
      plotOutput("histogram")
      
    )),
#-----------------------------------
  #2 Zakłądka trenowanie modelu
  tabPanel("Trenowanie modelu",
    sidebarPanel(
             
      dateRangeInput("train", 
                "Próbka treningowa",
                start = "2013-01-01",
                end = as.character(Sys.Date()-366)),
      
      dateRangeInput("test", 
                "Próbka testowa",
                start = as.character(Sys.Date()-365),
                end = as.character(Sys.Date())),
      
      selectInput("model", "Wybierz model GARCH:", 
                choices = c("Standard GARCH",
                            "Exponential GARCH (EGARCH)", 
                            "Treshold Garch TGARCH",
                            "Integrated GARCH (iGARCH)", 
                            "GJR-GARCH", 
                            "Asymmetric Power ARCH (APARCH)",
                            "Component Garch",
                            "Absolute Value GARCH (AVGARCH)",
                            "Nonlinear GARCH (NGARCH)",
                            "Nonlinear Asymmetric GARCH (NA-GARCH)",
                            "Full GARCH (ALLGARCH)")),
      
      selectInput("rozklad", "Wybierz zakladany rozklad zmiennej:", 
                  choices = c("Normalny",
                              "Generalized Error Distribution (GED)",
                              "t-Studenta",
                              "Skosny Normalny",
                              "Skosny Generalized Error Dist.",
                              "Skosny t-Studenta",
                              "Normal Invwerse Gaussian",
                              "GH-Skosny t-Student",
                              "Johnon's SU")),
      selectInput("stala", "Czy model ze stala:", 
                  choices = c("TAK",
                              "NIE")),
      numericInput("q", "Rząd q procesu ARCH(q):", 1, min=0, max=10, step=1),
      numericInput("p", "Rząd p procesu GARCH(p):", 1, min=0, max=10, step=1), 
      numericInput("ar", "Rząd procesu AR:", 0, min=0, max=10, step=1),
      numericInput("ma", "Rząd procesu MA:", 0, min=0, max=10, step=1),
      
      paste("Ponieżej znajdują się wykresy diagnostyczne modelu"),
      
      #Duży odstęp
      br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
      br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
      br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
      br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
      br(),br(),br(),
      
      #Wybór wykresu diagnostycznego
      selectInput("wykres_diagnostyczny", "Wybierz rodzaj wykresu diagnostycznego:", 
                  choices = c("Zwroty/odchylenie",
                              "VAR 1% w okresie in-sample",
                              "Wariancja warunkowa i zwroty",
                              "ACF zmiennej",
                              "ACF kwadratow zmiennej",
                              "ACF wartosci bezwzglednych zmiennej",
                              "Korelacja krzyzowa kwadratow zmiennej i zmiennej",
                              "Rozklad empiryczny standaryzowanych reszt",
                              "Rozklad kwantylowy standaryzowanych reszt",
                              "ACF standaryzowanych reszt",
                              "ACF kwadratow standaryzowanych reszt",
                              "Krzywa News-Impact",
                              "Wszystkie"
                  ))
      
    ),
      
      mainPanel(
      
        verbatimTextOutput("dopasowanymodel"),
        plotOutput("wykres_diagnostyczny")
             
           )
  ),
#-----------------------------------
  #3 Zakładka - Prognoza warunkowej wariancji
  tabPanel("Prognoza Warunkowej Wariancji",
    sidebarPanel( 
      
      #Przycisk Licz prognozę
      actionButton("licz", "Licz prognozę"), br(),
      print("Uwaga! Liczenie prognozy w okresie out ofsample może trwać dłuższą chwilę"), br(), br(),
      
      #Ilość okresów prognozy
      #numericInput("okresy", "Ilość okresów prognozy w przód:", 1, min=1, max=5, step=1),
      
      #br(),
      
      print("Poniżej znajdują się błędy prognoz")
    
      ),

    mainPanel(
      
      #Informacja o zakończneiu liczenia prognozy
      verbatimTextOutput("koniecliczenia"),
      
      verbatimTextOutput("czasprzeliczenia"),
      
      #Pierwsze obserwacje prognozy
      verbatimTextOutput("prognozowanie"),
      
      br(),
      
      #Wykres odchylen warunkowych prognozy
      dygraphOutput("wykres_warunkowaWar"),
      
      br(),
      
      #Wykres wariancji warunkowej prognozy
      plotOutput("wykresprognozy"),
      
      #Błędy prognoz
      verbatimTextOutput("bledyprognoz")
      
    )
  )
))
