library(shiny)
library(ggplot2)
library(dplyr)
Sys.setlocale(category = "LC_ALL", locale = "Polish")

setwd("C:/Users/alicj/Dropbox/Doktorat/Praca doktorska/Analiza_model_final")
x <- read.csv("1_final_database_1618.csv", as.is = T, row.names = NULL)
d <- x[, c("Nazwa.IZFiA", "Aktywa_log", "Typ.y", "Profil_ad", "PW1000", "Wiek", "OZW2", "OB", "TER", "Aktywa", "Aktywa_log", 
           "Aktywa_log", "std_daily_log_rr", "var_5", "var_95", "es", "eg", "zmiana_akt_perc", "rr_yearly_l", "Rok", 
           "mean_daily_log_rr", "OZZ")]
d$rr_yearly_ob <- (exp(d$rr_yearly_l)-1) + d$OB/100
d$rr_yearly_ob_log <- log(1+d$rr_yearly_ob)

ui <- fluidPage(
  titlePanel("Opłaty w funduszach inwestycyjnych"),
  sidebarLayout(

    sidebarPanel(
        radioButtons("obInput", "Rodzaj stopy zwrotu", choices = c("Stopa zwrotu przed opłatą", "Stopa zwrotu po opłacie"), 
                      selected = "Stopa zwrotu po opłacie"),
        radioButtons("colInput", "Podział wg", choices = c("Aktywa" = "Aktywa", "Rok obserwacji" = "Rok", 
                                                                      "Pierwsza wpłata powyżej 1000 PLN" = "PW1000", 
                                                                      "Profil" = "Typ.y", "brak podziału" = "NULL", "Ryzyko"), 
                     selected = "Typ.y"),
        uiOutput("riskOutput"),
        sliderInput("assetsInput", "Suma aktywów", min = 0, max = 500, value = c(0, 500)),
        sliderInput("ageInput", "Czas funkcjonowania", min = 0, max = 23, value = c(0, 23)),
        checkboxGroupInput("typeInput", "Profil funduszu",
                      choices = c("akcji", "mieszane", "absolutnej stopy zwrotu" = "absolutnejstopyzwrotu", 
                                  "rynku surowców" = "rynkusurowcow", "dłużne" = "dluzne"),
                      selected = c("akcji", "mieszane", "absolutnejstopyzwrotu", "rynkusurowcow", "dluzne")),
        checkboxGroupInput("pwInput", "Pierwsza wpłata powyżej 1000 PLN",
                     choices = c("nie" = "0", "tak" = "1"), selected = c("0", "1")),
        checkboxGroupInput("ozwInput", "Rodzaj opłaty za wynik",
                     choices = c("brak", "ponad benchmark", "wartość absolutna" = "wartosc absolutna", "wzrostu od poczatku roku", "wzrostu od powstania funduszu"), 
                     selected = c("brak", "ponad benchmark", "wartosc absolutna", "wzrostu od poczatku roku", "wzrostu od powstania funduszu")),
        checkboxGroupInput("yearInput", "Rok obserwacji",
                           choices = c("2017" = "17", "2018" = "18", "2019" = "19"), selected = c("17", "18", "19"))
    ),

    mainPanel(
      plotOutput("coolplot"), 
      br(), br(), br(), br(), 
      tableOutput("results"))
  )
)

server <- function(input, output) {
    
  output$riskOutput <- renderUI({
    if(input$colInput == "Ryzyko"){
      radioButtons("riskInput", "Metoda pomiaru ryzyka", 
                   choices = c("Odchylenie standardowe" = "std_daily_log_rr", 
                               "Value at Risk 5%" = "var_5",  
                               "Value at Risk 5% dla krótkiej pozycji" = "var_95", 
                               "Expected Shortfall 5%" = "es",  
                               "Expected Shortfall 5% dla krótkiej pozycji" = "eg"), 
      selected = "var_5")
    }
  })
  
  output$coolplot <- renderPlot({
    if(input$colInput == "Ryzyko"){
      filtered <-
        d %>%
        filter(between(Aktywa, input$assetsInput[1], input$assetsInput[2]),
               Wiek >= input$ageInput[1],
               Wiek <= input$ageInput[2],
               Typ.y %in% input$typeInput,
               PW1000 == input$pwInput,
               Rok == input$yearInput
        )
      if(input$obInput == "Stopa zwrotu przed opłatą") {
        ggplot(filtered, aes(x = rr_yearly_ob_log, y = OB)) +
          geom_point(aes_string(colour = input$riskInput)) 
      }
      else {
        ggplot(filtered, aes(x = rr_yearly_ob, y = OB)) +
          geom_point(aes_string(colour = input$riskInput))
      }
    }
    else {
      filtered <-
        d %>%
        filter(between(Aktywa, input$assetsInput[1], input$assetsInput[2]),
               Wiek >= input$ageInput[1],
               Wiek <= input$ageInput[2],
               Typ.y %in% input$typeInput,
               PW1000 == input$pwInput,
               Rok == input$yearInput
        )
      if(input$obInput == "Stopa zwrotu przed opłatą") {
        ggplot(filtered, aes(x = rr_yearly_ob_log, y = OB)) +
          geom_point(aes_string(colour = input$colInput)) 
      }
      else {
        ggplot(filtered, aes(x = rr_yearly_ob, y = OB)) +
          geom_point(aes_string(colour = input$colInput))
      }
    }
  })

  output$results <- renderTable({
    filtered <-
      d %>%
      filter(between(Aktywa, input$assetsInput[1], input$assetsInput[2]),
             Wiek >= input$ageInput[1],
             Wiek <= input$ageInput[2],
             Typ.y %in% input$typeInput,
             PW1000 == input$pwInput,
             Rok == input$yearInput
      ) %>%
      select(Nazwa.IZFiA, Typ.y, Aktywa, PW1000, Wiek, OZW2, OB)
    filtered
  })

}

shinyApp(ui = ui, server = server)

