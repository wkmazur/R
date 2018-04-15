##################################################
# SERVER // Lesiak & Mazur GARCH modeler         #
##################################################

server <- function(input, output) {
  
  datasetInput <- reactive({
    
    #Wymagaj załadowania danych przez użytkownika
    req(input$plik1)
    
    #Przypusz dane z inputu
    inFile <- input$plik1
    
    #Jeśli brak zwróć NULL
    if (is.null(inFile))
      return(NULL)
    
    #Wczytaj dane z pliku csv
    df <- read.csv(input$plik1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    #df$Data <- as.Date(df$Data)
    df$Data <- as.Date(df$Data,format='%Y-%m-%d')
    df %>% filter(Data>as.Date(input$dates[1]) & Data<as.Date(input$dates[2]) )->df
    
    
  })
  
    #TABELA
    output$contents <- renderTable({
    
       #Przepisanie danych
       df <- datasetInput()
    
       #Zmiana typu danych w celu poprawnego wyświetlenia
       df$Data <- as.character(df$Data)
    
       #Wyświetl Head lub Summary
       if(input$disp == "head") {
         return(head(df))  }
       else {
         return(summary(df))  }
  
  })
  
    #WYKRES
    #output$wykres <- renderPlot({
      
      #Przepisz dane
     # df <- datasetInput()
        
      #Nagłówki
      #colnames(df)<-c("Date","Open","High","Low","Close","Volume")
      
      #Stopy zwrotu
      #df$r <- diff.xts(log(df$Close))
      
      #Wykres cen i zwrotów
      #twoord.plot(lx=c(seq(1:length(df$Date))),ly=df$Close,rx=c(seq(1:length(df$Date))),
       #           ry=df$r,
        #          xlab="Date",
         #         ylab="Cena zamkniecia",rylab="Logarytmiczna stopa zwrotu",lcol=4,
          #        main="Wykres cen wybranych akcji i ich zwroty",
           #       type=c("l","l"),
            #      xticklab = df$Date)
      
      #})
    
    #HISTOGRAM  
    output$histogram <- renderPlot({
      
      #Przepisz dane
      df <- datasetInput()
        
      #Nagłówki
      colnames(df)<-c("Date","Open","High","Low","Close","Volume")
      
      #Stopa zwrotu
      df$r <- diff.xts(log(df$Close))
      
      #Histogram
      hist(df$r, prob = T, breaks = 100, main = "Histogram zwrotów")
      curve(dnorm(x,
                  mean = mean(df$r, na.rm = T),
                  sd = sd(df$r, na.rm = T)),
            col = "darkblue", lwd = 2, add = TRUE)
      
    })
    
    #Interaktywny wykres
    output$dygraph <- renderDygraph({
      
      #import danych
      df <- datasetInput()
      
      #Nagłówki
      colnames(df)<-c("Date","Open","High","Low","Close","Volume")
      
      #Stopa zwrotu
      df$r <- diff.xts(log(df$Close))
      temp<-df[,c("Close","r","Date")]
      
      #Wykres
      dygraph(xts(x=temp[-3],order.by = temp$Date)) %>%
        dyAxis("y", label = "Close") %>%
        dyAxis("y2", label = "r", independentTicks = TRUE) %>%
        dySeries("r", axis = 'y2') %>% 
        dyOptions(fillAlpha=0) %>% 
        dyRangeSelector()
      
    })
    
    
    #MODEL
    tworzeniespecu <- reactive({
      
      #Rodzaj modelu
      model<-switch(input$model,
                    "Standard GARCH"="sGARCH", #OK
                    "Exponential GARCH (EGARCH)"="eGARCH", #OK
                    "Treshold Garch TGARCH"="fGARCH",
                    "Integrated GARCH (iGARCH)"="iGARCH", #OK
                    "GJR-GARCH"="gjrGARCH", #OK
                    "Asymmetric Power ARCH (APARCH)"="apARCH", #OK
                    "Component Garch"="csGARCH", #OK
                    "Absolute Value GARCH (AVGARCH)"="fGARCH",
                    "Nonlinear GARCH (NGARCH)"="fGARCH",
                    "Nonlinear Asymmetric GARCH (NA-GARCH)"="fGARCH",
                    "Full GARCH (ALLGARCH)"="fGARCH")
      
      #Jeśli model nie należy do family GARCH model to submodel NULL
      submodell <- NA
      #TGARCH
      if(input$model == "Treshold Garch TGARCH") {
        submodell <- as.character("TGARCH")  }
      #AGARCH
      if(input$model == "Absolute Value GARCH (AVGARCH)") {
        submodell <- as.character("AVGARCH")  }
      #NGARCH
      if(input$model == "Nonlinear GARCH (NGARCH)") {
        submodell <- as.character("NGARCH")  }
      #NA-GARCH
      if(input$model == "Nonlinear Asymmetric GARCH (NA-GARCH)") {
        submodell <- as.character("NAGARCH")  }
      #ALLGARCH
      if(input$model == "Full GARCH (ALLGARCH)") {
        submodell <- as.character("ALLGARCH")  }
      
        
      #Zakładany rozkład modelu
      rozklad<-switch(input$rozklad,
                      "Normalny"="norm",
                      "Generalized Error Distribution (GED)"="ged",
                      "t-Studenta"="std",
                      "Skosny Normalny"="snorm",
                      "Skosny Generalized Error Dist."="sged",
                      "Skosny t-Studenta"="sstd",
                      "Normal Invwerse Gaussian"="nig",
                      "GH-Skosny t-Student"="ghst",
                      "Johnon's SU"="jsu")
      
      #Czy stała?
      stala<-switch(input$stala,
                    "TAK"=TRUE,
                    "NIE"=FALSE)
      
      #Rzędy P, Q i AR MA
      p<-input$p
      q<-input$q
      ar<-input$ar
      ma<-input$ma
      
      #Spec dla modelu
      spec = ugarchspec(
        variance.model = list(model = model,
                              garchOrder = c(p,q),
                              submodel = submodell),
        mean.model = list(armaOrder = c(ar, ma), include.mean = stala),
        distribution.model =rozklad)

    })
    
    
    output$dopasowanymodel <- renderPrint({
      
      #Okres in-sample i out-of-sample
      datasetInput() %>% filter(Data>as.Date(input$train[1]) & Data<as.Date(input$train[2]) )->train
      datasetInput() %>% filter(Data>as.Date(input$test[1]) & Data<as.Date(input$test[2]) )->test
      
      #Nagłówki
      colnames(train)<-c("Date","Open","High","Low","Close","Volume")
      colnames(test)<-c("Date","Open","High","Low","Close","Volume")
      
      #Zwroty
      train$r <- diff.xts(log(train$Close))
      test$r <- diff.xts(log(test$Close))
      
      #Wybór wykresu diagnostycznego
      wykres_diagnostyczny<-switch(input$wykres_diagnostyczny,
                                   "Zwroty/odchylenie"=1,
                                   "VAR 1% w okresie in-sample"=2,
                                   "Wariancja warunkowa i zwroty"=3,
                                   "ACF zmiennej"=4,
                                   "ACF kwadratow zmiennej"=5,
                                   "ACF wartosci bezwzglednych zmiennej"=6,
                                   "Korelacja krzyzowa kwadratow zmiennej i zmiennej"=7,
                                   "Rozklad empiryczny standaryzowanych reszt"=8,
                                   "Rozklad kwantylowy standaryzowanych reszt"=9,
                                   "ACF standaryzowanych reszt"=10,
                                   "ACF kwadratow standaryzowanych reszt"=11,
                                   "Krzywa News-Impact"=12,
                                   "Wszystkie"="all")
      
      #Oszacowanie modelu
      oszacowany.model <- ugarchfit(spec = tworzeniespecu(),
                                    data = na.omit(train$r)) 
      
      #Drukuj wydruk z oszacowaniem modelu
      print(oszacowany.model)
      
      #Wykres diagnostyczny
      output$wykres_diagnostyczny <- renderPlot({
        plot(oszacowany.model,which=wykres_diagnostyczny)
       })
    })
    
#---------------- Prognoza
    
    liczstart <- eventReactive(input$licz, {
      print(noquote("Prognoza warunkowej wariancji została oszacowana!"))
    })
    
    output$koniecliczenia <- renderPrint({ 
      liczstart() 
    })
    
    
    #Wykonaj jeżeli wciśnięt przyczisk Licz prognozę
    progn <- eventReactive(input$licz, {
      
      #Przepisz dane
      df <- datasetInput()
      
      #Nagłówki
      colnames(df)<-c("Date","Open","High","Low","Close","Volume")
      
      #Stopa zwrotu
      df$r <- diff.xts(log(df$Close))
      
      #Zmienne potrzebne do pętli
      df$obs<- 1:nrow(df)
      start  <- df$obs[df$Date >= as.Date(input$test[1])]
      start <- start[1]
      finish <- df$obs[df$Date <= as.Date(input$test[2])]
      finish <- finish[length(finish)]
      outsample <- df[start:finish, ]
      warunkoweodchylenie <- rep(NA, times = finish - start + 1)
      
        #Prognoza - pętla
        #Czas start
        time1 <- Sys.time()
        for (k in start:finish) {
          tmp.data <- df[df$obs <= (k - 1), ]
          tmp.data <- tmp.data[as.Date(input$train[1]) <= tmp.data$Date, ]
          spec <- tworzeniespecu()
          tmp.model <- ugarchfit(spec = spec, data = na.omit(tmp.data$r))
          sigma.forecast  <- ugarchforecast(tmp.model, n.ahead = 1)
          sigma.forecast2 <- sigma.forecast@forecast$sigmaFor[1, 1]
          warunkoweodchylenie[k - start + 1] <- sigma.forecast2
          
          #zmienna z odchyleniem warunkowym
          prognoza.war.odch <- warunkoweodchylenie
        }
        
              #Wariancja zwrotów out-of-sample
              rout <- df$r[df$obs >= start]
              var.rout <- NA
              var.rout <- (na.omit(rout)-mean(na.omit(rout)))^2
              var.rout <- as.numeric(var.rout)
              
              ### Błędy prognoz
              #ME - MEAN ERROR
              me <- function(obs,pred){
                mean(pred-obs)
              }
              #MAE
              mae <- function(obs,pred){
                mean(abs(pred-obs))
              }
              #RMSE
              rmse_moj <- function(obs,pred){
                sqrt(mean((pred-obs)^2))
              }
              #AMAPE
              amape<-function(obs,pred){
                mean(abs((pred^2-obs^2)/(pred^2+obs^2)))
              }
              #TIC
              tic <- function(obs,pred){
                (sqrt(mean((pred-obs)^2)))/(sqrt(mean(pred^2))+sqrt(mean(obs^2)))
              }
              
          #Czas przeliczenia
          output$bledyprognoz <- renderPrint({
              print(noquote("Bledy prognoz wynosza:"))
              #ME
              print(noquote("ME:"))
                print(me(var.rout,warunkoweodchylenie))
              #MAE    
              print(noquote("MAE:"))
                print(mae(var.rout,warunkoweodchylenie))
              #RMSE  
              print(noquote("RMSE:"))
                print(rmse_moj(var.rout,warunkoweodchylenie))
              #AMAPE  
              print(noquote("AMAPE:"))
                print(amape(var.rout,warunkoweodchylenie))
              #TIC  
              print(noquote("TIC:"))
                print(tic(var.rout,warunkoweodchylenie)) 
            })  
        
        #Czas stop
        time2 <- Sys.time()
          
          #Czas przeliczenia
         output$czasprzeliczenia <- renderPrint({
            print(noquote("Czas wykonania się funkcji wynosi: "))
            print(time2-time1)  
         })
          
        #Drukuj oszacowane prognozy warunkowej wariancji
         print(noquote("Pierwsze obserwacje oszacowań: "))
         print(head(prognoza.war.odch^2))
        
        #Interaktywny wykres odchylenia warunkowego
        output$wykres_warunkowaWar<-renderDygraph({
          
          outsample<-cbind(outsample,prognoza.war.odch)[,c("r","prognoza.war.odch","Date")]
          print(colnames(outsample))
          
          #Wykres
          dygraph(xts(outsample[,c("r","prognoza.war.odch")],order.by = outsample$Date)) %>%
            dyAxis("y", label = "r") %>%
            dyAxis("y2",valueRange = c(0, max(outsample$prognoza.war.odch)*10), label = "prognoza.war.odch", independentTicks = TRUE) %>%
            dySeries("prognoza.war.odch", axis = 'y2') %>% 
            dyOptions(fillAlpha=0) %>% 
            dyRangeSelector()
        })  
      
      #Wykres warunkowej wariancji  
      output$wykresprognozy <- renderPlot({   
          plot(df$Date[df$obs[start:finish]], prognoza.war.odch^2,
               type='l', main="Wykres prognozy warunkowej wariancji", xaxt='n', 
               xlab = " ", ylab = "Warunkowa wariancja", lwd=2, col='green' )
        axis.Date(1, at=seq(df$Date[df$obs[start]], df$Date[df$obs[finish]],"month"), format="%m/%Y", las=2)
              })  
        
        })#koniec ciała prognozy (wyczekiwania na przycisk Licz)

    
    #Uruchamianie prognozy
    output$prognozowanie <- renderPrint({ 
      
          #Prognoza
            progn() 
    })
    
}

