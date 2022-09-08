
library(shiny)
library(shinydashboard)
library(mathjaxr)
library(ggplot2)

############### VISUAL (UI) ###########################

#if (interactive()) {
# Dashboard inicio
ui <- dashboardPage(
        
        ####--Color
        skin = "black",
        
        ######Titulo de la pagina
        dashboardHeader(title="Loan Calculator"),
        
        ###### Barra de Menu
        dashboardSidebar(
            
            ## Iconos y nombres de las secciones del menu
            # Los icnonos se pueden elegir en la siguiente pagina
            # https://getbootstrap.com/docs/3.4/components/#glyphicons
            sidebarMenu(
                menuItem('Información',tabName = 'inicio',icon = icon('info')),
                menuItem('Calculadora',tabName = 'calcu',icon = icon('usd')),
                menuItem('Codigo',tabName = 'cod',icon = icon('signal'))
                
            )
            
        ), # <-- Fin Brra Menu
        
        ###### Secciones de la Pagina
        dashboardBody(
          tags$style("body { background-color: ghostwhite}"),
            ###  Seccion Inicio -------
            tabItems(
                ## Seccion Inicio Body
                tabItem(tabName = "inicio",
                      
                      imageOutput("imgportada"),  
                        
                      h1("Calculadora de Prestamo",align="center",
                         style = "font-family: Palatino;"),
                      
                      
                      br(),
                      
                      h2('Propósito',style = "font-family: Palatino;",
                         align="center"),
                      
                      p('Este proyecto consiste en la creación de una calculadora 
                      capaz de informarle al usuario
                      elementos informativos acerca del prestamo que solicito.
                        Por ejemplo:',
                      style = "font-family: Palatino;"),
                      
                      ## Bullets
                      tags$div(
                        tags$ul(
                          tags$li("Total del monto a pagar"
                                  ,style = "font-family: Palatino;"),
                          tags$li("Intereses",style = "font-family: Palatino;"),
                          tags$li("Pagos periodicos",
                                  style = "font-family: Palatino;")
                        )
                      ),
                      
                      h2("Metodología",style = "font-family: Palatino;",
                         align="center"),
                      p("Para realizar esta calculadora, las formulas utilizadas 
                        fueron considerando el metodo de 
                        amortización gradual. Este método consiste en efectuar 
                        pagos nivelados a lo  largo del periodo. 
                        A continuación, se explicara en detalle el procedimiento.",
                        style = "font-family: Palatino;text-align: justify; font-size = 25px"),
                      
                      ## Pagos
                      h3("Pagos",style = "font-family: Palatino;"),
                      p("El monto de los pagos que se realizará a lo largo 
                        del periodo.",
                        style = "font-family: Palatino;text-align: justify; font-size = 25px"),
                      imageOutput("imgpagos"),
                      
                      ## saldo insoluto
                      h3("Saldo insoluto",style = "font-family: Palatino;"),
                      p("Muestra la cantidad que todavía falta pr pagar.",
                        style = "font-family: Palatino;text-align: justify; font-size = 25px"),
                      imageOutput("imgsi"),
                      
                      ## intereses
                      h3("Intereses",style = "font-family: Palatino;"),
                      p("El monto correspondiente a los intereses que deben
                        pagarse periodicamente",
                        style = "font-family: Palatino;text-align: justify; font-size = 25px"),
                      imageOutput("imgint"),
                      
                      ## Amortizacion
                      h3("Amortización",style = "font-family: Palatino;"),
                      p("Muestra la cantidad amortizada de la deuda.",
                        style = "font-family: Palatino;text-align: justify; font-size = 25px"),
                      imageOutput("imgamo"),
                      
                      
                        
                        
                ),  # <-- Fin seccion inicio   
                
            ### Seccion Cacluladora Body -------
                tabItem(tabName = "calcu",
                    
                    ######## BOX de Datos resultados
                    fluidRow( 
                      
                      ######## Pagos
                      valueBoxOutput('Pagos'),
                      ######## Monto + intereses
                      valueBoxOutput('TotalPago'),
                      ######## Intereses
                      valueBoxOutput('Intereses')
                      
                      
                    ), # <-- FIN fluidRow 2    
                        
                        
                        
                    ####### Insertar datos         
                    fluidRow( # <- inicio fluidRow 1
                        
                    ####### Datos    
                        ## Inicio Box Insertar datos
                        # Estetica del Box    
                        box(title='Datos del Prestamo', status = 'primary', solidHeader = TRUE,
                            br(),
                            'Responda a lo siguiente para poder realizar el calculo',
                            br(),
                            ## Ventana de texto para el monto
                            # 'monto' - nombre de la variable (monto)    
                            #numericInput("monto", "Monto del prestamo ($):",
                            #          value = 10000),
                            sliderInput("monto","Monto del prestamo:",
                                        min = 100000,max=100000000,
                                        value = 100000,
                                        step = 1000,animate = TRUE),
                            br(),
                            
                            ## Barra para selecciona el anio
                            # 'n' - nombre de la variable (anios)
                            #numericInput("anios", "Duración del periodo de deuda (anios):",
                            #            value = 30),
                            sliderInput("anios","Duración del periodo de deuda (anios):",
                                        min = 1,max=100,value = 5,
                                        step = 2,animate = TRUE),
                            
                            br(),
                            
                            ## Modo de pago
                            # tipo de capitalizacion de la tasa
                            #### Numero de periodos de capitalizacion al anio
                            # la variable cap representa los periodos de capitalizacion
                            # 12 = mensual (12 periodos al anio)
                            # 6 = semestral (2 periodos al anio)
                            # 4 = cuatrimestral (3 periodos al anio)
                            # 3 = trimestral (4 periodos al anio)
                            # 2 = bimestral (6 periodos al anio)
                            # 1 = anual (1 periodo)
                            
                            "Escriba el número correspondiente de acuerdo a su modo de pago",br(),
                            #"12 = mensual",br(),
                            "6 = semestral",br(),
                            "4 = cuatrimestral ",br(),
                            "3 = trimestral",br(),
                            "2 = bimestral",br(),
                            "1 = anual",
                            #numericInput("cap", "Modo de pago:",
                            #             min = 1,max=100,value = 1),
                            
                            #sliderInput("cap","Modo de pago:",
                            #            min = 1,max=12,value = 5,
                            #            step = 1),
                            radioButtons(
                              inputId = "cap",
                              label = "Modo de pago:", 
                              choices = c("6", "4","3","2","1")
                            ),
                            
                            
                            ## Ventana de texto para la tasa de interes
                            #  'tasa' - nombre de la variable (tasa)
                            #numericInput("tasa", "Interes de interes anual (%):",
                            #             min = 1,max=100,value = 5)
                            sliderInput("tasa","Interes de interes anual (%):",
                                        min = 1,max=10,value = 5,
                                        step = 1,animate = TRUE)
                            
                            
                            
                        ), # <-- Fin Box Insertar datos 
                        
                    ####### Grafica 
                        ## Inicio Box de Grafica 
                        box(title='Gráfica', status = 'info', solidHeader = TRUE,
                            plotOutput('graf')
                            
                        )# <-- Fin Box grafica 
                        
                        
                    ) # <-- FIN fluidRow 1
                    
                        
                ), # <--- Fin Cacluladora 
           
            ### Seccion codigo ------
                tabItem( tabName = "cod",
                    
                    h1("Código",align="center", style = "font-family: Palatino;"),
                    p("Si desea saber el codigo detras de la aplicación,
                      ingrese al siguiente enlace",
                      style = "font-family: Palatino;text-align: justify; font-size = 25px"),
                    uiOutput("link"),
                    imageOutput("imgblog"),
                    p("Tambien, en esta otro link puesdes encontrar el código de la
                      aplicación en Shiny",style = "font-family: Palatino;text-align: justify; font-size = 25px"),
                    uiOutput("link2"),
                    imageOutput("imgblog2")
                    
                    
                ) # <--- FIN Simulaciones
            
                
            ) # <-- fin TabItems (contiene todos las secciones del menu)
            
            
        ) # <-- Fin Body
        
    ) # <--- Fin Dashboard
#)



################# SERVIDOR ####################
# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
    })
    
    #### Funciones -----
            

        #### Periodo variable    
        Periodo_Func <- function(cap){
            
            if (cap==12) {
                perido <- 12
            } else if (cap == 6) {
                periodo <- 2
                return(periodo)
            } else if (cap == 4) {
                periodo <- 3
                return(periodo)
            } else if (cap == 3) {
                periodo <- 4
                return(periodo)
            } else if (cap == 2) {
                periodo <- 6
                return(periodo)
            } else if (cap == 1) { 
                periodo <- 1
                return(periodo)
            } else {
                return("Escriba un valor aceptable en capitalizacion de la tasa")
            }
        }
    
        #### Pagos
        R_function <- function(L,tasa,anios,cap,valor='no'){
            
            #### Numero de periodos de capitalizacion al anio
            # la variable cap representa los periodos de capitalizacion
            # 12 = mensual (12 periodos al anio)
            # 6 = semestral (2 periodos al anio)
            # 4 = cuatrimestral (3 periodos al anio)
            # 3 = trimestral (4 periodos al anio)
            # 2 = bimestral (6 periodos al anio)
            # 1 = anual (1 periodo)
            
            if (cap==12) {
                perido <- 12
            } else if (cap == 6) {
                periodo <- 2
            } else if (cap == 4) {
                periodo <- 3
            } else if (cap == 3) {
                periodo <- 4
            } else if (cap == 2) {
                periodo <- 6
            } else if (cap == 1) { 
                periodo <- 1  
            } else {
                return("Escriba un valor aceptable en capitalizacion de la tasa")
            }
            
            # tasa capitalizable mensual
            # debemos de dividir la tasa anual entre el numero de peridos
            # ... de acuerdo con el periodo de capitalizacion de la tasa
            tasa <- tasa/100
            i <- tasa/periodo
            v <- 1+i
            # anios en periodos
            # para saber el numero de periodos o meses totales
            # ... es neesario multiplicar la cantidad de anios por los meses.
            n <- anios*periodo
            #anualidad vencida
            an <- (1-(1/v)^n)/i
            
            
            # formula de Pagos de una amortizacion gradual
            # L es el monto total de la deuda
            R <- L/an
            
            if (valor == 'no'){
                
                #crear dataframe
                resultadosDF <- data.frame(
                    "Pagos.Mensuales"=c(round(R,2))
                )
                
                return(resultadosDF)
            } else if (valor == 'si'){
                
                resultados<- round(R,2)
                return(resultados)
            }
            
            
        } # <--- FIN R_function() Funcion calculo de Rt
        
        ### Categorias de tipo de pago 
            # 12 = mensual (12 periodos al anio)
            # 6 = semestral (2 periodos al anio)
            # 4 = cuatrimestral (3 periodos al anio)
            # 3 = trimestral (4 periodos al anio)
            # 2 = bimestral (6 periodos al anio)
            # 1 = anual (1 periodo)
        TipoPago_Function <- function(cap){
            
            #cap
            
            if (cap==12) {
                TipoPago <- "Mensual"
                return(TipoPago)
            } else if (cap == 6) {
                TipoPago <- "Semestral"
                return(TipoPago)
            } else if (cap == 4) {
                TipoPago <- "Cuatrimestral"
                return(TipoPago)
            } else if (cap == 3) {
                TipoPago <- "Trimestral"
                return(TipoPago)
            } else if (cap == 2) {
                TipoPago <- "Bimestral"
                return(TipoPago)
            } else if (cap == 1) { 
                TipoPago <- "Anual"
                return(TipoPago)
            } else {
                return("Escriba un valor aceptable en capitalizacion de la tasa")
            }
            
        }
        
        
        #### Tabla Amortizacion
        TablaA_function <- function(L,tasa,anios,cap){
            
            ## quitar notacion cientifica
            options(scipen=999)  
            
            #### Numero de periodos de capitalizacion al anio
            # la variable cap representa los periodos de capitalizacion
            # 12 = mensual (12 periodos al anio)
            # 6 = semestral (2 periodos al anio)
            # 4 = cuatrimestral (3 periodos al anio)
            # 3 = trimestral (4 periodos al anio)
            # 2 = bimestral (6 periodos al anio)
            # 1 = anual (1 periodo)
            
            #if (cap==12) {
            #    perido <- 12
            #} else if (cap == 6) {
            #    periodo <- 2
            #} else if (cap == 4) {
            #    periodo <- 3
            #} else if (cap == 3) {
            #    periodo <- 4
            #} else if (cap == 2) {
            #    periodo <- 6
            #} else if (cap == 1) { 
            #    periodo <- 1  
            #} else {
            #    return("Escriba un valor aceptable en capitalizacion de la tasa")
            #}
            
            periodo <- Periodo_Func(cap)
            Rt <- R_function(L,tasa,anios,cap,'si')
            
            # v
            tasa <- tasa/100
            i <- tasa/periodo
            v <- 1/(1+i)
            n <- anios*periodo
            
            ### Vectores 
            # interes
            It <- NULL
            # Amortizacion
            Pt <- NULL
            # Saldo insoluto
            Bt <- NULL
            # Pagos
            R <- c(0,rep(Rt,n))
            
            ### Primer valor de los vectores
            It[1]<-0
            Pt[1] <- 0
            Bt[1] <- L
            t <- 0:n
            
            # contador para ciclo
            k <- 1
            
            ### Ciclo para crear tabla
            for (k in 1:n) {
                
                interest <- i*Bt[k]
                
                It[k+1] <- interest
                Pt[k+1] <- Rt-It[k+1]
                Bt[k+1] <- Bt[k]- Pt[k+1]
                
            }
            
            
            ## creacion del data frame de la tabla
            ## .. de amortizacion
            Amortizacion.Tabla <- data.frame(
                "Periodo_t"=c(t),
                "Pagos_Rt"=c(R),
                "Intereses.Pagados_It" = c(round(It,2)),
                "Amortizacion.Deuda_Pt" = c(round(Pt,2)),
                "Saldo.por.Pagar_Bt"= c(round(Bt,2))
            )
            
            
            return(Amortizacion.Tabla)
            
        }   # <--- FIN TablaA_Function() Funcion tabla de amortizacion 
    
        
        ### Funcion para crear grafica
        Graf_A <- function(L,tasa,anios,cap){
            
            # Crear tabla de amortizacion
            TAP <-TablaA_function(L,tasa,anios,cap)
            Pago.interes <- TAP$Pagos_Rt + TAP$Intereses.Pagados_It
            Pago.interes <- cumsum(Pago.interes)
            Pagos.sum <- cumsum(TAP$Pagos_Rt)
            TAP2<- cbind(TAP,Pago.interes,Pagos.sum)
            
            CapPeriodo <- TipoPago_Function(cap)
            
            # crear grafica
            Graf <- ggplot(data=TAP2, aes(x=Periodo_t)) +
                geom_area(aes(y=Saldo.por.Pagar_Bt,fill="Loan Balance"),alpha=.5)+
                geom_area(aes(y=Pago.interes,fill="Pagos + intereses"),alpha=.5)+
                geom_area(aes(y=Pagos.sum,fill="Pagos"),alpha=.5)+
                geom_line(aes(y=Saldo.por.Pagar_Bt),col="black")+
                geom_line(aes(y=Pago.interes),col="black")+
                geom_line(aes(y=Pagos.sum),col="black")+
                
                ### Disenio grafica
                xlab('Monto')+
                ylab('Periodo')+
                
                ggtitle("Gráfica de amortización de deuda", 
                        subtitle = paste("Monto: ",L,"| Periodo: ",
                        anios, " año(s) | Capitalizable ",CapPeriodo))+
                
                theme_test() + 
                theme(plot.title = element_text(color="black", 
                                                size = 12,face="bold"), 
                      plot.subtitle = element_text(size = 10, 
                                                   face = "bold",
                                                   color = "black"),
                      legend.title = element_text(size = 10,face = 'bold'),
                      axis.text.x = element_text(color="black"),
                      axis.text.y = element_text(color = "black"))+
                guides(fill=guide_legend(""))
            
            return(Graf)
            
        } # <--- FIN Graf_A() Funcion para crear grafica de amortizacion de deuda
        
        
    ### Calculos (Amortizacion de deuda) -----
        
        ### Actualizacion Datos
        observeEvent(input$monto,{
            ## Inicializar variables
            L <- input$monto
            updateNumericInput(session,"monto",value = L)
            
        })
        
        observeEvent(input$tasa,{
            tasa <- input$tasa
            updateNumericInput(session,"tasa",value = tasa)
            
        })
        
        observeEvent(input$anios,{
            anios <- input$anios
            updateNumericInput(session,"anios",value = anios)
        })
        
        observeEvent(input$cap,{
            cap <- as.numeric(input$cap) 
            # Texto tipo de pago
            updateNumericInput(session,"cap",value = cap)
            TP <- TipoPago_Function(input$cap)
            
        })
        
        ### Value Box del Pago
        output$Pagos <- renderValueBox({
            
            ## Inicializar variables
            L <- input$monto
            tasa <- input$tasa
            anios <- input$anios
            cap <- input$cap
            # Texto tipo de pago
            TP <- TipoPago_Function(input$cap)
            
            
            ## Pagos
            pagos.M <- R_function(L,tasa,anios,cap,valor='no')  # El Resultado son los pagos anuales

            
            ##### Box
            valueBox( 
                round(pagos.M,2),
                paste0('Pago'," ",TP), icon = icon('credit-card',lib = 'glyphicon'), 
                color = 'yellow'
                
            )
            
            
        }) # <-- FIN output$Pagos
        
    
    ### Crear tabla de amortizacion
        
        output$Tabla <- renderValueBox({
            
            ## Inicializar variables
            L <- input$monto
            tasa <- input$tasa
            anios <- input$anios
            cap <- input$cap
            
            # Crear tabla de amortizacion
            TAP <-TablaA_function(L,tasa,anios,cap)
            
            ### Crear otras variables acumulativas
            # Suma de los pagos e intereses
            Pago.interes <- TAP$Pagos_Rt + TAP$Intereses.Pagados_It
            Pago.interes <- cumsum(Pago.interes)
            # suma de solo de pagos
            Pagos.sum <- cumsum(TAP$Pagos_Rt)
            # Suma solo de intereses
            Solo.Intereses <- cumsum(TAP$Intereses.Pagados_It)
            
            DATATable<-as.data.frame(TAP)
            
            valueBox(DATATable)
             
            
        })
    
        
    ### Calculo monto con intereses final VP (Anualidad vencida) -------
        
        ### Value Box del monto total con intereses
        output$TotalPago <- renderValueBox({
            
            ## Inicializar variables
            L <- input$monto
            tasa <- input$tasa
            anios <- input$anios
            cap <- input$cap
            periodo <- Periodo_Func(cap)
            # Crear tabla de amortizacion
            TAP <-TablaA_function(L,tasa,anios,cap)
            
            ### Crear otras variables acumulativas
            # Suma de los pagos e intereses
            Pago.interes <- TAP$Pagos_Rt + TAP$Intereses.Pagados_It
            # cumsum() regresa un vector
            Pago.interes <- cumsum(Pago.interes)
            # Obtenemos el ultimo elemento de la suma acumulativa
            SumaPagoIntereses <- as.numeric(Pago.interes)[anios+1]
            SumaPagoIntereses <- as.numeric(Pago.interes) 
            ult <- length(SumaPagoIntereses)
            SumaPagoIntereses<- SumaPagoIntereses[ult]
            
            #### box
            valueBox( 
                round(SumaPagoIntereses,2),
                'Pago Total', icon = icon('credit-card',lib = 'glyphicon'), 
                color = 'purple'
                
            )  
            
        }) # <-- Fin utput$TotalPago
                
            
    ### Calculo intereses (Anualidad vencida) -------
        
        ### Value Box del monto total con intereses
        output$Intereses <- renderValueBox({
            
            ## Inicializar variables
            L <- input$monto
            tasa <- input$tasa
            anios <- input$anios
            cap <- input$cap
            
            # Crear tabla de amortizacion
            TAP <-TablaA_function(L,tasa,anios,cap)
            # Suma solo de intereses. cumsum() regresa un vector.
            # Obtenemos el ultimo elemento del vector
            #Solo.Intereses <- cumsum(TAP$Intereses.Pagados_It)[anios+1]
            Solo.Intereses <- cumsum(TAP$Intereses.Pagados_It) 
            ult <- length(Solo.Intereses)
            Solo.Intereses<- Solo.Intereses[ult]
            
            
            #### box
            valueBox( 
                round(Solo.Intereses,2),
                'Intereses', icon = icon('credit-card',lib = 'glyphicon'), 
                color = 'blue'
                
            )  
            
            
        }) # <-- Fin output$Intereses
        
    
    ### Graficas -----
      
        output$graf <- renderPlot({
          ## Inicializar variables
          L <- input$monto
          tasa <- input$tasa
          anios <- input$anios
          cap <- input$cap
          periodo <- Periodo_Func(cap)

          Graf_A(L,tasa,anios,cap)
          
        })  

    
    ### IMGENES  -----
        # REFERENCIA:
        #filename <- normalizePath(file.path('./images',
        #                                    paste('image', input$n, '.jpeg', sep='')))
        # https://rdrr.io/cran/shiny/man/renderImage.html
        
        # Ruta de la carpeta de imagenes
        
        ## portada
        output$imgportada <- renderImage({
          list(src="img/portada.png", 
               filetype = "image/png",
               width = "100%",
               height = 320)
        })
        
        ## blog 1 
        output$imgblog <- renderImage({
          list(src="img/blog.png", 
               filetype = "image/png",
               width = "100%",
               height = 320)
        })
        
        ## blog 2
        output$imgblog2 <- renderImage({
          list(src="img/blog2.png", 
               filetype = "image/png",
               width = "100%",
               height = 320)
        })
        
        ## formula de pagos
        output$imgpagos <- renderImage({
          
          list(src="img/pagos2.png", 
               filetype = "image/png",
               width = "100%",
               height = 300)
          
        })
        
        ## formula saldo insoluto
        output$imgsi <- renderImage({
          
          list(src="img/saldo_insoluto.png", 
               filetype = "image/png",
               width = "100%",
               height = 300)
          
        })
        
        ## formula intereses
        output$imgint <- renderImage({
          
          list(src="img/intereses.png", 
               filetype = "image/png",
               width = "100%",
               height = 300)
          
        })
        
        
        ## formula amortizacion
        output$imgamo <- renderImage({
          
          list(src="img/amortizacion.png", 
               filetype = "image/png",
               width = "100%",
               height = 300)
          
        })
        
    ### Links ----
      
    output$link <- renderUI({
      url<- a("Blog page 1",href="https://laurabetancourtlea.wixsite.com/laurabetancourt/post/loan-calculator")
      tagList("Link:",url)
    })
        
    output$link2 <- renderUI({
      url<- a("Blog page 2",href="https://laurabetancourtlea.wixsite.com/laurabetancourt/post/loan-calculator-parte-ii")
      tagList("Link:",url)
    })   
        
        
            
}

# Corren la APP
shinyApp(ui = ui, server = server)


#} #<-- fin interactive()


