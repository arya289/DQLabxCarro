# Library Needed
# Dashboard
library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(shinyWidgets)
# Data Wrangling
library(dplyr)
library(tidyr)
library(lubridate)
# Data Visualization
library(ggplot2)
library(plotly)
library(ggthemes)
library(ggmosaic)
library(ggalt)
library(scales)
library(RColorBrewer)

carro <- read.csv("carro4.csv")
carro2 <- read.csv("carro3.csv")


ui <-  dashboardPage(skin = "red", 
                     dashboardHeader(title = "Carro Dashboard Analytics"),
                     
                     dashboardSidebar(
                         sidebarMenu(
                             menuItem("Home", tabName = "tab_home", icon = icon("home")),
                             menuItem("Price Analysis", icon = icon("chart-line"), tabName = "tab_price"),
                             menuItem("Top Status Analysis", icon = icon("cog"), tabName = "tab_status"),
                             menuItem("Prediction Analysis", icon = icon("usd"), tabName = "tab_prediction")
                         ) # end of sidebar Menu
                     ), # end of dashboard Sidebar
                     
                     dashboardBody(
                         tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: red}")),
                         tabItems(
                             
                             tabItem(
                                 tabName = "tab_home",
                                 img(src='id_meta_jual.jpg', align = "center"),
                                 br(),
                                 br(),
                                 h1("Welcome to Carro Dashboard Analytics", align = "Center",style = "color: red"),
                                 br(),
                                 h3("Please Select the Sidebar Menu to Start Your Analysis", align = "Center", style = "color: orange")
                             ), # end of Home Tab Item
                             
                             
                             tabItem(
                                 tabName = "tab_price",
                                 h4("Prince Range Analysis"),
                                 fluidRow(
                                     column(3,
                                            wellPanel(
                                                selectInput("brandID",
                                                            "Select Car Brand:",
                                                            choices = unique(carro$MEREK),
                                                            width = 200)
                                            )
                                            
                                     ),
                                     
                                     column(3,
                                            wellPanel(
                                                selectInput(inputId = "modelID",
                                                            label = "Select Model:",
                                                            choices = NULL,
                                                            width = 200)
                                            )
                                            
                                     ),
                                     
                                     column(3,
                                            wellPanel(
                                                selectInput(inputId = "transmisiID",
                                                            label = "Select Transmission:",
                                                            choices = NULL,
                                                            width = 200)
                                            )
                                            
                                     ),
                                     
                                     column(3,
                                            wellPanel(
                                                sliderInput("yearRange",
                                                            "Year:",
                                                            min = 1999,
                                                            max = 2019,
                                                            value = 2018)))
                                     
                                 ),
                                 
                                 fluidRow(
                                     box(
                                         title = "Price Range (Awal vs Panduan)",
                                         status = "info",
                                         solidHeader = TRUE,
                                         collapsible = TRUE,
                                         plotOutput("price", height = "500px")
                                     ),
                                     
                                     box(
                                         title = "Price Range (Terendah vs Tertinggi)",
                                         status = "warning",
                                         solidHeader = TRUE,
                                         collapsible = TRUE,
                                         plotOutput("price2", height = "500px")
                                     )
                                     
                                 )
                             ), # end Sales Tab Item
                             
                             
                             tabItem(
                                 tabName = "tab_status",
                                 h4("Top Status Analysis"),
                                 fluidRow(
                                     column(6,
                                            wellPanel(
                                                selectInput(inputId = "priceID",
                                                            label = "Select Price Parameter:",
                                                            choices = names(carro[ ,c(8,9,10)]),
                                                            width = 200)
                                            )
                                            
                                     ),
                                     
                                     column(6,
                                            wellPanel(
                                                dropdownButton(
                                                    inputId = "mydropdown",
                                                    label = "Select Car Brand:", 
                                                    status = "danger", 
                                                    width = 200,
                                                    circle = F,
                                                    checkboxGroupInput(inputId = "brandID2", 
                                                                       label = NULL, 
                                                                       choices = unique(carro$MEREK), 
                                                                       selected = c("CHEVROLET", "DAIHATSU", "DATSUN",
                                                                                    "ISUZU", "KIA", "MITSUBISHI",
                                                                                    "NISSAN", "SUZUKI"))
                                                )
                                            )
                                            
                                     )
                                 ),
                                 
                                 fluidRow(
                                     box(
                                         title = "Price Distribution among Top Status",
                                         status = "info",
                                         solidHeader = TRUE,
                                         collapsible = TRUE,
                                         plotlyOutput("status", height = "500px")
                                     ),
                                     
                                     
                                     
                                     box(
                                         title = "Proportion of Top Status in Every Brand",
                                         status = "warning",
                                         solidHeader = TRUE,
                                         collapsible = TRUE,
                                         plotlyOutput("status2", height = "500px")
                                     )
                                 ),
                                 
                                 
                                 fluidRow(
                                     box(
                                         title = "Boxplot of Top Status vs Price",
                                         status = "info",
                                         solidHeader = TRUE,
                                         collapsible = TRUE,
                                         plotlyOutput("status3", height = "500px")
                                     ),
                                     
                                     
                                     
                                     box(
                                         title = "Violin Plot Brand vs Price",
                                         status = "warning",
                                         solidHeader = TRUE,
                                         collapsible = TRUE,
                                         plotlyOutput("status4", height = "500px")
                                     )
                                 )
                                 
                             ), # end Top Status Tab Item
                             
                             tabItem(
                               tabName = "tab_prediction",
                               h4("Filtering based on Parameters"),
                               fluidRow(
                                         column(3,
                                                 wellPanel(
                                                     selectInput("brandID3",
                                                                 "Select Car Brand:",
                                                                 choices = unique(carro2$MEREK),
                                                                 width = 200)
                                                 )
                                                 
                                         ),
                                         
                                         column(3,
                                                wellPanel(
                                                    selectInput(inputId = "modelID3",
                                                                label = "Select Model:",
                                                                choices = NULL,
                                                                width = 200)
                                                )
                                                
                                         ),
                                         
                                         column(3,
                                                wellPanel(
                                                    selectInput(inputId = "transmisiID3",
                                                                label = "Select Transmission:",
                                                                choices = NULL,
                                                                width = 200)
                                                )
                                                
                                         ),
                                         
                                         column(3,
                                                wellPanel(
                                                    selectInput(inputId = "typeID3",
                                                                label = "Select Type:",
                                                                choices = NULL,
                                                                width = 200)
                                                )
                                                
                                           )
                                         ),
                                   
                                fluidRow(      
                                         
                                        column(3,
                                               wellPanel(
                                                   selectInput(inputId = "fuelID3",
                                                               label = "Select Fuel Type:",
                                                               choices = NULL,
                                                               width = 200)
                                               )
                                              ),
                                        
                                        column(3,
                                                wellPanel(
                                                    numericInput(inputId = "ccID3", 
                                                                 label = "Engine Volume (L):", 
                                                                 value = 1.0,
                                                                 min = 1.0, 
                                                                 max = 3.8, 
                                                                 step = 0.1) 
                                                )
                                              ),
                                         
                                         column(3,
                                                wellPanel(
                                                    sliderInput("yearRange3",
                                                                "Year:",
                                                                min = 1999,
                                                                max = 2019,
                                                                value = 2018))),
                                        
                                        column(3,
                                               wellPanel(
                                                   selectInput(inputId = "statusID3",
                                                               label = "Select Status:",
                                                               choices = NULL,
                                                               width = 200)
                                               )
                                        )
                               ),
                                           
                               
                               
                               
                               fluidRow(
                                        column(3,
                                            wellPanel(
                                                      h4("Prediksi Harga Panduan:"),
                                                      br(),
                                                      verbatimTextOutput("Pred")
                                                     )
                                                ),
                                        column(9,
                                               box(
                                                   title = "Rekomendasi",
                                                   status = "danger",
                                                   solidHeader = TRUE,
                                                   collapsible = TRUE,
                                                   width = 12,
                                                   h4("1. Untuk mendapakan gambaran harga yang lebih detail, 
                                                      ada baiknya dilakukan splitting dalam variabel/kolom `Type`, sehingga kita bisa mengetahui apakah ada 
                                                      pengaruh dari sisi CC dan Jenis BBM"),
                                                   br(),
                                                   h4("2. Variabel Top Status ada baiknya diganti dengan aktual rataan/median hari terjualnya mobil, 
                                                      mengingat jumlah Fast jauh lebih banyak dibanding Medium dan Slow, sehingga tidak memberikan
                                                      informasi yang signifikan (sulit membedakan pengaruh dari ketiganya terhadap harga)."),
                                                   br(),
                                                   h4("3. Mengingat justifikasi panelis expert masih dirasa sangat penting di industri ini, ada baiknya
                                                      variabel `POOL LEADER` dilengkapi datanya sehingga bisa digunakan juga sebagai prediktor, atau bisa juga
                                                      untuk membandingkan hasil Prediksi dengan hasil panelis expert."),
                                                   br(),
                                                   h4("4. Prediksi dengan metode OOM tidak disarankan karena hanya menggunakan koefisien yang dipukul rata, sehingga
                                                      menihilkan kemungkinan pengaruh berbagai variabel lainnya."),
                                                   br(),
                                                   h4("5. Variabel lain yang cukup penting dalam prediksi harga namun belum masuk adalah jarak tempuh.")
                                                   )
                                               )

                             )
                             
                        )# end Prediction Tab Item
                             
                             
                             
                         ) # end Tab Items
                     )# end of Dashboard Body
)# end of Dashboard Page




server <- function(input, output, session) {
    
    load('chevrolet.rda')
    load('daihatsu.rda')
    load('datsun.rda')
    load('isuzu.rda')
    load('kia.rda')
    load('mitsubishi.rda')
    load('nissan.rda')
    load('suzuki.rda')
    
    # Tab Price
    observeEvent(input$brandID,{
        updateSelectInput(session,'modelID',
                          choices=unique(carro$MODEL[carro$MEREK==input$brandID]))
    }) 
    
    observeEvent(input$modelID,{
        updateSelectInput(session,'transmisiID',
                          choices=unique(carro$TRANSMISI[carro$MEREK==input$brandID & 
                                                         carro$MODEL==input$modelID]))
    })   
    
    # Tab Prediction
    observeEvent(input$brandID3,{
        updateSelectInput(session,'modelID3',
                          choices=unique(carro2$MODEL[carro2$MEREK==input$brandID3]))
    }) 
    
    observeEvent(input$modelID3,{
        updateSelectInput(session,'transmisiID3',
                          choices=unique(carro2$TRANSMISI[carro2$MEREK==input$brandID3 & 
                                                          carro2$MODEL==input$modelID3]))
    }) 
    
    
    observeEvent(input$modelID3,{
        updateSelectInput(session,'typeID3',
                          choices=unique(carro2$TYPE[carro2$MEREK==input$brandID3 & 
                                                     carro2$MODEL==input$modelID3]))
    }) 
    
    
    observeEvent(input$modelID3,{
        updateSelectInput(session,'fuelID3',
                          choices=unique(carro2$FUEL[carro2$MEREK==input$brandID3 & 
                                                     carro2$MODEL==input$modelID3]))
    })
    
    observeEvent(input$modelID3,{
        updateSelectInput(session,'statusID3',
                          choices=unique(carro2$TOP.STATUS[carro2$MEREK==input$brandID3 & 
                                                           carro2$MODEL==input$modelID3]))
    })

    data <- reactive({
            data.frame(MEREK=input$brandID3,
                       MODEL=input$modelID3,
                       TRANSMISI=input$transmisiID3,
                       TYPE=input$typeID3,
                       FUEL=input$fuelID3,
                       TOP.STATUS=input$statusID3,
                       TAHUN=input$yearRange3,
                       CC=input$ccID3)
            })
    
    model <- reactive({
        # Error handling 
        if (is.null(input$brandID3) 
        ) {
            return(NULL)
        }
        
        if (input$brandID3 == "CHEVROLET") {
            return(chev_lm)
        } else if (input$brandID3 == "DAIHATSU") {
            return(daih_lm)
        } else if (input$brandID3 == "DATSUN") {
            return(dats_lm)
        } else if (input$brandID3 == "ISUZU") {
            return(isuz_lm)
        } else if (input$brandID3 == "KIA") {
            return(kia_lm)
        } else if (input$brandID3 == "MITSUBISHI") {
            return(mits_lm)
        } else if (input$brandID3 == "NISSAN") {
            return(niss_lm)
        } else {
            return(suzu_lm)
        } 

            })

    pred <- reactive({
        predict(model(),data())
            })
    
    
    # Set Reactive Values
    filteredcarro <- reactive({ 
        
        # Error handling 
        if (is.null(input$brandID) |
            is.null(input$modelID) |
            is.null(input$transmisiID)
        ) {
            return(NULL)
        }
        
        carro %>%
            filter(TAHUN == input$yearRange,
                   MEREK %in% input$brandID,
                   MODEL %in% input$modelID,
                   TRANSMISI %in% input$transmisiID
            )
    })
    
    
    # Set Reactive Values
    filteredcarro2 <- reactive({ 
        
        # Error handling 
        if (is.null(input$brandID2) |
            is.null(input$priceID) 
        ) {
            return(NULL)
        }
        
        carro %>%
            filter(MEREK %in% input$brandID2)
        
        
    })
    
    
    output$Pred <- renderPrint(paste("Rp. ", comma(pred())))
    
    
    output$price <- renderPlot ({
        
        
        
        carro2 <- filteredcarro() %>% 
            select(TYPE, HARGA.PANDUAN, HARGA.TERENDAH) %>% 
            gather(group, value, -TYPE)
        
        
        ggplot(filteredcarro(), aes(y = TYPE)) + 
            geom_point(data = carro2, aes(x = value, color = group), size = 7) +
            geom_dumbbell(aes(x = HARGA.TERENDAH, xend= HARGA.PANDUAN),
                          colour = "grey60", 
                          size = 3,
                          colour_x = "red", 
                          colour_xend = "#395B74") +
            theme(legend.position="top") +
            scale_color_manual(name = "", values = c("#395B74", "red")) +
            scale_x_continuous(labels = scales::comma, name = "Harga (Rupiah)")
        
        
    })  
    
    output$price2 <- renderPlot ({
        
        
        
        carro2 <- filteredcarro() %>% 
            select(TYPE, HARGA.PANDUAN, HARGA.TERTINGGI) %>% 
            gather(group, value, -TYPE)
        
        
        ggplot(filteredcarro(), aes(y = TYPE)) + 
            geom_point(data = carro2, aes(x = value, color = group), size = 7) +
            geom_dumbbell(aes(x = HARGA.PANDUAN, xend= HARGA.TERTINGGI),
                          colour = "grey60", 
                          size = 3,
                          colour_x = "#395B74", 
                          colour_xend = "limegreen") +
            theme(legend.position="top") +
            scale_color_manual(name = "", values = c("#395B74", "limegreen")) +
            scale_x_continuous(labels = scales::comma, name = "Harga (Rupiah)")
        
        
    })
    
    
    output$status <- renderPlotly ({
        
        plot1 <-  ggplot(filteredcarro2(), 
                         aes(x=filteredcarro2()[[input$priceID]], 
                             fill = TOP.STATUS)) +
            geom_histogram(alpha = 0.8) +
            scale_x_continuous(labels = scales::comma, 
                               name = "Harga (Rupiah)") +
            theme(legend.position = "none")
        
        
        ggplotly(plot1)
        
    }) 
    
    
    
    output$status2 <- renderPlotly ({
        
        plot2 <- ggplot(filteredcarro2()) +
            geom_mosaic(aes(x = product(MEREK), fill=TOP.STATUS)) +
            labs(x = NULL, y = NULL) +
            theme_igray() +
            theme(legend.position = "none",
                  axis.text.x = element_text(size = 7))
        
        ggplotly(plot2)
        
    }) 
    
    
    output$status3 <- renderPlotly ({
        
        plot3 <- ggplot(filteredcarro2()) +
            geom_boxplot(aes(x = TOP.STATUS, y = filteredcarro2()[[input$priceID]], fill = TOP.STATUS)) +
            labs(x = NULL, y = NULL) +
            scale_y_continuous(labels = scales::comma, name = "Harga (Rupiah)") +
            theme(legend.position = "none",
                  axis.text.x = element_text(size = 7))
        
        ggplotly(plot3)
        
    }) 
    
    
    
    output$status4 <- renderPlotly ({
        
        plot4 <- ggplot(filteredcarro2()) +
            geom_violin(aes(x = MEREK, y = filteredcarro2()[[input$priceID]], fill = MEREK)) +
            labs(x = NULL, y = NULL) +
            scale_y_continuous(labels = scales::comma, name = "Harga (Rupiah)") +
            theme(legend.position = "none",
                  axis.text.x = element_text(size = 7))
        
        ggplotly(plot4)
        
    }) 
    
    

} # end of Server











shinyApp(ui = ui, server = server)