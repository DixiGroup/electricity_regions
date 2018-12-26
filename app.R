#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(readr)
library(ggplot2)
library(png)
library(gtable)
library(gridExtra)
library(grid)
library(stringr)


# Define UI for data upload app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Генератор регіональної інфографіки щодо електроенергії"),
    
    # Sidebar layout with input and output definitions ----
    useShinyjs(),
    column(7,
           
           fluidRow( 
               
               sidebarPanel(
                   
                   # Input: Select a file ----
                   fileInput("file1", "Оберіть csv-файл з даними. Розділювач - кома, кодування - UTF-8",
                             multiple = FALSE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")),
                   tags$br(),
                   disabled(sliderTextInput("date_range", "Перший і останній місцяць", 
                                            choices = c("січень 2018", "вересень 2018"), selected = c("січень 2018", "вересень 2018"))),
                   tags$br(),
                   disabled(selectInput("cons_category", label = "Оберіть категорію споживачів", choices = "Усі")),
                   disabled(selectInput("region", label = "Регіон", choices = "Вся Україна")),
                   selectInput("variable_name", label = "Оберіть змінну", choices = c("Накопичений борг, грн" = "debt_month_start", 
                                                                          "Споживання, кВт/год" = "consumption_tkwth", "Споживання, грн" = "consumption_tuah",
                                                                          "Рівень оплати, %" = "payment_percent")),
                   radioButtons("map_type", "Тип графіка: ", choices = c("лінійний графік" = "line", "порівняння стовпчиками" = "bar"))
                   #helpText("Мапа генерується і змінюється тільки після натискання кнопки знизу:"),
                   #fluidRow(disabled(actionButton("generate_button", "Згенерувати мапу", width = "100%")))
                   
               ),
               
               column(width = 8,
                      plotOutput("regional_plot", width = "750px",height =  "500px" ),
                      hidden(checkboxGroupInput("cats", label = "", choices = "", inline = TRUE)))
           ),
           wellPanel(
               fluidRow( #tags$hr(),
                   helpText("Після того, як графіка згенерувалася, ви можете завантажити її на диск."),
                   column(1, 
                          radioButtons("extension", "Формат: ", choices = c("png", "pdf"))),
                   column(2,
                          radioButtons("goal", "Призначення: ", choices = c("facebook" = "fb", "презентація" = "pres"))),
                   column(3, 
                          textInput("customtitle", label = "Заголовок у файлі")),
                   column(3, 
                          textInput("customsubtitle", label = "Підзаголовок у файлі")),
                   
                   column(2,
                          disabled(downloadButton('downloadData', 'Завантажити мапу')))))
           
    )
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
    logo <- readPNG("qr_code_small.png")
    source("functions.R")
    system("mkdir ~/.fonts")
    download.file(url = "https://github.com/localizator/ukrainian-fonts-pack/raw/master/BlissPro-Light%20-%20Bliss%20Pro%20-%20Light.ttf",
                  destfile = "~/.fonts/Bliss Pro Light.ttf")
    system('fc-cache -fEV ~/.fonts') 
    
    # reaction on uploading file
    upload_react <- observeEvent(input$file1, {
        req(input$file1)
        tryCatch(
            {
                debt <- read_delim(input$file1$datapath, delim = ",", locale = locale(decimal_mark = ".", encoding = "UTF-8"))
                debt <- debt %>% 
                    dplyr::select(-company, -debt_year_start) %>% 
                    dplyr::select(year:paid_month_tuah) %>%  
                    dplyr::group_by(year, month, oblast, consumer_cat, consumer_type) %>% 
                    #group_by(-consumption_tkwth, -consumption_tuah, -debt_month_start) %>% 
                    dplyr::summarise(paid_month_tuah = sum(paid_month_tuah),
                                     consumption_tkwth = sum(consumption_tkwth),
                                     consumption_tuah = sum(consumption_tuah),
                                     debt_month_start = sum(debt_month_start)) %>% 
                    dplyr::ungroup() %>% 
                    dplyr::arrange(year, month) %>% 
                    dplyr::mutate(month_string = paste(ua_monthes[month], year))
                # активуємо деактивовані елементи вводу
                enable("date_range")
                enable("cons_category")
                enable("region")
                # Генеруємо список категорій
                cat_list = c("Усі", unique(debt$consumer_cat))
                #cat_list <- lapply(cat_list, function(x) c(x = x))
                #names(cat_list) <- cat_list
                #print(cat_list)
                updateSelectInput(session = session, inputId = "cons_category", choices = cat_list)
                # завантажуємо список областей
                updateSelectInput(session = session, "region", choices = c("Вся Україна", sort(debt$oblast)))
                
                # змінюємо значення слайдера виходячи з наявних дат
                slider_dates <-  unique(debt$month_string)
                updateSliderTextInput(session = session, "date_range", choices = slider_dates, selected = c(slider_dates[length(slider_dates)-1], slider_dates[length(slider_dates)]))
                debt <<- debt
                #reaction on changing category
                cat_react <- observeEvent(input$cons_category, {
                    category <- input$cons_category
                    if (category != "Усі") {
                        debt <- debt %>% 
                            filter(consumer_cat == category)
                        subcats <- unique(debt$consumer_type)
                    } else {
                        subcats <- unique(debt$consumer_cat)
                    }
                    updateCheckboxGroupInput(session, inputId = "cats", choices = subcats, selected = subcats, inline = TRUE)
                }
                )
                
                # plot updating 
                output$regional_plot <- renderPlot({
                    var <- input$variable_name
                    dates_selected <- input$date_range
                    period1 <- strsplit(dates_selected[1], " ")[[1]]
                    month1 <- which(ua_monthes == period1[1])
                    year1 <- as.integer(period1[2])
                    period2 <- strsplit(dates_selected[2], " ")[[1]]
                    month2 <- which(ua_monthes == period2[1])
                    year2 <- as.integer(period2[2])
                    category <- input$cons_category
                    if (length(input$cats) > 0) 
                    {
                        if (category == "Усі") {
                            debt <- debt %>% 
                                filter(consumer_cat %in% input$cats)
                        } else {
                            debt <- debt %>% 
                                filter(consumer_type %in% input$cats)
                        }
                        region <- input$region
                        pl <<- switch(input$map_type,
                                      line = linegraph(debt, var, region, category, year1, month1, year2, month2),
                                      bar = bargraph(debt, var, region, category, year1, month1, year2, month2))
                        enable("downloadData")
                        show("cats")
                        updateTextInput(session, inputId = "customtitle", value = pl$labels$title)
                        updateTextInput(session, inputId = "customsubtitle", value = pl$labels$subtitle)
                        pl
                    } else {
                        ggplot()
                    }

                })
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            return(paste("graph", input$extension, sep = "."))
        }, content = function(file) {
            pl <- pl + ggtitle(isolate(input$customtitle), isolate(input$customsubtitle))
            p <- ggplot_build(pl)
            pl_local <- ggplot_gtable(p)
            caption_row  <- pl_local$layout$b[pl_local$layout$name == "caption"]
            if (input$goal == "fb") {
                w = 8.5
                h = 4.85
                dp = 130
                logo_side <- h / 10 
                if (isolate(input$map_type) == "line") {
                    hjust_cor <- 0.2
                    vjust <- 1.35
                    leftest_panel <- min(pl_local$layout$l[grepl("axis-l", pl_local$layout$name)])
                    hjust <- 0.5
                } else {
                    hjust_cor <- 1
                    vjust <- 1.15
                    leftest_panel <- min(pl_local$layout$l[grepl("panel", pl_local$layout$name)])
                    hjust <- (w / 2) / logo_side + hjust_cor
                }
                
            } else {
                w = 8
                h = 5.66
                dp = 300
                logo_side <- h / 10 
                if (isolate(input$map_type) == "line") {
                    hjust_cor <- 0.15
                    vjust <- 1.25
                    leftest_panel <- min(pl_local$layout$l[grepl("axis-l", pl_local$layout$name)])
                    hjust <- 0.5
                    
                } else {
                    hjust_cor <- 0.87
                    vjust <- 1.05
                    leftest_panel <- min(pl_local$layout$l[grepl("panel", pl_local$layout$name)])
                    
                    hjust <- (w / 2) / logo_side + hjust_cor
                }
            }
            #p <- ggplot_build(pl)
            #y_labels <- p$layout$panel_ranges[[1]]$y.labels
            #if (isolate(input$map_type) == "line") {
            #    hjust_cor <- hjust_cor - 0.25 * (7 - max(nchar(y_labels)))
            #}
            #margin_correction <- 0.2 + 0.15 * (7 - max(nchar(y_labels)))
            #print(margin_correction)
            #p <- ggplot_build(pl + theme(plot.margin = unit(c(0.2,1,0.2,margin_correction), "cm")))
            #print(hjust_cor)
            #hjust_cor <- 1
            #pl_local <- ggplot_gtable(p)
            #leftest_panel <- min(pl_local$layout$l[grepl("panel", pl_local$layout$name)])
            #leftest_panel <- min(pl_local$layout$l[grepl("axis-l", pl_local$layout$name)])
            #print(leftest_panel)
            caption_row  <- pl_local$layout$b[pl_local$layout$name == "caption"]
            pl_local <- gtable_add_grob(pl_local, rasterGrob(logo), caption_row - 2, leftest_panel, caption_row, leftest_panel, name = "logo")
            #logo_side <- h / 10 
            pl_local$grobs[[which(pl_local$layout$name == "logo")]]$height <- unit(logo_side, "in")
            pl_local$grobs[[which(pl_local$layout$name == "logo")]]$width <-  unit(logo_side, "in")
            if (input$extension == "pdf") {
                dev <- cairo_pdf
            } else {
                dev <- "png"
            }
            #hjust <- (w / 2) / logo_side + hjust_cor
            #hjust <- 0.5
            pl_local$grobs[[which(pl_local$layout$name == "logo")]]$hjust <- hjust
            pl_local$grobs[[which(pl_local$layout$name == "logo")]]$vjust <- vjust
            pl_local$layout$clip[which(pl_local$layout$name == "logo")] <- "off"
            ggsave(file, pl_local, width = w, height = h, dpi = dp, device = dev)
        }
    )

}

# Run the application 
shinyApp(ui = ui, server = server)

