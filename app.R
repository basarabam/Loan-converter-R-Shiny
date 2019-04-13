#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library("tidyverse")
library("tvm")
library("Quandl")
library("lubridate")
library("xts")
library("readxl")
library("openxlsx")

library(shiny)
library(shinythemes)

#Loading data from files and Quandle, you should add Quandle key for more than 50 queries per day
Quandl.api_key("j3w9njL3_zx3eNfThFZQ")
#Reading file of middle RSD/EUR exchange rate and converting to Data Frame
exc_eur_rsd <- as.data.frame(read_csv2("Data_read/Sred_kurs_NBS.csv"))
exc_eur_rsd <- exc_eur_rsd %>% select(Datum_Primene, Srednji_kurs)
exc_eur_rsd$Datum_Primene <- as.Date(exc_eur_rsd$Datum_Primene, format = "%d.%m.%Y.")

#Loading data from Quandle
exc_rate <- Quandl("ECB/EURCHF")

# Define UI for application that calculates loan amortization
ui <- fluidPage(theme = shinytheme("flatly"),
        
   # Application title
   titlePanel("CHF to EUR loan converter"),
   
   # Sidebar with a slider input for loan data 
   navbarPage("Calculator!",
        tabPanel("CHF loan Info",
        sidebarLayout(
        sidebarPanel(
         numericInput("interest_rate",
                     "Interest rate on CHF loan %:",
                     min = 3.001,
                     max = 8.001,
                     value = 5.001,
                     step = 0.001),
      
         numericInput("number_of_periods",
                   "Number of loan periods (months):",
                   min = 1,
                   max = 360,
                   value = 120,
                   step = 1),
         numericInput("loan_amount",
                      "Loan Amount in CHF:",
                      min = 1000,
                      max = 1000000,
                      value = 10000,
                      step = 1000),
         dateInput("loan_date",
                   "Loan origination date (start date):",
                   min = "2005-11-30",
                   max = "2019-04-06",
                   value = "2005-11-30",
                   format = "dd.mm.yyyy."),
         dateInput("loan_first_p_date",
                   "First payment due date:",
                   min = "2005-11-30",
                   max = "2019-04-06",
                   value = "2005-11-30",
                   format = "dd.mm.yyyy."),
         #File with payments made in rsd, according to provided xlsx template
         fileInput("placanja_rsd_file",
                   "File with payments made with XLSX template, RSD:",
                   accept = c(".xlsx")),
         #Part for downloading xlsx file template to be filled by the user
         p("Download the Excel template Workbook, to fill the payments in RSD and upload to this converter:"),
         tags$a(href='template_excel_input.xlsx', target='blank',
                'Download file upload template',
                download = 'Template_chf_loan.xlsx'),
         br(),
         br(),
         #Download the calculation as a excel workbook
         p("Download data in Excel workbook:"),
         downloadButton("downloadData", "Download in Excel")
   ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tabsetPanel( type = "tabs",
                      tabPanel("Loan Summary", htmlOutput("summary")),
                      tabPanel("Amortization plan", DT::dataTableOutput("amortPlan")),
                      tabPanel("Loan Plot", plotOutput("loan_plot")),
                      tabPanel("Payments RSD", DT::dataTableOutput("loaded_data"))
         
                )
              )
           )
        ),
   #Conversion input panel - check for consiste check for working
   tabPanel("EUR Reprogram Info",
            sidebarLayout(
                  sidebarPanel(
                          numericInput("interest_rate_eur",
                                       "Interest rate on EUR loan %:",
                                       min = 3.001,
                                       max = 8.001,
                                       value = 3.5,
                                       step = 0.001),
                          #Number of periods for reprogram
                          numericInput("number_of_periods_rep",
                                       "Number of loan periods (months):",
                                       min = 1,
                                       max = 360,
                                       value = 120,
                                       step = 1),
                          #Loan amount for reprogram
                          numericInput("loan_amount_rep_chf",
                                       "Loan Amount for reprogram in CHF:",
                                       min = 1000,
                                       max = 1000000,
                                       value = 10000,
                                       step = 1000),
                          dateInput("repr_date",
                                    "Loan reprogram date:",
                                    min = "2005-03-30",
                                    max = Sys.Date(),
                                    value = "2005-11-30",
                                    format = "dd.mm.yyyy."),
                          numericInput("wr_off_loan",
                                       "Loan write off %:",
                                       min = 0,
                                       max = 100,
                                       value = 40,
                                       step = 1)
                  ),
                  mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Reprogram info", htmlOutput("infoEUR"))
                                  
                          )
                          
                  )
            )
            ),
                    
   tabPanel("Help",
            tags$h1("This is the help text"),
            tags$br("Help text coming soon !")
   )
   #Navbar close parentheses
   )


)

# Define server logic 
server <- function(input, output) {

#Definig reactive output for first panel CHF
        #creating reactive EUR/CHF exchange rate on loan day
        exc_rat <- reactive({
         eur_chf_exc <- exc_rate %>% filter(Date == input$repr_date) %>% select(Value) %>% pull()
         eur_chf_exc
        })
        #converting loan amount to euro. This should be changed
        l_am_eur <- reactive({
                loan_am_eur <- round(input$loan_amount /exc_on_loan_day(), 2)
                loan_am_eur
        })
        #creating reactive monthly payment chf
        month_pay_chf <- reactive({
                m_p_chf <- pmt(amt = input$loan_amount, maturity = input$number_of_periods, rate=input$interest_rate/100/12)
        })
   
  
        
        # creating reactive table data chf
        t_p <- reactive({
                # generate exchange rate EUR/CHF on loan day
                
                #exc_on_loan_day <- exc_rate %>% filter(Date == input$loan_date) %>% select(Value) %>% pull()
                # converting loan amount from CHF to EUR
                #loan_am_eur <- input$loan_amount /exc_on_loan_day()
                # calculating monthly payment, forming data table - I should check interest in the payment
                #month_pay_eur <- pmt(amt = l_am_eur(), maturity = input$number_of_periods, rate=input$interest_rate/100/12)
                
                
                
                table_plan <- data.frame("Payment_day" = input$loan_first_p_date %m+% months(c(0:(input$number_of_periods-1))))
                table_plan$Loan_amo <- input$loan_amount
                table_plan$Monthly_payment_CHF <- month_pay_chf()
                table_plan$Cum_payments <- cumsum(table_plan$Monthly_payment_CHF)
                table_plan$Remaining_payments <- month_pay_chf() * input$number_of_periods - cumsum(table_plan$Monthly_payment_CHF)
                #Calculating interest helping DF
                #Check previous code
                table_plan$interest_1 <-0
                table_plan$interest_1[1] <- table_plan$Loan_amo * input$interest_rate/100/12
                table_plan$payed_principal_1[1] <- table_plan$Monthly_payment_CHF - table_plan$interest_1[1]
                table_plan$Loan_am_down <- 0
                table_plan$Loan_am_down[1] <- table_plan$Loan_amo[1] - table_plan$payed_principal_1[1] 
                #Loop for filling values calculated on previous cell values find better way!
                for (i in 2:input$number_of_periods){
                        table_plan$interest_1[i] <- table_plan$Loan_am_down[i-1] * input$interest_rate/100/12
                        table_plan$payed_principal_1[i] <- table_plan$Monthly_payment_CHF[i] - table_plan$interest_1[i]
                        table_plan$Loan_am_down[i] <- table_plan$Loan_am_down[i-1] - table_plan$payed_principal_1[i]
                }
                
                
                table_plan$Cum_interest <- round(cumsum(table_plan$interest_1), 2)
                table_plan$Cum_principal <- round(cumsum(table_plan$payed_principal_1), 2)
                #table_plan <- table_plan %>% mutate_if(is.numeric, round, digits = 3) %>% mutate_if(is.numeric, format, nsmall = 3, decimal.mark = ",", big.mark = ".")
                table_plan
                    
        })
        

        
        
        output$amortPlan <- DT::renderDataTable(DT::datatable({
                t_p() %>% select("Date of payment" = Payment_day,
                                 "Monthly payment CHF" = Monthly_payment_CHF, 
                                 "Payed principal" = payed_principal_1,
                                 "Payed interest" = interest_1, 
                                 "Cumulative loan payed" = Loan_am_down) %>%
                        mutate_if(is.numeric, round, digits = 3) %>% mutate_if(is.numeric, format, nsmall = 3, decimal.mark = ",", big.mark = ".")
   }))
        #Creating reactive Excel file with sheets and data
        wb <- reactive({
                wb_1 <- createWorkbook()
                ## Add worksheets
                addWorksheet(wb_1, "Loan Plan")
                addWorksheet(wb_1, "Payments")
                
                #Adding data uploaded by user
                inFile <- input$placanja_rsd_file
                
                if(is.null(inFile))
                        return(NULL)
                inFile1 <- inFile$datapath
                df_plac <- as.data.frame(read_xlsx(inFile1))
                df_plac$Datum <- ymd(df_plac$Datum)            
               
                # Adding data to created sheets
                writeData(wb_1, 1, t_p(), rowNames = TRUE, startCol = "A", startRow = 1, borders="surrounding", borderColour = "black") ## black border
                writeData(wb_1, 2, df_plac, rowNames = TRUE, startCol = "A", startRow = 1, borders="surrounding", borderColour = "black") ## black border
                wb_1
        })
        
        output$summary <- renderUI({
                
                HTML(paste0("<h3>CHF LOAN SUMMARY</h3><br> </br>Loan amount CHF: ", input$loan_amount, "</br> Monthly payment CHF: ",
                            round(month_pay_chf(), 3), "</br>Date of first payment: ",
                            format(input$loan_first_p_date, format = "%d.%m.%Y."),
                            "</br>Total number of payments in months: ", input$number_of_periods,
                            "</br>Total principal payed CHF: ", round(sum(t_p()$payed_principal_1), 3),
                            "</br>Total interest payed CHF: ", round(sum(t_p()$interest_1), 3),
                            "</br>Total loan payments CHF: ", round(sum(t_p()$Monthly_payment_CHF), 3)
                            ))
                
        })
        # Rendering loan plot for t_p() reactive object
        output$loan_plot <- renderPlot({
                t_p() %>% select(Payment_day, "Interest payed" = interest_1, "Principal payed" = payed_principal_1) %>%
                        gather(key = "Payed_part", value = "Amount", -Payment_day) %>%
                        mutate(year = year(Payment_day)) %>% 
                        group_by(year, Payed_part) %>%
                        summarize(paid_amount = sum(Amount)) %>%
                        ggplot(aes(x = as.factor(year), y = paid_amount, fill = Payed_part)) +
                        geom_col() + labs(x = "Loan years", y = "Amount EUR", title = "PAID AMOUNT THROUGH LOAN PAYMENT PERIOD") + 
                        theme_minimal() + theme(axis.text.x = element_text(size = 12, angle = 25)) + scale_fill_brewer(palette="Paired")
        })
        
        #Reading uploaded excel file by user check this!!!
        output$loaded_data <- DT::renderDataTable(DT::datatable({
                inFile <- input$placanja_rsd_file
                
                if(is.null(inFile))
                        return(NULL)
                inFile1 <- inFile$datapath
                df_plac <- as.data.frame(read_xlsx(inFile1))
                df_plac$Datum <- ymd(df_plac$Datum)            
                #here we have a problem with date format???
                #df_plac$Datum <- ymd(df_plac$Datum)
                df_plac
        }))
        output$downloadData <- downloadHandler(
                filename = function() {"test.xlsx"},
                content = function(file){saveWorkbook(wb(), file, overwrite = TRUE)}
                )
        
#Defining output and reactive data for second panel
        rep_payment_eur <- reactive({
                r_p_eur <- pmt(amt = input$loan_amount_rep_chf/exc_rat(), maturity = input$number_of_periods_rep, rate=input$interest_rate_eur/100/12)
                r_p_eur
                })
        
        output$infoEUR <- renderUI({
                HTML(paste0("Conversion rate: ",exc_rat(),
                            "</br>New monthly payment EUR: ", round(rep_payment_eur(), 3)
                            ))
        })
       

        
}

# Run the application 
shinyApp(ui = ui, server = server)

