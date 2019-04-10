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

#Loading data from files and Quandle

#Reading file of middle RSD/EUR exchange rate and converting to Data Frame
exc_eur_rsd <- as.data.frame(read_csv2("Data_read/Sred_kurs_NBS.csv"))
exc_eur_rsd <- exc_eur_rsd %>% select(Datum_Primene, Srednji_kurs)
exc_eur_rsd$Datum_Primene <- as.Date(exc_eur_rsd$Datum_Primene, format = "%d.%m.%Y.")

#Loading data from Quandle
exc_rate <- Quandl("ECB/EURCHF")

# Define UI for application that calculates loan amortization
ui <- fluidPage(
        
   # Application title
   titlePanel("CHF to EUR loan converter"),
   
   # Sidebar with a slider input for loan data 
   sidebarLayout(
      sidebarPanel(
         numericInput("interest_rate",
                     "Interest rate on CHF loan %:",
                     min = 3.001,
                     max = 8.001,
                     value = 5.001,
                     step = 0.001),
      
         numericInput("number_of_periods",
                   "Number of periods in months:",
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
         fileInput("placanja_rsd_file",
                   "File with payments made, RSD:",
                   accept = c(".xlsx")),
         tags$a(href='template_excel_input.xlsx', target='blank', 'Download file upload template', download = 'Template_chf_loan.xlsx',
                downloadButton("downloadData", "Download"))
   ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tabsetPanel( type = "tabs",
                      tabPanel("Summary", htmlOutput("summary")),
                      tabPanel("Am_plan", DT::dataTableOutput("amortPlan")),
                      tabPanel("Plot", plotOutput("loan_plot")),
                      tabPanel("Payed", DT::dataTableOutput("loaded_data"))
         
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        #creating reactive EUR/CHF exchange rate on loan day
        exc_on_loan_day <- reactive({
         eur_chf_exc <- exc_rate %>% filter(Date == input$loan_date) %>% select(Value) %>% pull()
         eur_chf_exc
        })
        l_am_eur <- reactive({
                loan_am_eur <- round(input$loan_amount /exc_on_loan_day(), 2)
                loan_am_eur
        })
        #creating reactive loan_am_eur
   
  
        
        # creating reactive table data
        t_p <- reactive({
                # generate exchange rate EUR/CHF on loan day
                
                #exc_on_loan_day <- exc_rate %>% filter(Date == input$loan_date) %>% select(Value) %>% pull()
                # converting loan amount from CHF to EUR
                #loan_am_eur <- input$loan_amount /exc_on_loan_day()
                # calculating monthly payment, forming data table - I should check interest in the payment
                month_pay_eur <- pmt(amt = l_am_eur(), maturity = input$number_of_periods, rate=input$interest_rate/100/12)
                
                table_plan <- data.frame("Payment_day" = input$loan_first_p_date %m+% months(c(0:(input$number_of_periods-1))))
                table_plan$Loan_amo <- l_am_eur()
                table_plan$Monthly_payment_EUR <- month_pay_eur
                table_plan$Cum_payments <- cumsum(table_plan$Monthly_payment_EUR)
                table_plan$Remaining_payments <- month_pay_eur * input$number_of_periods - cumsum(table_plan$Monthly_payment_EUR)
                #Calculating interest helping DF
                #Check previous code
                table_plan$interest_1 <-0
                table_plan$interest_1[1] <- table_plan$Loan_amo * input$interest_rate/100/12
                table_plan$payed_principal_1[1] <- table_plan$Monthly_payment_EUR - table_plan$interest_1[1]
                table_plan$Loan_am_down <- 0
                table_plan$Loan_am_down[1] <- table_plan$Loan_amo[1] - table_plan$payed_principal_1[1] 
                #Loop for filling values calculated on previous cell values find better way!
                for (i in 2:input$number_of_periods){
                        table_plan$interest_1[i] <- table_plan$Loan_am_down[i-1] * input$interest_rate/100/12
                        table_plan$payed_principal_1[i] <- table_plan$Monthly_payment_EUR[i] - table_plan$interest_1[i]
                        table_plan$Loan_am_down[i] <- table_plan$Loan_am_down[i-1] - table_plan$payed_principal_1[i]
                }
                
                
                table_plan$Cum_interest <- round(cumsum(table_plan$interest_1), 2)
                table_plan$Cum_principal <- round(cumsum(table_plan$payed_principal_1), 2)
                #table_plan <- table_plan %>% mutate_if(is.numeric, round, digits = 3) %>% mutate_if(is.numeric, format, nsmall = 3, decimal.mark = ",", big.mark = ".")
                table_plan
                    
        })
        

        
        
        output$amortPlan <- DT::renderDataTable(DT::datatable({
                t_p() %>% select(Payment_day, Monthly_payment_EUR, payed_principal_1, interest_1, Loan_am_down) %>%
                        mutate_if(is.numeric, round, digits = 3, nsmall = 3, decimal.mark = ",", big.mark = ".")
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
                
                HTML(paste0("<h1>LOAN SUMMARY</h1> </br>Loan amount CHF: ", input$loan_amount, "</br> Loan amount converted to EUR: ",
                            l_am_eur(), "</br> Exchange rate EUR/CHF ", exc_on_loan_day(),
                            " at loan first payment ", format(input$loan_first_p_date, format = "%d.%m.%Y.")))
                
        })
        output$loan_plot <- renderPlot({
                t_p() %>% mutate(year = year(Payment_day)) %>% 
                        group_by(year) %>%
                        summarize(paid_principal = sum(as.numeric(payed_principal_1))) %>%
                        ggplot(aes(x = year, y = paid_principal, fill = year)) +
                        geom_col() + theme_minimal()
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
       

        
}

# Run the application 
shinyApp(ui = ui, server = server)

