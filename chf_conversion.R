library("tidyverse")
library("tvm")
library("Quandl")
library("lubridate")
library("xts")
library("readxl")

#Reading file of middle RSD/EUR exchange rate and converting to Data Frame
exc_eur_rsd <- as.data.frame(read_csv2("Data_read/Sred_kurs_NBS.csv"))
exc_eur_rsd <- exc_eur_rsd %>% select(Datum_Primene, Srednji_kurs)
exc_eur_rsd$Datum_Primene <- as.Date(exc_eur_rsd$Datum_Primene, format = "%d.%m.%Y.")

#Variables for input
exc_rate <- Quandl("ECB/EURCHF")
loan_am_chf <- 10000
int_r_fix_chf <- 0.07
per_of_pay_month <- 240
loan_date <- as.Date("2007-12-28")

# Reading input excel file, filled by the user
user_input_file <- as.data.frame(read_excel("www/template_excel_input.xlsx"))
user_input_file$Datum <- as.Date(user_input_file$Datum)

#Joining table
Calc_rsd_eur <- user_input_file %>% left_join(exc_eur_rsd, by = c("Datum" = "Datum_Primene"))

#Converting RSD amount to EUR amount
Calc_rsd_eur$Isplacen_iznos_EUR <- Calc_rsd_eur$Isplacen_iznos / Calc_rsd_eur$Srednji_kurs




exc_on_loan_day <- exc_rate %>% filter(Date == loan_date) %>% select(Value) %>% pull()


month_pay_chf <- loan(amt = loan_am_chf, maturity = per_of_pay_month, rate=int_r_fix_chf/12, type = "bullet")
#Euro conversion and calculation
loan_am_eur <- loan_am_chf/exc_on_loan_day
month_pay_eur <- pmt(amt = loan_am_eur, maturity = per_of_pay_month, rate=int_r_fix_chf/12)
inter_amt_eur <- month_pay_eur*per_of_pay_month - loan_am_eur


#Information - amount of loan on day payed to client in CHF
print(paste0("Iznos CHF kredita konvertovan u EUR na dan isplate kredita ", format(loan_date, format = "%d.%m.%Y."), " iznosi ", format(loan_am_eur, digits = 2, nsmall = 2, decimal.mark = ",", big.mark = "."), " EUR "))
print(paste0("Prva rata konvertovana u evre iznosi ", format(month_pay_eur, digits = 2, nsmall = 2, decimal.mark = ",", big.mark = "."), " EUR "))
print(paste0("Ukupno placanje kamate do kraja otplate u EUR ", format(inter_amt_eur, digits = 2, nsmall = 2, decimal.mark = ",", big.mark = "."), " EUR "))
#month_pay_chf * per_of_pay_month - loan_am_chf
