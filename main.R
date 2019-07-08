library(tidyverse)
library(tabulizer)
library(textclean)
library(stringr)

f <- "data/documentView_retrieveStatementPdf07.pdf"
f1 <- "data/documentView_retrieveStatementPdf07 (2).pdf"

hawaii_telecom <- function(f){
  
  local_df <- pdf_text(f) %>%    # читаємо pdf
    .[[1]] %>% str_split(., "\n", simplify = TRUE) %>%  # розбиваємо строки
    data.frame() %>%   
    t() %>% 
    as.data.frame() %>% 
    magrittr::set_colnames("data") %>% 
    filter(grepl("Payments|Service Period|Account Number|TOTAL AMOUNT DUE|TOTAL NEW CHARGES|Payment Due|Invoice Number",data))  # обираємо тік потрібну інфу
 
   global_acc <- local_df %>%    # шукаємо глобальний аккаунт
     filter(grepl("Account",data)) %>%  # по ключовому слову
     mutate_all(as.character) %>% 
     mutate(data = replace_white(data)) %>% # удаляэмо зайві пробели
     .[1,1] %>%  
     str_extract(., '(?<=Account Number:\\s)\\w+') # шукаємо слово наступне за Account Number:
   
   inv_number <-  local_df %>% 
     filter(grepl("Invoice Number",data)) %>% 
     mutate_all(as.character) %>%
     mutate(data = replace_white(data)) %>%
     .[1,1] %>% 
     str_extract(., '(?<=Invoice Number:\\s)\\w+')
   
   inv_date <-  local_df %>% 
     filter(grepl("Invoice Date",data)) %>% 
     mutate_all(as.character) %>%
     mutate(data = replace_white(data)) %>%
     .[1,1] %>% 
     str_split(.," ") %>% unlist() %>%  .[length(.)] # шукаємо останній елемент масиву (там дата по файлу). краще переписати на регулярний вираз, но хз як
    
  total <-  local_df %>% 
     filter(grepl("TOTAL AMOUNT DUE",data)) %>% 
     mutate_all(as.character) %>%
     mutate(data = replace_white(data)) %>%
     .[1,1] %>% parse_number()                     # парсимо число
  
  new <- local_df %>% 
    filter(grepl("TOTAL NEW CHARGES",data)) %>% 
    mutate_all(as.character) %>%
    mutate(data = replace_white(data)) %>%
    .[1,1] %>% parse_number()
  
  period <- local_df %>% 
    filter(grepl("Service Period",data)) %>% 
    mutate_all(as.character) %>%
    mutate(data = replace_white(data)) %>%
    .[1,1] %>% 
    str_split(.," ") %>% unlist() 
  
  pay_due <- local_df %>%
    filter(grepl("Payment Due",data)) %>%
    mutate_all(as.character) %>%
    mutate(data = replace_white(data)) %>%
    .[1,1] %>% 
    gsub(" Payment Due: ",'',.) %>% as.Date(., "%B %d, %Y")   # переводимо текст дати в дату
    
  
  start <- period %>% .[length(.)-2] %>% lubridate::mdy(.)   # переводимо дату в класс Date
  end <- period %>% .[length(.)] %>% lubridate::mdy(.)
  
  res <-data.frame(                                       # фінальний дата фрейм
    "Contract_Number" = "HawaiianTelcom_PublicStorage",   
    "Provider" = "Hawaiiantel_us_fix_man",
    "Global_Account" = global_acc,
    "Invoice_Number" = inv_number,
    "Invoice_Date" = inv_date,
    "Total" = new,
    "Currency" = "USD",
    "Total_Amount_Due" = total,
    "Due_Date" = pay_due,
    "Date_From" = start,
    "Date_To" =  end)
  return(res)
}

path<-c(paste0(getwd(),"/data/"))   # вказуємо шлях до папки, де наші файли

data <- data_frame(filename =  list.files(path)) %>%      #циклом проганяємо всі файли 
  # slice(8:9) %>% 
  mutate(file_contents = map(paste0("data/",filename),          
                             ~hawaii_telecom(.))) %>% 
  unnest() %>% 
  select(-filename)

 
