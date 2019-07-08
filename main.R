library(tidyverse)
library(tabulizer)
library(textclean)
library(stringr)

f <- "data/documentView_retrieveStatementPdf07.pdf"
f1 <- "data/documentView_retrieveStatementPdf07 (2).pdf"

hawaii_telecom <- function(f){
  
  local_df <- pdf_text(f) %>% 
    .[[1]] %>% str_split(., "\n", simplify = TRUE) %>% 
    data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    magrittr::set_colnames("data") %>% 
    filter(grepl("Payments|Service Period|Account Number|TOTAL AMOUNT DUE|TOTAL NEW CHARGES|Payment Due|Invoice Number",data))
 
   global_acc <- local_df %>% 
     filter(grepl("Account",data)) %>% 
     mutate_all(as.character) %>% 
     mutate(data = replace_white(data)) %>%
     .[1,1] %>%  
     str_extract(., '(?<=Account Number:\\s)\\w+')
   
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
     str_split(.," ") %>% unlist() %>%  .[length(.)]
    
  total <-  local_df %>% 
     filter(grepl("TOTAL AMOUNT DUE",data)) %>% 
     mutate_all(as.character) %>%
     mutate(data = replace_white(data)) %>%
     .[1,1] %>% parse_number()
  
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
    gsub(" Payment Due: ",'',.) %>% as.Date(., "%B %d, %Y")
    
  
  start <- period %>% .[length(.)-2] %>% lubridate::mdy(.)
  end <- period %>% .[length(.)] %>% lubridate::mdy(.)
  
  res <-data.frame(
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

path<-c(paste0(getwd(),"/data/"))

data <- data_frame(filename =  list.files(path)) %>%
  # slice(8:9) %>% ## find all files in folder
  mutate(file_contents = map(paste0("data/",filename),          
                             ~hawaii_telecom(.))) %>% ## read each file without colnames
  unnest()

 
