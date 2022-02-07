read_excel_title <- function(path, sheet) { 
  library(stringr)
  library(stringi)
  library(readxl)
  
  tabla <- readxl::read_xlsx(path = path,sheet = sheet, col_names = FALSE, col_types =  "text" )  
  primera_fila = tabla[1,]
  
  primera_fila[] <- lapply(primera_fila, gsub, pattern='  ', replacement=' ')
  primera_fila[] <- lapply(primera_fila, gsub, pattern=' ', replacement='_')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='[,]', replacement='_')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='[.]', replacement='_')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='[ÃÂ¿]', replacement='')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='\r', replacement='_')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='\n', replacement='_')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='[-]', replacement='_')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='[[]', replacement='_')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='[]]', replacement='_')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='[$]', replacement='_')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='__', replacement='_')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='200', replacement='Y200')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='201', replacement='Y201')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='202', replacement='Y202')
  #primera_fila[] <- lapply(primera_fila, gsub, pattern='2019', replacement='ANO_2019')
  #indeseables = c('1_','2_','3_','4_','5_','6_','7_','8_','9_', '$' ) 
  indeseables = c( '$' , '[?]', '[/]', '[)]', '[()]') 
  for(i in indeseables) {
    primera_fila[] <- lapply(primera_fila, gsub, pattern= i, replacement='')
  }
  
  primera_fila = toupper(stri_trans_general(primera_fila,"Latin-ASCII"))
  colnames(tabla) = primera_fila
  tabla = tabla[-1,]
}


read_csv_title <- function(path) { 
  library(stringr)
  library(stringi)
  library(readr)
  
  tabla <- readr::read_csv(file = path  , col_names = FALSE )  
  primera_fila = tabla[1,]
  
  primera_fila[] <- lapply(primera_fila, gsub, pattern='  ', replacement=' ')
  primera_fila[] <- lapply(primera_fila, gsub, pattern=' ', replacement='_')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='[,]', replacement='_')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='[.]', replacement='_')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='[ÃÂ¿]', replacement='')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='\r', replacement='_')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='\n', replacement='_')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='[-]', replacement='_')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='[[]', replacement='_')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='[]]', replacement='_')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='[$]', replacement='_')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='__', replacement='_')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='200', replacement='Y200')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='201', replacement='Y201')
  primera_fila[] <- lapply(primera_fila, gsub, pattern='202', replacement='Y202')
  #primera_fila[] <- lapply(primera_fila, gsub, pattern='2019', replacement='ANO_2019')
  #indeseables = c('1_','2_','3_','4_','5_','6_','7_','8_','9_', '$' ) 
  indeseables = c( '$' , '[?]', '[/]', '[)]', '[()]') 
  for(i in indeseables) {
    primera_fila[] <- lapply(primera_fila, gsub, pattern= i, replacement='')
  }
  
  primera_fila = toupper(stri_trans_general(primera_fila,"Latin-ASCII"))
  colnames(tabla) = primera_fila
  tabla = tabla[-1,]
}

