#' @title  DATA RANDOM ENTRE DUES DATES (dataini i datafi)
#' @description selectorvariables 
#' @param  dataini           xxx
#' @param  datafi            xxx
#' @return variables
#' @export data.random
#' @importFrom dplyr "%>%"
data.random <- function(dataini=20120101, datafi=20121231) {
  
  # dataini=20120101
  # datafi=20161231
  
  dataini <- as.POSIXct(lubridate::ymd(dataini))
  datafi <- as.POSIXct(lubridate::ymd(datafi))
  temps <- as.numeric(difftime(datafi,dataini,unit="sec"))
  
  # Genera Data sumant temps random a dataini
  rt <- dataini + runif(1, 0, temps)
}

#  RETORNA UNA DATA A STRING  ------------------

data.to.string<-function(data) {
  
  data.string=paste0(year(data),
                     str_pad(lubridate::month(data),2,"left","0"),
                     str_pad(lubridate::day(data),2,"left","0"))
  
}



#' @title   Data R Lubridate a partir de data UTC
#' @description selectorvariables 
#' @param  x            xxx
#' @param  dt            xxx
#' @return variables
#' @export dataUTC_to_Rdata
#' @importFrom dplyr "%>%"
dataUTC_to_Rdata<-function(x,dt) {
  
  # dt<-dades
  # x=c("data_inici_HD","ANT1_ARTER_PERI","ANT2_ARTE_PERI","ANT1_CI")
  
  # Seleccionar nom del camp si es tipo caracter 
  vector_caracter<-dt %>% dplyr::select_if(~!any(class(.)!="character",na.rm=F)) %>% names()
  
  # Vectors de variables UTC (data POSIXct)
  x_UTC<-x [!x %in% vector_caracter]
  
  # Vector de variables caracter ("37712")
  x_text<-x [x %in% vector_caracter]
  
  
  # Funcio que converteix UTC data a date ymd
  data_convert_UTC<-function(x){
    x<-format(as.POSIXct(x, origin='1970-01-01'), format='%Y/%m/%d')
    x<-lubridate::ymd(x)}
  
  # Funcio que converteix data caracter ("37712) a date ymd () "2003-04-01"
  data_convert_text<-function(x){
    x<-as.Date(as.numeric(x), origin = "1899-12-30") %>% 
      lubridate::ymd()}
  
  # Aplicar conversions als dos tipos de dates
  dt<-dt %>% purrr::modify_at(x_UTC,~data_convert_UTC(.x))   # UTC ->date
  
  dt<-dt %>% purrr::modify_at(x_text,~data_convert_text(.x))   # text->date
  
  dt
  
}



###acabar-ho!!!


# Funcio que converteix data caracter ("37712") a date ymd () "2003-04-01"
data_convert_text<-function(x){
  x<-as.Date(as.numeric(x), origin = "1899-12-30") %>% 
    lubridate::ymd()}

# Funcio que converteix de numeric (15784) a Date "2013-03-20"
data_convert_numeric<-function(x){ x<-as.Date(x, origin = "1970-01-01")}

# Funcio que converteix UTC data a date ymd
data_convert_UTC<-function(x){
  x<-format(as.POSIXct(x, origin='1970-01-01'), format='%Y/%m/%d')
  x<-lubridate::ymd(x)}


#  CONVERTEIX FORMAT TEXT A DATA                     -------------
#
#          Format YYYYMMDD (Format text -> data)          
# Input : dades, conductorvariables, campdata (com a indicadora (0/1))

convertir_dates<-function(d=dadestotal,taulavariables="variables_R.xls",campdata="dates")
  
{
  ####  Llegir etiquetes i variables a analitzar  ##
  variables <- readxl::read_excel(taulavariables) %>% tidyr::as_tibble() 
  # variables[is.na(variables)]<- 0
  campdata_sym<-sym(campdata)
  variables<-variables %>% dplyr::filter(!is.na(!!campdata_sym))
  
  
  #
  #
  # etiquetar variables         
  seleccio<-variables
  camp<- as.vector(seleccio$camp) #
  
  for (i in 1:length(camp)){if (seleccio$dates[i]==1) { 
    
    pepito<-paste0("as.Date(d[[camp[",i,"]]], '%Y%d%m')")
    
    d[[camp[i]]]<-eval(parse(text=pepito))
    
  } }
  
  d
  
}


#  Passa data de SPSS a Rdata  ----------------------------

# 13481683200 --> 2010-01-01

dataSPSS_to_Rdata <- function(x) {
  y<-as.Date(x/86400, origin = "1582-10-14") %>% 
    lubridate::ymd() }

