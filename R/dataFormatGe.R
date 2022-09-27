#' @title  DATA RANDOM ENTRE DUES DATES (dataini i datafi)
#' @description DATA RANDOM ENTRE DUES DATES 
#' @param  dataini           xxx
#' @param  datafi            xxx
#' @return variables
#' @export data.random
#' @importFrom dplyr "%>%"
#' @examples
#' domini="farmacs_prescrits"
#' cod=c("A10BB01","A10BD01","A10BD04","A10BA02","J01DD07")
#' agr_Farmac=c("Sulfonilureas","Biguanidas","Tiazolidinadiones","Biguanidas","Antibioticos")
#' dt_cataleg<-data.frame(domini=domini,cod=cod,agr_Farmac=agr_Farmac)
data.random <- function(dataini=20120101, datafi=20121231) {
  
  # dataini=20120101
  # datafi=20161231
  
  dataini <- as.POSIXct(lubridate::ymd(dataini))
  datafi <- as.POSIXct(lubridate::ymd(datafi))
  temps <- as.numeric(difftime(datafi,dataini,units ="secs"))
  
  # Genera Data sumant temps random a dataini
  rt <- dataini + stats::runif(1, 0, temps)
}


#' @title   RETORNA UNA DATA A STRING
#' @description RETORNA UNA DATA A STRING 
#' @param  data           xxx
#' @return RETORNA UNA DATA A STRING 
#' @export data.to.string
#' @importFrom dplyr "%>%"
#' @examples
#' A<-"27-09-2022"
#' B<-data.to.string(A)
#' B
data.to.string<-function(data) {
  
  data.string=paste0(lubridate::year(data),
                     stringr::str_pad(lubridate::month(data),2,"left","0"),
                     stringr::str_pad(lubridate::day(data),2,"left","0"))
  
}



#' @title   Data R Lubridate a partir de data UTC
#' @description Data R Lubridate a partir de data UTC 
#' @param  x             xxx
#' @param  dt            xxx
#' @return variables
#' @export dataUTC_to_Rdata
#' @importFrom dplyr "%>%"
#' @examples
#'
#' domini="farmacs_prescrits"
#' cod=c("A10BB01","A10BD01","A10BD04","A10BA02","J01DD07")
#' agr_Farmac=c("Sulfonilureas","Biguanidas","Tiazolidinadiones","Biguanidas","Antibioticos")
#' dt_cataleg<-data.frame(domini=domini,cod=cod,agr_Farmac=agr_Farmac)
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




#' @title   converteix data caracter ("37712") a date ymd
#' @description Funcio que converteix data caracter ("37712") a date ymd () "2003-04-01"
#' @param  x            xxx
#' @return variables
#' @export data_convert_text
#' @importFrom dplyr "%>%"
#' @examples
#' domini="farmacs_prescrits"
#' cod=c("A10BB01","A10BD01","A10BD04","A10BA02","J01DD07")
#' agr_Farmac=c("Sulfonilureas","Biguanidas","Tiazolidinadiones","Biguanidas","Antibioticos")
#' dt_cataleg<-data.frame(domini=domini,cod=cod,agr_Farmac=agr_Farmac)
data_convert_text<-function(x){
  x<-as.Date(as.numeric(x), origin = "1899-12-30") %>% 
    lubridate::ymd()}

 

#' @title   Funcio que converteix de numeric (15784) a Date
#' @description Funcio que converteix de numeric (15784) a Date "2013-03-20" 
#' @param  x            xxx
#' @return data
#' @export data_convert_numeric
#' @importFrom dplyr "%>%"
#' @examples
#' domini="farmacs_prescrits"
#' cod=c("A10BB01","A10BD01","A10BD04","A10BA02","J01DD07")
#' agr_Farmac=c("Sulfonilureas","Biguanidas","Tiazolidinadiones","Biguanidas","Antibioticos")
#' dt_cataleg<-data.frame(domini=domini,cod=cod,agr_Farmac=agr_Farmac)
data_convert_numeric<-function(x){ x<-as.Date(x, origin = "1970-01-01")}



#' @title   Funcio que converteix UTC data a date ymd
#' @description Funcio que converteix UTC data a date ymd 
#' @param  x            xxx
#' @return data
#' @export data_convert_UTC
#' @importFrom dplyr "%>%"
#' @examples
#' domini="farmacs_prescrits"
#' cod=c("A10BB01","A10BD01","A10BD04","A10BA02","J01DD07")
#' agr_Farmac=c("Sulfonilureas","Biguanidas","Tiazolidinadiones","Biguanidas","Antibioticos")
#' dt_cataleg<-data.frame(domini=domini,cod=cod,agr_Farmac=agr_Farmac)
data_convert_UTC<-function(x){
  x<-format(as.POSIXct(x, origin='1970-01-01'), format='%Y/%m/%d')
  x<-lubridate::ymd(x)}


#' @title   CONVERTEIX FORMAT TEXT A DATA 
#' @description Format YYYYMMDD (Format text -> data)   
#' @param  d                         xxx
#' @param  taulavariables            xxx
#' @param  campdata                  xxx
#' @return data
#' @export convertir_dates
#' @importFrom dplyr "%>%"
#' @examples
#' domini="farmacs_prescrits"
#' cod=c("A10BB01","A10BD01","A10BD04","A10BA02","J01DD07")
#' agr_Farmac=c("Sulfonilureas","Biguanidas","Tiazolidinadiones","Biguanidas","Antibioticos")
#' dt_cataleg<-data.frame(domini=domini,cod=cod,agr_Farmac=agr_Farmac)
convertir_dates<-function(d="dadestotal",taulavariables="variables_R.xls",campdata="dates")
  
{
  ####  Llegir etiquetes i variables a analitzar  ##
  variables <- readxl::read_excel(taulavariables) %>% tidyr::as_tibble() 
  
  # variables[is.na(variables)]<- 0
  campdata_sym<-dplyr::sym(campdata)
  variables<-variables %>% dplyr::filter(!is.na(!!campdata_sym))
  
  # etiquetar variables         
  seleccio<-variables
  camp<- as.vector(seleccio$camp) #
  
  for (i in 1:length(camp)){if (seleccio$dates[i]==1) { 
    
    pepito<-paste0("as.Date(d[[camp[",i,"]]], '%Y%d%m')")
    
    d[[camp[i]]]<-eval(parse(text=pepito))
    
  } }
  
  d
  
}



# 13481683200 --> 2010-01-01
#' @title   Passa data de SPSS a Rdata
#' @description Passa data de SPSS a Rdata 
#' @param  x            xxx
#' @return data
#' @export dataUTC_to_Rdata
#' @importFrom dplyr "%>%"
#' @examples
#' domini="farmacs_prescrits"
#' cod=c("A10BB01","A10BD01","A10BD04","A10BA02","J01DD07")
#' agr_Farmac=c("Sulfonilureas","Biguanidas","Tiazolidinadiones","Biguanidas","Antibioticos")
#' dt_cataleg<-data.frame(domini=domini,cod=cod,agr_Farmac=agr_Farmac)
dataSPSS_to_Rdata <- function(x) {
  y<-as.Date(x/86400, origin = "1582-10-14") %>% 
    lubridate::ymd() }

