#' @title             Read_conductor
#' @description       Llegeix el fitxer conductor de diferents formats txt o rds o xls xlsx o data_frame tibble
#' @param fitxer      Fitxer que el convertirem a tibble.
#' @param ...         Altres funcions
#' @return            Una taula tibble
#' @export            read_conductor
#' @importFrom        dplyr "%>%"
#' @examples
#'camp=c("idp",
#'       "dtindex",
#'       "sexe",
#'       "dnaix",
#'       "situacio",
#'       "entrada",
#'       "sortida", 
#'       "INCLUSIO.DM2",
#'       "DG.HTA",
#'       "DG.IC",
#'       "cHDL.valor",
#'       "cLDL.valor",
#'       "cT.valor",
#'       "GLICADA.valor",
#'       "IMC.valor")
#'descripcio=c("Identificacio Pacient",
#'             "data Index",
#'             "Sexe",
#'             "data Naixament",
#'             "Situacio",
#'             "Entrada",
#'             "Sortida",
#'             "Inclusio Diabetes Tipus 2",
#'             "Hipertensió arterial",
#'             "Insuficiencia Cardiaca",
#'             "Colesterol HDL(mg/dL)",
#'             "Colesterol LDL(mg/dL)",
#'             "Colesterol Total(mg/dL)",
#'             "HbA1c",
#'             "IMC" )
#'descripcio2=c("Identificacion Paciente",
#'              "data Indice",
#'              "Sexo",
#'              "data Naicimiento",
#'              "Situacion",
#'              "Entrada",
#'              "Salida",
#'              "Inclusion Diabetes Tipus 2",
#'              "Hipertensión arterial",
#'              "Insuficiencia Cardiaca",
#'              "Colesterol HDL(mg/dL)",
#'              "Colesterol LDL(mg/dL)",
#'              "Colesterol Total(mg/dL)",
#'              "HbA1c",
#'              "IMC" )
#'factor=c("","","","","","","",1,1,1,"","","","","")
#'dates=c("",1,"",1,"",1,1,"","","","","","","","")
#'recode=c("","","","","","","","","","","","","","7.0","")
#'taula1=c(1,1,"","","","","","","","","","","","","")
#'var_nou=c("idp_nou","index_nou","","","","","","","","","","","","","")
#'
#'conductor1<-data.frame(camp,descripcio,descripcio2,factor,dates,recode,taula1,var_nou)
#'k1<-read_conductor(conductor1)
#'k1
read_conductor<-function(fitxer,...) {
  # fitxer<-here::here(fitxer_cataleg)
  # Si el fitxer es un data_frame saltar
  if (any(class(fitxer) %in% c("tbl_df","tbl","data.frame")))

    dt <- tibble::as_tibble(fitxer)

  else {

    if (stringr::str_detect(fitxer,"\\.txt$")){

      dt<-data.table::fread(fitxer) %>% tibble::as_tibble()

    } else if (stringr::str_detect(fitxer,"\\.rds$")) {

      dt<-readRDS(fitxer,...) %>%tibble::as_tibble()

    } else if (stringr::str_detect(fitxer,"\\.xls$")) {

      dt<-readxl::read_excel(fitxer,...) %>% tibble::as_tibble()

    } else if (stringr::str_detect(fitxer,"\\.xlsx$")) {

      dt<-readxl::read_excel(fitxer,...) %>% tibble::as_tibble()

    } else if (stringr::str_detect(fitxer,"\\.xlS$")) {

      dt<-readxl::read_excel(fitxer,...) %>% tibble::as_tibble()

    } else if (stringr::str_detect(fitxer,"\\.sav$")) {

      dt<-foreign::read.spss(fitxer,use.value.labels = T,to.data.frame = T,...) %>% tibble::as_tibble()
    }
    else {stop("format de dades no reconegut ")}
  }

}

