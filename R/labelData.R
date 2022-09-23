#' @title  Label variables according external file
#' @description Retorna 
#' @param data data.frame
#' @param taulavariables excel file with description field
#' @param camp_descripcio Field with label in excel file
#' @return Data.frame or tibble with labeled variables
#' @export
#' @importFrom dplyr "%>%"
#' @examples
#'
#' Hmisc::label(iris)
#' 
#' labels_data<-data.frame(camp=names(iris),
#' description=c("Longitud de sepal",
#'            "Amplada de sepal",
#'             "longitul del petal",
#'             "amplada del sepal",
#'             "Especies varies"))
#' labels_data
#' iris_labeled<-labelData(iris,labels_data,"description")
#' Hmisc::label(iris)
#' Hmisc::label(iris_labeled)

labelData<-function(data,taulavariables,camp_descripcio) {

  # data=iris#
  # taulavariables = label_data
  # camp_descripcio="description"
  
  # #  Llegir etiquetes i variables a analitzar ####
  # variables <- readxl::read_excel(taulavariables) %>% tidyr::as_tibble()
  variables<-read_conductor(taulavariables) %>% 
    dplyr::mutate_all(as.character)
  
  variables<-variables %>% dplyr::filter(!is.na(camp))

  # selecciono els camps necessaris (camp i descripcio) i amb etiqueta
  camp_descripcio<-rlang::sym(camp_descripcio)

  variables<-variables %>% dplyr::select(camp,descripcio=!!camp_descripcio)

  # Els que no tenen etiqueta assignar el mateix nom del camp
  variables<-variables %>% dplyr::mutate(descripcio=ifelse(descripcio=="0",camp,descripcio))

  # Etiquetar variables
  seleccio<-variables
  camp<- as.vector(seleccio$camp) #
  descripcio<- as.vector(seleccio$descripcio) #
  ### etiquetar variables seleccionades     #
  for (i in 1:length(descripcio)){if (any(colnames(data) == camp[i])) {Hmisc::label(data[[camp[i]]]) <- descripcio[i]}}
  data
  
  
}

