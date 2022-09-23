missings_to_level<-function(dades,variable="popes") {
  
  # dades=temp
  # variable="val_CKDEPI.cat5"
  
  # Subset columnes de d
  d_temp<-dades %>% select_("temp"=variable)
  
  # names(dt)[names(dt)==variable]<-"variable_temporal"
  # dt<-dt %>% rename_("variable_temporal_provisional"=variable)
  
  levels_nous <- levels(d_temp$temp)
  levels_nous[length(levels_nous) + 1] <- "None"
  
  d_temp$temp<-factor(d_temp$temp,levels = levels_nous)
  d_temp$temp[is.na(d_temp$temp)]<-"None"
  
  #
  dades <- dades %>% select_(paste0("-",variable))
  
  # Canviar el nom al origen 
  names(d_temp)[names(d_temp) == "temp"] <- variable
  
  dades <-cbind(dades,d_temp)
  
}

