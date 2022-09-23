netejar_espais<-function(dt=dades) {
  # dt=dt_total
  dt<-dt %>% mutate_if(is.character,stringr::str_trim)
}

