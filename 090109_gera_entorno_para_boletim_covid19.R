get_mapa <- function() {
  require(geojsonR)
  require(geojsonsf)
  require(sf)
  
  #usando apis do IBGE para coleta dos dados
  #https://servicodados.ibge.gov.br/api/docs
  aux <- httr::GET('https://servicodados.ibge.gov.br/api/v3/malhas/paises/BR?intrarregiao=municipio&formato=application/vnd.geo+json&qualidade=intermediaria')
  mapa <- geojsonsf::geojson_sf(httr::content(teste,as = "text"))
  dados <- jsonlite::fromJSON('https://servicodados.ibge.gov.br/api/v1/localidades/municipios')
  
  #juntando informacoes
  mapa$codarea <- as.numeric(mapa$codarea)
  names(mapa) <- c("id","geometry")
  mapa <- merge(mapa, dados)
  
  return(mapa)
}

get_entorno <- function(mapa, entorno = c("MANAUS",
                                          "CAREIRO",
                                          "CAREIRO DA VÁRZEA",
                                          "AUTAZES",
                                          "MANAQUIRI",
                                          "BERURI",
                                          "BORBA",
                                          "TAPAUÁ",
                                          "MANICORÉ",
                                          "HUMAITÁ",
                                          "CANUTAMA",
                                          "LÁBREA", 
                                          "PORTO VELHO"), estados = c("AM","RO")){

  mapa$nome <- toupper(mapa$nome)
  
  result <- mapa[mapa$nome %in% entorno & mapa$microrregiao$mesorregiao$UF$sigla %in% estados,]
  
  return(result)
}
  
