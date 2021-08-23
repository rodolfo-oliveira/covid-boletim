

baixa_dados <- function(directory = "data", estados, update=F){
  require(dplyr)

  if(file.exists("data/dados.gz")==F | update==T){
    download.file("https://data.brasil.io/dataset/covid19/caso_full.csv.gz",
                  destfile = "data/dados.gz")
  }
  
  #extraindo dados
  fn <- gzfile("data/dados.gz",open = "r",)
  caso_full <- read.csv(fn)

  caso_full %>% 
    filter(substr(city_ibge_code, start = 1, stop = 2) %in% estados)  %>%
    mutate(CD_GEOCODM = city_ibge_code,
           new_confirmed = new_confirmed,
           confirmed = last_available_confirmed,
           confirmed_per_100k_inhabitants = last_available_confirmed_per_100k_inhabitants,
           new_deaths = new_deaths,
           deaths = last_available_deaths,
           death_rate = last_available_death_rate,
           date = last_available_date) %>%
    select(CD_GEOCODM, city, new_confirmed,confirmed, confirmed_per_100k_inhabitants, new_deaths,
           deaths, death_rate, estimated_population_2019, date) -> df_covid
  
  return(df_covid)
}
  

get_mapa <- function() {
  require(geojsonR)
  require(geojsonsf)
  require(sf)
  require(httr)
  
  #usando apis do IBGE para coleta dos dados
  #https://servicodados.ibge.gov.br/api/docs
  aux <- httr::GET('https://servicodados.ibge.gov.br/api/v3/malhas/paises/BR?intrarregiao=municipio&formato=application/vnd.geo+json&qualidade=intermediaria')
  mapa <- geojsonsf::geojson_sf(httr::content(aux,as = "text"))
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