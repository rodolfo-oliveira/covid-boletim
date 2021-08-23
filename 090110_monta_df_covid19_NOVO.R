

baixa_dados <- function(directory = "data", estados){
  require(dplyr)

  download.file("https://data.brasil.io/dataset/covid19/caso_full.csv.gz",
                destfile = "data/dados.gz")
  
  #extraindo dados
  fn <- gzfile("data/dados.gz",open = "r",)
  caso_full <- read.csv(fn)

  caso_full %>% 
    filter(substr(city_ibge_code, start = 1, stop = 2) %in% estados) %>%
    mutate(CD_GEOCODM = city_ibge_code,
           confirmed = last_available_confirmed,
           confirmed_per_100k_inhabitants = last_available_confirmed_per_100k_inhabitants,
           deaths = last_available_deaths,
           death_rate = last_available_death_rate,
           date = last_available_date) %>%
    select(CD_GEOCODM, city, confirmed, confirmed_per_100k_inhabitants,
           deaths, death_rate, estimated_population_2019, date) -> df_covid
  
  return(df_covid)
}
  

