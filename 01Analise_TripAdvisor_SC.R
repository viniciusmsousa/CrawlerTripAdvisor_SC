# Carregando Pacotes e Definindo funções ----------------------------------

source("00Packages.R")

# Funções para Scraping

xml_nodeset_to_df <- function(xml_nodeset){
  favoritos_dos_viajantes_links <- tibble(
    Nome = rep(NA,length(xml_nodeset)),
    Link = rep(NA,length(xml_nodeset))
  )
  for(i in seq_along(xml_nodeset)){
    favoritos_dos_viajantes_links[i,1] <- str_replace_all(
      string = xml_attrs(xml_child(xml_nodeset[[i]], 1))[["alt"]],
      pattern = " ",
      replacement = "_"
    )
    favoritos_dos_viajantes_links[i,2] <- paste0("https://www.tripadvisor.com.br",
                                                 xml_attrs(xml_nodeset[[i]])[["href"]])
  }
  return(favoritos_dos_viajantes_links)
}

get_last_page <- function(html){
  
  pages_data <- html %>%
    html_nodes('.pageNum.last.taLnk') %>% 
    html_text()                   
  
  
  pages_data[(length(pages_data)-1)] %>%            
    unname() %>%     
    as.numeric()                                     
}

get_reviews <- function(html){
  html %>% 
    
    html_nodes('.partial_entry') %>%      
    html_text() %>%
    str_trim() %>%     
    unlist() 
}

get_reviewer_names <- function(html){
  html %>% 
    html_nodes('.thankUser') %>% 
    html_text() %>% 
    str_trim() %>% 
    str_remove_all("Obrigado, ") %>% 
    unlist()
}

get_reviewer_location <- function(html){
  html %>% 
    html_node(".userLoc") %>% 
    html_text() %>% 
    str_trim()
}

get_review_dates <- function(html){
  html %>% 
    html_nodes('.prw_rup.prw_reviews_stay_date_hsx') %>% 
    html_text() %>% 
    str_trim() %>% 
    str_remove_all("Data da experiência: ")
}

get_review_title <- function(html){
  html %>% 
    html_nodes('.noQuotes') %>% 
    html_text() %>% 
    str_trim()
}

get_data_table <- function(html, tourist_attraction){
  
  # Extraindo informação basica do html
  reviews <- get_reviews(html)
  reviewer_names <- get_reviewer_names(html)
  dates <- get_review_dates(html)
  titles <- get_review_title(html)
  locations <- get_reviewer_location(html)
  
  
  # cobinando em um tibble
  combined_data <- tibble(reviewer = reviewer_names,
                          date = dates[1:length(reviewer_names)],
                          review = reviews[1:length(reviewer_names)],
                          title = titles[1:length(reviewer_names)],
                          location = locations[1:length(reviewer_names)]) 
  
  # Adicionando a coluna com o nome da atracao
  combined_data %>% 
    mutate(Attraction = tourist_attraction) %>% 
    select(Attraction, reviewer, location,date,title ,review)
}

get_data_from_url <- function(url, tourist_attraction){
  html <- read_html(url)
  get_data_table(html, tourist_attraction)
}

get_attraction <- function(attraction,link){
  
  tourist_attraction <- attraction
  url <- link
  # Ler a primeira pagina com comentario
  first_page <- read_html(url)
  
  # Extrair numero de páginas
  #latest_page_number <- 5
  latest_page_number <- get_last_page(first_page)
  
  # Criando urls de interesse
  breaked_url <- str_split(url,"Reviews-",simplify = T)
  
  list_of_pages <- c(url,str_c(breaked_url[1],'Reviews-or',
                               seq(from=10,to=latest_page_number*10-10,by=10),
                               #seq(from=10,to=40,by=10),
                               '-',
                               breaked_url[2]))
  plan(multiprocess)
  # extraindo o comentário de cada pagina e depois dando o 'bind' em uma coluna
  list_of_pages %>% 
    # aplicar em todas urls
    future_map(get_data_from_url, tourist_attraction,.progress = T) %>%  
    # combinando em uma tabela
    bind_rows() -> df_atracao     
  
  return(df_atracao)
  
}

# bind_attraction vetorizado, mas sem acompanhar o progresso
# bind_attractions <- function(df_favoritos,
#                              col_links = "Link",
#                              col_nome = "Nome"){
#   
#   nomes <- unlist(df_favoritos[1:5,col_nome])
#   links <- unlist(df_favoritos[1:5,col_links])
#   
#   list_of_dfs <- vector(mode = "list",length = length(links))
#   names(list_of_dfs) <- nomes
#   
#   plan(sequential)
#   future_map2(nomes,links,get_attraction,.progress = T) %>% 
#     bind_rows() -> df
#   
#   return(df)
#   
# }


# bind_attraction sem ser vetorixado, mas acompanhando o progresso
bind_attractions <- function(df_favoritos,
                             col_links = "Link",
                             col_nome = "Nome"){
  
  nomes <- unlist(df_favoritos[,col_nome])
  links <- unlist(df_favoritos[,col_links])
  
  list_of_dfs <- vector(mode = "list",length = length(links))
  names(list_of_dfs) <- nomes
  
  
  for(i in seq_along(nomes)){
    print(paste0("Atração '",nomes[i],"' (",i," de ",length(nomes),")"))
    get_attraction(attraction = nomes[i],
                   link = links[i]) -> df_attraction
    
    list_of_dfs[[i]] <- df_attraction
  } 
  
  df_final <- bind_rows(list_of_dfs)
  return(df_final)
  
}

# Funcoes para ler e preparar os dados


mes <- tibble(month =c("janeiro","fevereiro","março","abril","maio","junho","julho",
                       "agosto","setembro","outubro","novembro","dezembro"),
              month_number = 1:12)

fix_date <- function(vetor){
  # usar essa funcao depois de ter o dataset completo para arrumar a data
  str_split(vetor," de ",simplify = T) %>% 
    as_tibble() %>% 
    rename(month = V1,
           year = V2) %>% 
    mutate(year=as.integer(year)) %>% 
    left_join(mes,
              "month") %>% 
    mutate(data = as_date(as.yearmon(paste(month_number,year,sep = "-"),"%m-%Y"))) %>% 
    select(data) %>% 
    as.vector()
}


# Analise -----------------------------------------------------------------


analise <- drake_plan(
  
  # Pagina de SC no TripAdvisor e links dos favoritos
  Pagina = "https://www.tripadvisor.com.br/Attractions-g303570-Activities-State_of_Santa_Catarina.html",
  TripAdvisorSC = read_html(Pagina),
  Favoritos_Nodeset = TripAdvisorSC %>%
    html_nodes(".photo_link"),
  Favoritos_dos_Viajantes = xml_nodeset_to_df(Favoritos_Nodeset),
  
  # Baixando os Comentários de cada atração
  df_TripAdvisorSC = bind_attractions(df_favoritos = Favoritos_dos_Viajantes)
)

config <- drake_config(analise) 
vis_drake_graph(config,
                main = "Fluxo do Projeto TripAdvisor SC")
make(analise)



# Rascunho ----------------------------------------------------------------


readd("df_TripAdvisorSC") %>% 
  as_tibble() -> df_TripAdvisorSC

View(df_TripAdvisorSC)
