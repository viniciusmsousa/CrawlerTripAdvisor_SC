# Carregando Pacotes e Definindo funções ----------------------------------

source("00Packages.R")

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
    # The '.' indicates the class
    html_nodes('.pageNum.last.taLnk') %>% 
    # Extract the raw text as a list
    html_text()                   
  
  # The second to last of the buttons is the one
  pages_data[(length(pages_data)-1)] %>%            
    # Take the raw string
    unname() %>%                                     
    # Convert to number
    as.numeric()                                     
}

get_reviews <- function(html){
  html %>% 
    # The relevant tag
    html_nodes('.partial_entry') %>%      
    html_text() %>% 
    # Trim additional white space
    str_trim() %>%     
    # Convert the list into a vector
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
  
  # Extract the Basic information from the HTML
  reviews <- get_reviews(html)
  reviewer_names <- get_reviewer_names(html)
  dates <- get_review_dates(html)
  titles <- get_review_title(html)
  locations <- get_reviewer_location(html)
  
  
  # Combine into a tibble
  combined_data <- tibble(reviewer = reviewer_names,
                          date = dates,
                          review = reviews[1:length(reviewer_names)],
                          title = titles,
                          location = locations) 
  
  # Tag the individual data with the company name
  combined_data %>% 
    mutate(Attraction = tourist_attraction) %>% 
    select(Attraction, reviewer, location,date,title ,review)
}


get_data_from_url <- function(url, tourist_attraction){
  
  html <- read_html(url)
  get_data_table(html, tourist_attraction)
}


baixar_comentario <- function(xml_nodeset, df_favoritos, ...){
  
  time_by_attraction <- tibble(
    attraction = df_favoritos$Nome,
    time = vector(mode = "character",length = length(df_favoritos$Nome))
  )
  
  for(i in seq_along(xml_nodeset)){
    #for(i in 1:2){  
    start_time <- Sys.time()
    scrape_write_table(url = df_favoritos$Link[i],
                       tourist_attraction = df_favoritos$Nome[i])
    time_by_attraction[i,2] <- Sys.time() - start_time
    
    
    print(paste0("Atração '",df_favoritos$Nome[i],"' (",i,") baixada com sucesso."))
  }
  
  rm(start_time)
  return(time_by_attraction)
  
}

# Fucoes para ler e preparar os dados

montar_df <- function(caminho){
  
  tsv_files <- list.files(path = caminho,pattern = "tsv")
  files <- paste0(getwd(),"/data/",tsv_files)
  df <- apply(X = rbindlist(lapply(files, read_tsv)),
              MARGIN = 2,
              FUN = iconv,
              from="UTF-8",
              to="ASCII//TRANSLIT") %>% 
    as_tibble()
  
  review_preprocessed <- df %>% 
    select("review") %>% 
    unlist() %>% 
    # remove non-alphanumeric symbols
    str_replace_all(.,"[^[:alnum:]]", " ") %>% 
    # collapse multiple spaces
    str_replace_all(.,"\\s+", " ") %>%
    #remove any numeric characters
    str_replace_all(.,"[[:digit:]]", "") %>%
    #remove punctuation
    str_replace_all(.,"[[:punct:]]","") %>%
    #remove stop words
    removeWords(.,stopwords("pt")) %>%
    #trim text
    str_trim(.,side=c("both"))
  
  df %>% 
    mutate(review_cleaned = review_preprocessed) 
  return(df)
}

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



get_attraction <- function(attraction,link){
  
  tourist_attraction <- attraction
  url <- link
  # Ler a primeira pagina com comentario
  first_page <- read_html(url)
  
  # Extrair numero de páginas
  #latest_page_number <- 2
  latest_page_number <- get_last_page(first_page)
  
  # Criando urls de interesse
  breaked_url <- str_split(url,"Reviews-",simplify = T)
  
  list_of_pages <- c(url,str_c(breaked_url[1],'Reviews-or',
                               seq(from=10,to=latest_page_number*10-10,by=10),
                               #seq(from=10,to=40,by=10),
                               '-',
                               breaked_url[2]))
  
  # extraindo o comentário de cada pagina e depois dando o 'bind' em uma coluna
  list_of_pages %>% 
    # aplicar em todas urls
    future_map(get_data_from_url, tourist_attraction,.progress = T) %>%  
    # combinando em uma tabela
    bind_rows() -> df_atracao  
  return(df_atracao)
  
}



# Getting attraction links and names --------------------------------------


main_page <- "https://www.tripadvisor.com.br/Attractions-g303570-Activities-State_of_Santa_Catarina.html"
html_main_page <- read_html(main_page)
favoritos_dos_viajantes <- html_main_page %>%
  html_nodes(".photo_link")

favoritos_dos_viajantes_links <- tibble(
  Nome = rep(NA,length(favoritos_dos_viajantes)),
  Link = rep(NA,length(favoritos_dos_viajantes))
)
for(i in seq_along(favoritos_dos_viajantes)){
  favoritos_dos_viajantes_links[i,1] <- str_replace_all(
    string = xml_attrs(xml_child(favoritos_dos_viajantes[[i]], 1))[["alt"]],
    pattern = " ",
    replacement = "_"
  )
  favoritos_dos_viajantes_links[i,2] <- paste0("https://www.tripadvisor.com.br",
                                        xml_attrs(favoritos_dos_viajantes[[i]])[["href"]])
}
favoritos_dos_viajantes_links


# Downloading data --------------------------------------------------------


bind_attractions <- function(df_favoritos,
                             col_link = "Link",
                             col_nome = "Nome"){
  
  nomes <- unlist(df_favoritos[1:5,col_nome])
  links <- unlist(df_favoritos[1:5,col_links])
  
  list_of_dfs <- vector(mode = "list",length = length(links))
  names(list_of_dfs) <- nomes
  
  
  for(i in seq_along(nomes)){
    print(paste0("Atração '",nomes[i],"' (",i,") de ",length(nomes)))
    get_attraction(attraction = nomes[i],
                   link = links[i]) -> df_attraction
    
    list_of_dfs[[i]] <- df_attraction
  } 
  
}


#df_favoritos <- favoritos_dos_viajantes_links
start_time <- Sys.time()
#for(i in seq_along(favoritos_dos_viajantes)){
for(i in 1:2){  
  scrape_write_table(url = favoritos_dos_viajantes_links$Link[i],
                     tourist_attraction = favoritos_dos_viajantes_links$Nome[i])
  print(paste0("Atração '",favoritos_dos_viajantes_links$Nome[i],"' (",i,") baixada com sucesso."))
}
end_time <- Sys.time()
time_get_data <- end_time - start_time
rm(start_time,end_time)



######




teste <- juntar_atracoes(df_favoritos = favoritos_dos_viajantes_links,
                            col_links = "Link",
                            col_nome = "Nome")






# Saving as unique datset -------------------------------------------------


tsv_files <- list.files(path = paste0(getwd(),"/data/"),pattern = "tsv")
files <- paste0(getwd(),"/data/",tsv_files)
tripadvisor_sc <- apply(X = rbindlist(lapply(files, read_tsv)),
                        MARGIN = 2,
                        FUN = iconv,
                        from="UTF-8",
                        to="ASCII//TRANSLIT")


write.csv(
  x = tripadvisor_sc,
  file = "tripadvisor_sc.csv",
  row.names = F
)

read_csv("tripadvisor_sc.csv")
##






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



######
favoritos_dos_viajantes <- readd("favoritos_dos_viajantes")





# Rascunho ----------------------------------------------------------------


readd("df_TripAdvisorSC1") %>% 
  as_tibble() -> df_TripAdvisorSC1

readd("df_TripAdvisorSC2") %>% 
  as_tibble() -> df_TripAdvisorSC2

readd("df_TripAdvisorSC3") %>% 
  as_tibble() -> df_TripAdvisorSC3

bind_rows(list(df_TripAdvisorSC1,
               df_TripAdvisorSC2,
               df_TripAdvisorSC3)) -> df_TripAdvisorSC
df_TripAdvisorSC

## Encontrando Tópicos

# stop words

library(tidytext)
dataframe <- df_TripAdvisorSC
rm(dataframe)



a <- stop_words_pt_e_personal(dataframe = df_TripAdvisorSC)
a












library(lexiconPT)
library(tidytext)

dict <- unique(data("sentiLex_lem_PT02"))



compute_sentiment <- function(df){
  
  df %>% 
    unnest_tokens(palavra, review) %>% 
    inner_join(sentiLex_lem_PT02, by = c("palavra" = "term")) %>% 
    group_by(Attraction) %>% 
    mutate(
      sentimento_soma = sum(polarity),
      sentimento_media = mean(polarity)
    ) -> df_sentimento
  
  return(df_sentimento)
}

df_TripAdvisorSC <- readd("df_TripAdvisorSC")
View(df_TripAdvisorSC)
# criacao do dataframe de sentimento por topico
df_sent <- df_TripAdvisorSC %>% 
  unnest_tokens(palavra, review) %>% 
  inner_join(sentiLex_lem_PT02, by = c("palavra" = "term")) %>% 
  group_by(Attraction) %>% 
  mutate(
    sentimento_soma = sum(polarity),
    sentimento_media = mean(polarity)
  )
df_sent %>% 
  glimpse()


library(ggplot2)
library(ggridges)
Attraction
roxo <- "#B2F4D2"
sent_medio_geral <- mean(df_sent$sentimento_media)
df_sent %>% 
  ungroup() %>%
  #  filter(Attraction %in% unique(df_sent$Attraction)[1:10]) %>% 
  mutate(Attraction = forcats::fct_reorder(Attraction, sentimento_media, median)) %>% 
  ggplot(aes(x = sentimento_media, y = Attraction)) +
  #facet_wrap( ~ Attraction, ncol=2)+
  geom_density_ridges_gradient(fill = roxo) +
  geom_vline(xintercept = sent_medio_geral, linetype = "dashed",size=1) +
  theme_minimal() +
  labs(x = "Sentimento", y = NULL,
       title = "Distribuição dos sentimentos por tópico") 




readd("SearchK") -> SearchK
readd("fitted") -> fitted

labelTopics(fitted)


readd("df_TripAdvisorSC") -> df_TripAdvisorSC

library(quanteda)
df_TripAdvisorSC[1:5,] -> a
a
adjs <- unlist(sentiLex_lem_PT02[sentiLex_lem_PT02$grammar_category =="Adj",1])


tokens_keep(x = tokens(a$review),
              adjs) -> b
str(b)
b[[3]]



df_TripAdvisorSC$data <- fix_date(df_TripAdvisorSC$date) 
df_TripAdvisorSC


df_sent <- df_TripAdvisorSC %>% 
  unnest_tokens(palavra, review) %>% 
  inner_join(sentiLex_lem_PT02, by = c("palavra" = "term")) %>%
  select(c(Attraction,polarity))
  #group_by(Attraction) %>% 
  mutate(
    sentimento_soma = sum(polarity),
    sentimento_media = mean(polarity)
  ) %>% glimpse()


