Sys.setenv(JAVA_HOME='C://Program Files//Java//jre1.8.0_211')

sql_translate_env.JDBCConnection <- dbplyr:::`sql_translate_env.Microsoft SQL Server`
sql_select.JDBCConnection <- dbplyr:::`sql_select.Microsoft SQL Server`

library(dbplyr)
library(DT)
library(dbplyr)
library(dplyr)
library(odbc)
library(tidyr)
library(DBI)
library(config)
library(stringr)
library(shiny)
library(readr)
library(rJava)
library(RJDBC)
library(qdap)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinycssloaders)

drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver",
            "C://Program Files//Microsoft JDBC Driver 6.0 for SQL Server//sqljdbc_6.0//enu//jre8//sqljdbc42.jar")
con <- dbConnect(drv,
                 "jdbc:sqlserver://patents.database.windows.net;user=dev;password=MeantrixCloud7895123;databaseName=complete_patents;")



loading_color = "#0d2c70"


# UI ----------------------------------------------------------------------

Title_SideBar_width <- "350px"

dbHeader <- dashboardHeader(title = "Pesquisador de Patentes",
                            tags$li(a(href = 'http://meantrix.com/',
                                      'Powered by Meantrix',
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"),
                            titleWidth = Title_SideBar_width)

SideBar <- dashboardSidebar(
  width = Title_SideBar_width,
  sidebarMenu(
    menuItem("Introducao",
             tabName = "painel_inicio",
             icon = icon("stream")),
    
    menuItem("Pesquisador Patentes",
             tabName = "painel_patentes",
             icon = icon("search"))
  )
)

DashBody <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "painel_inicio",
      
      h3("Painel para colocar uma breve apresentacao da ferramento, ou analise geral das patentes."),
      h5(""),
      h5("'FREE YOUR MIND'"),
      h6("Morpheus")
    ),
    
    
    # tabela com a busca das patentes
    tabItem(
      tabName = "painel_patentes",
      
      h1("Instruções para pesquisa"),
      
      h4("Primeiro, digite as palavras para serem buscadas nos abstracts das patentes na caixa
                'Digite sua pesquisa' e clique no botão 'Pesquisar'. Em seguida espere a busca 
                ser concluída. Isso ocorreá quando aparecer a caixa 'Classes das Patentes 
                retornadas. Selecione as de interesse:'. Então selecione as classes desejadas e clique no botão
                'Trazer resultados'. Em seguida aparecerá a tabela com as patentes."),
      
      textInput(inputId = "key_words",
                label = "Digite sua pesquisa",
                value = "water"),
      
      h5("Digite os termos da pesquisa separa por 'OR'. Exemplo: pigment OR method OR vacumn"),
      
      submitButton(text="Pesquisar"),
      
      h2("Classes das Patentes retornadas. Selecione as de interesse:"),
      
      uiOutput("selecionar_classes")  %>% withSpinner(color=loading_color),
      
      submitButton(text="Trazer Resultados"),
      
      h2("Tabela com Artigos"),
      
      dataTableOutput("tabela_filtrada_cpc") %>% withSpinner(color=loading_color)
      
    )
  )
)



ui <- dashboardPage(
  header = dbHeader,
  sidebar = SideBar,
  body = DashBody,
  skin = "black"
)




# Server ------------------------------------------------------------------


server <- function(input, output) {
  
  
  tabela <- reactive({
    str_split(string = list(input$key_words),
              pattern = "OR",
              simplify = T) %>%
      str_trim() %>%
      as.vector() -> splited
    
    qry <- str_remove(
      string = paste("SELECT TOP 50 * FROM patents_2304 WHERE",paste0(" CONTAINS (abstract, '",splited,"')"),
                     sep = " "),
      pattern = " OR "
    )
    
    dbGetQuery(con,
               qry) %>%
      as_tibble() %>%
      select(title,abstract, assignee, cpc, inventor) %>% 
      mutate(cpc = if_else(
        condition = is.na(word_count(cpc)),
        true = "Sem Agregacao",
        false = str_sub(string = cpc,start = 11,end = 13)),
        assignee = str_remove_all(assignee, '\\"|\\[|\\]'),
        inventor = str_remove_all(inventor, '\\"|\\[|\\]')
      )
  })
  
  output$query <- renderText({
    
    str_split(string = list(input$key_words),
              pattern = "OR",
              simplify = T) %>%
      str_trim() %>%
      as.vector() -> splited
    
    str_remove(
      string = paste("SELECT top 50 * FROM patents_2304 WHERE",paste0(" CONTAINS (abstract, '",splited,"')"),
                     # paste0("abstract LIKE '%",splited,"%'",collapse = " OR "),
                     sep = " "),
      pattern = " OR "
    )
    
  })
  
  output$selecionar_classes = renderUI({
    str_split(string = list(input$key_words),
              pattern = "OR",
              simplify = T) %>%
      str_trim() %>%
      as.vector() -> splited
    
    qry <- str_remove(
      string = paste("SELECT top 50 * FROM patents_2304 WHERE",paste0(" CONTAINS (abstract, '",splited,"')"),
                     sep = " "),
      pattern = " OR "
    )
    
    dbGetQuery(con,
               qry) %>%
      as_tibble() %>%
      select(cpc) %>%
      mutate(cpc = if_else(
        condition = is.na(word_count(cpc)),
        true = "Sem Agregacao",
        false = str_sub(string = cpc,start = 11,end = 13))
      ) %>%
      unlist() %>%
      unique() -> cpcs
    
    
    selectInput(inputId = 'classes',label =  '',
                choices = cpcs,
                multiple = T)
  })
  
  output$tabela_filtrada_cpc <- renderDataTable({
    
    tabela() %>% 
      filter(cpc %in% input$classes) %>% 
      rename(
        `Título` = title,
        Abstract = abstract,
        Assignee = assignee,
        CPC = cpc,
        Inventor = inventor
      )
    
  })
  
  
}
# Run the application
shinyApp(ui = ui, server = server)