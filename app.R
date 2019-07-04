
source("00Packages.R")

Title_SideBar_width <- "350px"
df_TripAdvisorSC <- readd("df_TripAdvisorSC")


# UI ----------------------------------------------------------------------

dbHeader <- dashboardHeader(title = "TripAdvisor SC",
                            tags$li(a(href = 'http://github.com/viniciusmsousa',
                                      'Powered by Vinicius M. de Sousa',
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"),
                            titleWidth = Title_SideBar_width)

SideBar <- dashboardSidebar(
  width = Title_SideBar_width,
  sidebarMenu(
    menuItem("Introducao",
             tabName = "painel_inicio",
             icon = icon("stream")),
    
    menuItem("Analise por Atração",
             tabName = "painel_por_atracao",
             icon = icon("search")),
    menuItem("Analise por tipo de turismo",
             tabName = "painel_por_tipo_turismo",
             icon = icon("align-left"))
  )
)

DashBody <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "painel_inicio",
      
      h3("Painel para colocar uma breve apresentacao da ferramenta, Explicando o objetivo
         bem como as funcionalidades basicas e de maneira sucinta"),
      h5(""),
      h5("'FREE YOUR MIND'"),
      h6("Morpheus")
    ),
    
    tabItem(
      tabName = "painel_por_atracao",
      
      h1("Instruções"),
      
      pickerInput(inputId = "classes",
                  label = "Selecione as classes",
                  choices = unique(df_TripAdvisorSC$Attraction),
                  options = list(`actions-box`=T),
                  multiple = T),
      
      # Filtro de tempo
      sliderInput(inputId = "data_filter",
                  label = "Selecione o Intervalo de Tempo:",
                  min = as.Date("1980-01-01","%Y-%m-%d"),
                  max = as.Date("2019-06-30","%Y-%m-%d"),
                  value=c(as.Date("2014-06-30"),as.Date("2018-06-30")),timeFormat="%Y-%m-%d"),
    ),
    
    tabItem(
      tabName = "painel_por_tipo_turismo",
      
      h1("Instruções"),
      
      pickerInput(inputId = "classes",
                  label = "Selecione as classes",
                  choices = unique(df_TripAdvisorSC$Attraction),
                  options = list(`actions-box`=T),
                  multiple = T),
      
      # Filtro de tempo
      sliderInput(inputId = "data_filter",
                  label = "Selecione o Intervalo de Tempo:",
                  min = as.Date("1980-01-01","%Y-%m-%d"),
                  max = as.Date("2019-06-30","%Y-%m-%d"),
                  value=c(as.Date("2014-06-30"),as.Date("2018-06-30")),timeFormat="%Y-%m-%d"),
      
      
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
  
  
  nrow <- reactive({})
  
  tabela <- reactive({
    
    str_split(string = list(input$key_words),
              pattern = "OR",
              simplify = T) %>%
      str_trim() %>%
      as.vector() -> splited
    
    qry <- str_remove(
      string = paste("SELECT * FROM patents_2304 WHERE",paste0(" CONTAINS (abstract, '",splited,"')"),
                     sep = " "),
      pattern = " OR "
    )
    
    dbGetQuery(con,
               qry) %>%
      as_tibble() %>%
      select(title,abstract, assignee, cpc, inventor, filing_date) %>% 
      mutate(cpc = if_else(
        condition = is.na(word_count(cpc)),
        true = "Sem Agregacao",
        false = str_sub(string = cpc,start = 11,end = 13)),
        assignee = str_remove_all(assignee, '\\"|\\[|\\]'),
        inventor = str_remove_all(inventor, '\\"|\\[|\\]'),
        year = ymd_hms(filing_date)
      )
  })
  
  output$tabela_filtrada <- renderDataTable({
    
    tabela() %>% 
      filter(cpc %in% input$classes,
             year > as.Date(input$data_filter[1]),
             year < as.Date(input$data_filter[2])) %>%
      
      rename(
        `Título` = title,
        Abstract = abstract,
        Assignee = assignee,
        CPC = cpc,
        Inventor = inventor
      ) %>% 
      select(-year)
    
  })
  
  
}
# Run the application
shinyApp(ui = ui, server = server)