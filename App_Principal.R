##Carregar os pacotes----
library(tidyverse)
library(shiny)
library(DT)
library(shinydashboard)
library(leaflet)
library(rnaturalearthdata)
library(rnaturalearth)
library(shinycssloaders)
library(sf)
library(plotly)

#Carregar os Datasets----
Medalhas <- read.csv("DataSets/Medalhas_Completas.csv")
Atletas <- read.csv("DataSets/atleta_resumo.csv")
Mapa_df <- read.csv("DataSets/Data_Mapa.csv")


#dashboard----
anos_olimpicos <- c(1896, 1900, 1904, 1908, 1912, 1920, 1924, 1928, 1932, 1936, 1948, 1952, 1956, 
                    1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 
                    2012, 2016, 2020, 2024)


ui <- dashboardPage(
  dashboardHeader(title = "Visualizações Olímpicas"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introdução", tabName = "introdução", icon = icon("info")),
      menuItem("Países", tabName = "paises", icon = icon("flag")),
      menuItem("Atletas", tabName = "atletas", icon = icon("users"))
    )
  ),
  
  dashboardBody(
    includeCSS("www/custom.css"),
    tabItems(
      # Aba de Introdução----
      tabItem(tabName = "introdução",
              fluidRow(
                box( #Box de Autor
                  title = tagList(icon("pencil"), "Autor"), status = "primary", solidHeader = TRUE, width = 3,
                  p("Luca Florentino Kosfeld", style = "font-size:25px"),
                  p("Estudante de Estatística da UFMG", style = "font-size:14px"),
                  tags$a(
                    href = "https://www.linkedin.com/in/luca-kosfeld",
                    tagList(icon("linkedin"), "LinkedIn"),
                    target = "_blank"
                  )
                ),
                box( #Box de Autor
                  title = tagList(icon("pen-clip"), "Orientador"), status = "primary", solidHeader = TRUE, width = 3,
                  p("Cristiano de Carvalho", style = "font-size:25px"),
                  p("Professor do Departamento de Estatística da UFMG", style = "font-size:14px"),
                  tags$a(
                    href = "https://www.est.ufmg.br/~cristianocs/",
                    tagList(icon("building-columns"), "Página do Professor"),
                    target = "_blank"
                  )
                ), 
                box( # Box de Metodologia
                  title = tagList(icon("laptop-code"), "Metodologia"), status = "warning", solidHeader = TRUE, width = 6,
                  p("A aplicação foi desenvolvida em ", tags$b("R"), " utilizando ", tags$b("Shiny"), 
                    " para criar uma interface web interativa."),
                  p("As visualizações foram geradas com ", tags$b("ggplot2"), " para gráficos e ", tags$b("leaflet"), 
                    " para mapas interativos."),
                  p("A manipulação dos dados foi realizada com os pacotes ", tags$b("dplyr"), " e ", tags$b("tidyr"), 
                    " garantindo precisão e organização das informações.")
                )
              ),
              fluidRow(
                box( # Box de Objetivo
                  title = tagList(icon("square-check"), "Objetivo"), status = "info", solidHeader = TRUE, width = 6,
                  p("Esta aplicação foi desenvolvida como parte de um projeto de extensão do ", 
                    tags$b("Departamento de Estatística da UFMG"), 
                    ", com o objetivo de visualizar dados das Olimpíadas de Verão (1896-2024) e 
    aplicar técnicas de limpeza de dados e criação de visualizações interativas.")
                ),
                box( # Box de Banco de Dados
                  title = tagList(icon("database"), "Bancos de Dados"), status = "success", solidHeader = TRUE, width = 6,
                  p("Os dados foram obtidos de duas principais fontes:"),
                  tags$ul(
                    tags$li(HTML('<b>Dataset 1</b>: Dataset retirado do site 
      <a href="https://basedosdados.org/dataset/62f8cb83-ac37-48be-874b-b94dd92d3e2b?table=567b1ccd-d8c2-4616-bacb-cf5c0e7b8d89" target="_blank">Base dos Dados</a>, 
      e possui os dados sobre os atletas e países das Olimpíadas de 1896 até 2020.')),
                    tags$li(HTML('<b>Dataset 2</b>: Dataset retirado do site 
      <a href="https://www.kaggle.com/datasets/sajkazmi/paris-olympics-2024-games-dataset-updated-daily" target="_blank">Kaggle</a>, 
      e possui os dados sobre os países das Olimpíadas de 2024.'))
                  )
                )
              )
      ),
      
      # Aba Países----
      tabItem(tabName = "paises",
              tabsetPanel(
                id = "pais_tabs",
                tabPanel("Mapa", icon = icon("earth-americas"),  #Mapa de Medalhas
                         fluidPage(
                           titlePanel("Mapa Interativo de Medalhas por País"),
                           leafletOutput("mapa", height = "80vh") %>% withSpinner() # Aumentar altura do mapa
                         )
                ),
                tabPanel("Evolução Temporal", icon = icon("chart-line"), #Histograma de Medalhas por edição
                         fluidRow(
                           column(width = 8,
                               selectInput("Country", label = "Selecione um país", 
                                           choices = unique(Medalhas$country), multiple = TRUE, 
                                           selected = c("Brazil", "United States of America"))
                           ),
                           column(width = 4,
                             radioButtons("var", "Escolha uma medalha", choices = c("Total", "Ouro", "Prata", "Bronze"), selected = "Total", inline = TRUE)
                           ),
                           box(width = 12, title = "Evolução Histórica das Medalhas", status = "info", solidHeader = TRUE,
                               plotOutput("plot", height = "390px") %>% withSpinner()
                           )
                         )
                ),
                tabPanel("Análise País Sede", icon = icon("chart-area"), #Análise do País Sede
                         fluidRow(
                           column(width = 3,
                                  selectInput("ano_sede", "Selecione o Ano",
                                              choices = setNames(unique(Medalhas$year), unique(Medalhas$ano_pais)), selected = 2016)),
                           column(width = 2,
                                  sliderInput("anos_antes", "Selecione quantas Edições anteriores comparar:",
                                              min = 1, max = 5, value = 3)),
                           column(width = 2,
                                  sliderInput("anos_depois", "Selecione quantas Edições posteriores comparar",
                                              min = 1, max = 5, value = 3)),
                           column(width = 5,
                                  radioButtons("var_sede", "Escolha uma medalha", choices = c("Ouro", "Prata", "Bronze", "Total"), selected = "Total"), inline = TRUE),
                           box(width = 12, title = "Análise de Desempenho do País Sede", status = "info", solidHeader = TRUE,
                               plotOutput("sede", height = "390px") %>% withSpinner())
                         )),
                tabPanel("Atletas por Medalha", icon = icon("diagram-project"),
                         fluidRow(
                           column(width = 4,
                                  selectInput("ano", "Selecione o Ano",
                                              choices = setNames(unique(Medalhas$year), unique(Medalhas$ano_pais)), selected = 2016)),
                           column(width = 4,
                                  radioButtons("var_pont", "Escolha uma medalha", choices = c("Ouro", "Prata", "Bronze", "Total"), selected = "Total", inline = TRUE)),
                           box(width = 12, title = "Número de Medalhas por Número de Atletas", status = "info", solidHeader = TRUE,
                               plotlyOutput("medalhas_atletas", height = "390px") %>% withSpinner())
                           
                         ))
              )
      ),
      
      # Aba Atletas----
      tabItem(tabName = "atletas",
              tabsetPanel(
                id = "atletas_tabs",
                tabPanel("Quadro de Atletas", icon = icon("table"),
                         fluidRow(
                           column(width = 4, 
                                  tags$h4("Selecione o País"),
                                  selectInput("country_atletas", NULL, 
                                              choices = c("Todos", unique(Atletas$country)), selected = "Brazil")
                           ),
                           column(width = 4, 
                                  tags$h4("Selecione o Esporte"),
                                  selectInput("sport_atletas", NULL, 
                                              choices = c("Todos", unique(Atletas$sport)), selected = "Artistic Gymnastics")
                           ),
                           column(width = 4, 
                                  tags$h4("Selecione o Ano"),
                                  selectInput("year_atletas", NULL, 
                                              choices = c("Todos", unique(Atletas$year)), selected = "Todos")
                           )
                         ),
                         fluidRow(
                           infoBoxOutput("maior_medalhas"),
                           infoBoxOutput("mais_novo"),
                           infoBoxOutput("mais_velho")
                         ),
                         fluidRow(
                           box(width = 12, title = "Tabela de Atletas", status = "info", solidHeader = TRUE,
                               withSpinner(DTOutput("tabela_filtrada_atletas"))
                           )
                         )
                ),
                tabPanel("Atletas por Ano", icon = icon("chart-column"),
                         fluidRow(
                           column(width = 6, title = "Filtros de Demografia",
                               radioButtons("visualizacao", "Selecione a Visualização", 
                                            choices = c("Feminino" = "F",
                                                        "Masculino" = "M",
                                                        "Total" = "total",
                                                        "Comparado" = "comparado"), 
                                            inline = TRUE, selected = "comparado")
                           ),
                           box(width = 12, title = "Atletas por Ano", status = "info", solidHeader = TRUE,
                               plotOutput("histograma")) %>% withSpinner()
                         )
                ),
                tabPanel("Pirâmide Etária", icon = icon("signal"),
                         fluidRow(
                           column(width = 4, title = "Filtros da Pirâmide",
                               selectInput("anos", "Selecione os Anos",
                                           choices = unique(Atletas$year), selected = c(2016, 2020), multiple = TRUE)),
                           column(width = 4, title = "Filtros da Pirâmide",
                                  selectInput("paises_prmd", "Selecione o País",
                                              choices = c("Todos", sort(unique(Atletas$country))), selected = "Brazil", multiple = FALSE)),
                           column(width = 2, title = "Opções de Visualização",
                               radioButtons("visualizacao2", "Selecione a Visualização",
                                            choices = c("Total" = "T",
                                                        "Percentual" = "P"),
                                            inline = FALSE, selected = "T")),
                           column(width = 2, title = "Opções de Atletas",
                               radioButtons("visualizacao_atletas", "Selecione a Visualização",
                                            choices =c("Medalhistas" = "M",
                                                       "Não Medalhistas" = "N",
                                                       "Todos" = "TDS"), inline = FALSE, selected = "TDS")),
                           box(width = 12, title = "Pirâmide Etária", status = "info", solidHeader = TRUE,
                               withSpinner(plotOutput("piramide", height = "390px")))
                         ))
              )
      )
    )
  ),
  skin = "black"
)



server <- function(input, output, session) {
  Country_Data <- reactive({
    Medalhas %>%
      filter(country %in% input$Country) %>%
      mutate(year = factor(year, levels = anos_olimpicos))
  })
  
  # Gráfico temporal para países----
  
  
  
  output$plot <- renderPlot({
    data1 <- Country_Data()
    ggplot(data = data1, aes(x = year, y = .data[[input$var]], color = country, group = country)) +
      geom_line(linewidth = 2.25) +
      geom_point(size = 4.75) +
      theme_minimal() +
      labs(
        title = paste("Medalhas de", paste(input$Country, collapse = ", "), "ao longo das Olimpíadas"),
        x = "Ano",
        y = "Número de Medalhas"
      ) +
      theme(
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        legend.position = "bottom"
      )
  })
  
  #Tabela filtrada de atletas----
  observeEvent(input$country_atletas, {
    # Filtrar os anos baseados no país selecionado
    anos_disponiveis <- Atletas %>%
      filter(if (input$country_atletas != "Todos") country == input$country_atletas else TRUE) %>%
      pull(year) %>%
      unique()
    
    # Atualizar o selectInput de anos
    updateSelectInput(session, "year_atletas",
                      choices = c("Todos", sort(anos_disponiveis)),
                      selected = input$year_atletas)
    
    # Filtrar os esportes baseados no país selecionado
    esportes_disponiveis <- Atletas %>%
      filter(if (input$country_atletas != "Todos") country == input$country_atletas else TRUE) %>%
      pull(sport) %>%
      unique()
    
    # Atualizar o selectInput de esportes
    updateSelectInput(session, "sport_atletas",
                      choices = c("Todos", sort(esportes_disponiveis)),
                      selected = input$sport_atletas)
  })
  
  
  observeEvent(input$year_atletas, {
    #Filtrar os esportes baseados no ano selecionado
    esportes_disponiveis <- Atletas %>%
      filter(if (input$year_atletas != "Todos") year == input$year_atletas else TRUE) %>%
      pull(sport) %>%
      unique()
    
    #Atualizar o selectInput de esportes
    updateSelectInput(session, "sport_atletas", 
                      choices = c("Todos", esportes_disponiveis),
                      selected = input$sport_atletas)
  })
  observeEvent(input$year_atletas, {
    #Filtrar os países baseados no ano selecionado
    paises_disponiveis <- Atletas %>%
      filter(if (input$year_atletas != "Todos") year == input$year_atletas else TRUE) %>%
      pull(country) %>%
      unique()
    
    #Atualizar o selectInput de países
    updateSelectInput(session, "country_atletas",
                      choices = c("Todos", paises_disponiveis),
                      selected = input$country_atletas)
  })
  
  observeEvent(input$sport_atletas, {
    # Filtrar os anos baseados no esporte selecionado
    anos_disponiveis <- Atletas %>%
      filter(if (input$sport_atletas != "Todos") sport == input$sport_atletas else TRUE) %>%
      pull(year) %>%
      unique()
    
    # Atualizar o selectInput de anos
    updateSelectInput(session, "year_atletas",
                      choices = c("Todos", sort(anos_disponiveis)),
                      selected = input$year_atletas)
    
    # Filtrar os países baseados no esporte selecionado
    paises_disponiveis <- Atletas %>%
      filter(if (input$sport_atletas != "Todos") sport == input$sport_atletas else TRUE) %>%
      pull(country) %>%
      unique()
    
    # Atualizar o selectInput de países
    updateSelectInput(session, "country_atletas",
                      choices = c("Todos", sort(paises_disponiveis)),
                      selected = input$country_atletas)
  })
  
  
  # Tabela reativa
  tabela_filtrada <- reactive({
    dados <- Atletas  
    
    if (input$country_atletas != "Todos") {
      dados <- dados %>%
        filter(country == input$country_atletas)
    }
    
    if (input$sport_atletas != "Todos") {
      dados <- dados %>%
        filter(sport == input$sport_atletas)
    }
    
    if (input$year_atletas != "Todos") {
      dados <- dados %>%
        filter(year == input$year_atletas) %>%
        select(-any_of(c("sex", "country_noc.x")))
    } else {
      dados <- dados %>%
        group_by(athlete_id) %>%
        summarize(
          athlete = first(athlete),
          Idade = first(Idade),
          sport = paste(unique(sport), collapse = ", "),
          year = paste(unique(year), collapse = ", "),
          country = first(country),
          Ouro = sum(Ouro),
          Prata = sum(Prata),
          Bronze = sum(Bronze)
        ) %>%
        mutate(Total = Ouro + Prata + Bronze)
    }
    
    dados <- dados %>%
      select(-athlete_id)
    
    return(dados)
  })
  
  # Renderizar tabela
  output$tabela_filtrada_atletas <- renderDT({
    datatable(
      tabela_filtrada(),
      class = 'stripe hover',
      rownames = FALSE,
      extensions = c('FixedHeader'),
      options = list(
        pageLength = 10,
        dom = 'tip',
        fixedHeader = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        )
      )
    )
  })
  
  #Info Boxes tabela----
  #Info Box Maior Medalhista
  output$maior_medalhas <- renderInfoBox({
    dados <- tabela_filtrada()
    
    # Verificar se a tabela tem dados
    if (nrow(dados) > 0) {
      maior_medalhista <- dados %>%
        arrange(desc(Total)) %>%  # Ordenar pelo total de medalhas
        slice(1)                 # Selecionar o primeiro (maior)
      
      # Nome do atleta e total de medalhas
      nome <- maior_medalhista$athlete
      total_medalhas <- maior_medalhista$Total
      
      infoBox(
        "Maior Medalhista",
        paste(nome, "-", total_medalhas, "medalhas"),
        icon = icon("medal"),
        color = "yellow"
      )
    } else {
      # Caso a tabela esteja vazia
      infoBox(
        "Maior Medalhista",
        "Nenhum dado disponível",
        icon = icon("times-circle"),
        color = "red"
      )
    }
  })
  #Info Box Mais novo
  output$mais_novo <- renderInfoBox({
    dados <- tabela_filtrada()
    
    #Verificar se a tabela tem dados
    if (nrow(dados) > 0) {
      mais_novo <- dados %>%
        arrange(Idade) %>%
        slice(1)
      
      #Nome do atleta e idade
      nome <- mais_novo$athlete
      idade <- mais_novo$Idade
      
      infoBox(
        "Mais Novo",
        paste(nome, "-", idade, "Idade"),
        icon = icon("child"),
        color = "aqua"
      )
    } else {
      infoBox(
        "Mais Novo",
        "Nenhum dado disponível",
        icon = icon("times-circle"),
        color = "red"
      )
    }
  })
  #Info Box Mais Velho
  output$mais_velho <- renderInfoBox({
    dados <- tabela_filtrada()
    
    #Verificar se a tabela tem dados
    if (nrow(dados) > 0) {
      mais_velho <- dados %>%
        arrange(desc(Idade)) %>%
        slice(1)
      
      #Nome do atleta e idade
      nome <- mais_velho$athlete
      idade <- mais_velho$Idade
      
      infoBox(
        "Mais Velho",
        paste(nome, "-", idade, "Idade"),
        icon = icon("person-cane"),
        color = "olive"
      )
    } else {
      infoBox(
        "Mais Velho",
        "Nenhum dado disponível",
        icon = icon("times-circle"),
        color = "red"
      )
    }
  })
  
  
  
  
  
  # Histograma demografico----
  hist_data <- reactive({
    dados <- Atletas %>%
      group_by(year, sex) %>%
      summarise(total = n_distinct(athlete_id), .groups = 'drop') %>%
      mutate(year = factor(year, levels = anos_olimpicos))
  })
  output$histograma <- renderPlot({
    data <- hist_data()
    if (input$visualizacao == "F"){
      dados_filtrados <- data %>% filter(sex == "Female")
      ggplot(dados_filtrados, aes(x = year, y = total, fill = sex)) +
        geom_bar(stat = "identity", color = "black") +
        labs(title = "Número de Atletas Femininas por Ano",
             x = "Ano", y = "Número de Atletas") +
        theme_minimal() +
        scale_fill_manual(values = c("Male" = "lightslateblue", "Female" = "firebrick2")) +
        theme(
          strip.text = element_text(size = 12),
          panel.spacing = unit(1, "lines"),
          plot.margin = margin(10, 10, 10, 10),
          axis.text.x = element_text(size = 12),  # Texto do eixo X
          axis.text.y = element_text(size = 14),  # Texto do eixo Y
          axis.title.x = element_text(size = 18), # Rótulo do eixo X
          axis.title.y = element_text(size = 18), # Rótulo do eixo Y
          legend.title = element_text(size = 18), # Título da legenda
          legend.text = element_text(size = 14)   # Texto da legenda
        )
      
    } else if (input$visualizacao == "M"){
      dados_filtrados <- data %>% filter(sex == "Male")
      ggplot(dados_filtrados, aes(x = year, y = total, fill = sex)) +
        geom_bar(stat = "identity", color = "black") +
        labs(title = "Número de Atletas Masculinos por Ano",
             x = "Ano", y ="Número de Atletas") +
        theme_bw() +
        scale_fill_manual(values = c("Male" = "lightslateblue", "Female" = "firebrick2")) +
        theme(
          strip.text = element_text(size = 12),
          panel.spacing = unit(1, "lines"),
          plot.margin = margin(10, 10, 10, 10),
          axis.text.x = element_text(size = 12),  # Texto do eixo X
          axis.text.y = element_text(size = 14),  # Texto do eixo Y
          axis.title.x = element_text(size = 18), # Rótulo do eixo X
          axis.title.y = element_text(size = 18), # Rótulo do eixo Y
          legend.title = element_text(size = 18), # Título da legenda
          legend.text = element_text(size = 14)   # Texto da legenda
        )
      
    } else if (input$visualizacao == "total"){
      dados_filtrados <- data %>%
        group_by(year) %>%
        summarize(total = sum(total, na.rm = TRUE))
      ggplot(dados_filtrados, aes(x = year, y = total)) +
        geom_bar(stat = "identity", fill = "lightblue", color = "black") +
        labs(title = "Total de Atletas por Ano",
             x = "Ano", y ="Número de Atletas") +
        theme_bw() +
        scale_fill_manual(values = c("Male" = "lightslateblue", "Female" = "firebrick2")) +
        theme(
          strip.text = element_text(size = 12),
          panel.spacing = unit(1, "lines"),
          plot.margin = margin(10, 10, 10, 10),
          axis.text.x = element_text(size = 12),  # Texto do eixo X
          axis.text.y = element_text(size = 14),  # Texto do eixo Y
          axis.title.x = element_text(size = 18), # Rótulo do eixo X
          axis.title.y = element_text(size = 18), # Rótulo do eixo Y
          legend.title = element_text(size = 18), # Título da legenda
          legend.text = element_text(size = 14)   # Texto da legenda
        )
      
    } else if (input$visualizacao == "comparado"){
      ggplot(data = data, aes(x = year, y = total, fill = sex)) +
        geom_bar(stat = "identity", position = "dodge", color = "black") +
        labs(title = "Comparação de Atletas Maculinos e Femininos por Ano",
             x = "Ano", y = "Número de Atletas", fill = "Sexo") +
        theme_bw() +
        scale_fill_manual(values = c("Male" = "lightslateblue", "Female" = "firebrick2")) +
        theme(
          strip.text = element_text(size = 12),
          panel.spacing = unit(1, "lines"),
          plot.margin = margin(10, 10, 10, 10),
          axis.text.x = element_text(size = 12),  # Texto do eixo X
          axis.text.y = element_text(size = 14),  # Texto do eixo Y
          axis.title.x = element_text(size = 18), # Rótulo do eixo X
          axis.title.y = element_text(size = 18), # Rótulo do eixo Y
          legend.title = element_text(size = 18), # Título da legenda
          legend.text = element_text(size = 14)   # Texto da legenda
        )
    }
  })
  # Piramide Etária----
  output$piramide <- renderPlot({
    brks <- c(0, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 
              32, 34, 36, 38, 40, 42, 44, 46, 48, 50, Inf)
    rotulos <- c("0-9", "10-11", "12-13", 
                 "14-15", "16-17", "18-19", "20-21", "22-23", "24-25", 
                 "26-27", "28-29", "30-31", "32-33", "34-35", "36-37", 
                 "38-39", "40-41", "42-43", "44-45", "46-47", "48-49", "50+")
    if (input$paises_prmd != "Todos") {
    Atletas <- Atletas %>%
      filter(country == input$paises_prmd)
    }
    
    if (input$visualizacao_atletas == "M"){ #Filtra o dataset para os atletas que ganharam medalhas e organiza para fazer a piramide
      prmd_data <- Atletas %>%
        filter(Total != 0) %>%
        filter(year %in% input$anos) %>%
        group_by(year, sex, Idade) %>%
        summarise(total = n_distinct(athlete_id), .groups = 'drop') %>%
        mutate(total = ifelse(sex == "Male", -total, total)) %>%
        mutate(faixa = cut(Idade, breaks = brks, labels = rotulos, right = FALSE)) %>%
        drop_na()
      
    } else if (input$visualizacao_atletas == "N"){ #Filtra o dataset para os atletas que não ganharam medalhas e organiza para fazer a piramide
      prmd_data <- Atletas %>%
        filter(Total == 0) %>%
        filter(year %in% input$anos) %>%
        group_by(year, sex, Idade) %>%
        summarise(total = n_distinct(athlete_id), .groups = 'drop') %>%
        mutate(total = ifelse(sex == "Male", -total, total)) %>%
        mutate(faixa = cut(Idade, breaks = brks, labels = rotulos, right = FALSE)) %>%
        drop_na()
      
    } else if (input$visualizacao_atletas == "TDS") { #Filtra o dataset para todos os atletas e organiza para fazer a piramide
      prmd_data <- Atletas %>%
        filter(year %in% input$anos) %>%
        group_by(year, sex, Idade) %>%
        summarise(total = n_distinct(athlete_id), .groups = 'drop') %>%
        mutate(total = ifelse(sex == "Male", -total, total)) %>%
        mutate(faixa = cut(Idade, breaks = brks, labels = rotulos, right = FALSE)) %>%
        drop_na()
    }
    
    if (input$visualizacao2 == "T") {
      ggplot(prmd_data, aes(x = faixa, y = total, fill = sex)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_y_continuous(labels = abs) +
        labs(x = "Faixa Etária", y = "Número de Atletas", fill = "Sexo") +
        theme_bw() +
        scale_fill_manual(values = c("Male" = "lightslateblue", "Female" = "firebrick2")) +
        facet_wrap(~year, ncol = 2) +
        theme(
          strip.text = element_text(size = 12),
          panel.spacing = unit(1, "lines"),
          plot.margin = margin(10, 10, 10, 10),
          axis.text.x = element_text(size = 14),  # Texto do eixo X
          axis.text.y = element_text(size = 14),  # Texto do eixo Y
          axis.title.x = element_text(size = 18), # Rótulo do eixo X
          axis.title.y = element_text(size = 18), # Rótulo do eixo Y
          legend.title = element_text(size = 18), # Título da legenda
          legend.text = element_text(size = 14)   # Texto da legenda
        )
    } else {
      prmd_data <- prmd_data %>%
        group_by(year) %>%
        mutate(total = round((total / sum(abs(total))) * 100, 2)) %>%  # Soma por ano e mantém a separação entre sexos
        ungroup()
      
      ggplot(prmd_data, aes(x = faixa, y = total, fill = sex)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_y_continuous(labels = abs) +
        labs(x = "Faixa Etária", y = "Percentual de Atletas", fill = "Sexo") +
        theme_bw() +
        scale_fill_manual(values = c("Male" = "lightslateblue", "Female" = "firebrick2")) +
        facet_wrap(~year, ncol = 2) +
        theme(
          strip.text = element_text(size = 12),
          panel.spacing = unit(1, "lines"),
          plot.margin = margin(10, 10, 10, 10),
          axis.text.x = element_text(size = 14),  # Texto do eixo X
          axis.text.y = element_text(size = 14),  # Texto do eixo Y
          axis.title.x = element_text(size = 18), # Rótulo do eixo X
          axis.title.y = element_text(size = 18), # Rótulo do eixo Y
          legend.title = element_text(size = 18), # Título da legenda
          legend.text = element_text(size = 14)   # Texto da legenda
        )
    }
  })
  
  #Mapa----
  output$mapa <- renderLeaflet({
    
    # Carregar dados geográficos dos países
    paises <- ne_countries(scale = "medium", returnclass = "sf")
    
    # Unir os dados de medalhas com os dados geográficos
    paises <- paises %>% 
      left_join(Mapa_df, by = c("name" = "country"))
    
    #Substituir NA por O
    paises$total_medalhas[is.na(paises$total_medalhas)] <- 0
    paises$ouro[is.na(paises$ouro)] <- 0
    paises$prata[is.na(paises$prata)] <- 0
    paises$bronze[is.na(paises$bronze)] <- 0
    
    
    # Criar um gradiente de cores com base no total de medalhas
    log_valores <- log(paises$total_medalhas + 1)  # Transformação logarítmica
    pal <- colorNumeric(palette = "Greens", domain = log_valores, na.color = "#cccccc")
    
    # Criar o mapa
    leaflet(paises) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(log(total_medalhas + 1)),
        weight = 1.3, color = "black", dashArray = "", fillOpacity = 0.7, # Destacar bordas em preto
        popup = paste(
          "<strong>", paises$name, "</strong><br>",
          "Total de Medalhas: ", paises$total_medalhas,  "<br>",
          "Ouro: ", paises$ouro, "<br>",      
          "Prata: ", paises$prata, "<br>",
          "Bronze: ", paises$bronze, "<br>"
        )
      ) %>%
      addLegend(
        pal = pal, values = ~log(total_medalhas + 1), opacity = 0.7, title = "Medalhas",
        position = "bottomright"
      ) %>%
      setView(lng = 0, lat = 20, zoom = 2.25) # Ajustar área de interesse
  })
  
  #Analise do País Sede----
  output$sede <- renderPlot({
    #Calcular os anos
    ano_selecionado <- as.numeric(input$ano_sede)
    edicoes_anteriores <- seq(ano_selecionado - 4, by = -4, length.out = as.numeric(input$anos_antes))
    edicoes_posteriores <- seq(ano_selecionado + 4, by = 4, length.out = as.numeric(input$anos_depois))
    anos_selecionados <- c(edicoes_anteriores, ano_selecionado, edicoes_posteriores)
    
    #Pegar os países sede
    pais_sede <- Medalhas %>%
      filter(year == ano_selecionado) %>%
      pull(pais_sede) %>%
      unique() %>%
      first()
    
    #Filtrar e sumarisar os dados
    dados_filtrados_sede <- Medalhas %>%
      group_by(year, country) %>%
      summarise(
        Total = sum(Total),
        Ouro = sum(Ouro),
        Prata = sum(Prata),
        Bronze = sum(Bronze),
        .groups = 'drop'
      ) %>%
      filter(
        year %in% anos_selecionados,
        country == pais_sede
      ) %>%
      arrange(year)
    
    #Criar o gráfico
    ggplot(data = dados_filtrados_sede, 
           aes(x = year, y = .data[[input$var_sede]])) +  
      geom_line(linewidth = 2.25, color = "dodgerblue3") +
      geom_point(size = 4.75, color = "dodgerblue3") +
      geom_vline(xintercept = ano_selecionado, 
                 linetype = "dashed", 
                 color = "red",
                 alpha = 0.8) +
      scale_x_continuous(breaks = anos_selecionados) +
      theme_minimal() +
      labs(
        title = sprintf("Desempenho do pais sede de  %d em edições adjacentes (%s)", 
                        ano_selecionado, 
                        pais_sede),
        subtitle = "Linha Vermela mostra ano em que foi sede",
        x = "Ano",
        y = "Medalhas"
      ) +
      theme(
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        legend.position = "bottom"
      )
  })
  
  #Medalhas pela qunatidade de atletas----
  output$medalhas_atletas <- renderPlotly({
    dados_paises <- Atletas %>%
      filter(year == input$ano) %>%
      group_by(year, country) %>%
      summarise(total_atletas = n_distinct(athlete_id), .groups = 'drop')
    
    medalhas_atletas_data <- Medalhas %>%
      left_join(dados_paises, by = c("country", "year")) %>%
      filter(!is.na(total_atletas))
    
    
    p <- ggplot(medalhas_atletas_data, aes(x = .data[[input$var_pont]], y = total_atletas, color = country)) +
      geom_point(size = 3) +
      theme_minimal() +
      theme(
        legend.position = "none",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5)
      ) +
      labs(
        x = "Medalhas",
        y = "Total de Atletas"
      )
    ggplotly(p) %>% layout(showlegend = FALSE)
  })

}
  
shinyApp(ui, server)
  
