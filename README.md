# Visualizações Olímpicas: Uma Análise Interativa dos Dados Olímpicos com Shiny

[![Linguagem](https://img.shields.io/badge/Linguagem-R-blue)](https://www.r-project.org/)
[![Framework](https://img.shields.io/badge/Framework-Shiny-E69F00)](https://shiny.rstudio.com/)

Este repositório contém o código-fonte do projeto "Visualizações Olímpicas", um aplicativo Shiny desenvolvido em R como Atividade Complementar de Iniciação Científica na Universidade Federal de Minas Gerais (UFMG). O objetivo principal é permitir a exploração e visualização interativa de dados históricos e recentes dos Jogos Olímpicos de Verão.

* **Autor:** Luca Florentino Kosfeld
* **Orientador:** Dr. Cristiano de Carvalho Santos
* **Instituição:** Universidade Federal de Minas Gerais - Departamento de Estatística
* **Link para o Aplicativo:** [https://lkosfeld.shinyapps.io/Visualizacoes_Olimpicas/](https://lkosfeld.shinyapps.io/Visualizacoes_Olimpicas/)

## Sumário

* [Visão Geral](#visão-geral)
* [Funcionalidades do Aplicativo](#funcionalidades-do-aplicativo)
* [Fontes dos Dados](#fontes-dos-dados)
* [Tecnologias Utilizadas](#tecnologias-utilizadas)
* [Como Executar o Projeto Localmente](#como-executar-o-projeto-localmente)

## Visão Geral

Os Jogos Olímpicos, um evento com raízes que remontam a 2500 a.C. na Grécia Antiga e revitalizados em 1896, transformaram-se no maior espetáculo esportivo global. Ao longo de sua história, os Jogos passaram por profundas transformações, refletindo a evolução da sociedade, com um aumento expressivo no número de modalidades e eventos, maior participação feminina, inclusão de atletas paralímpicos e avanços tecnológicos.

Este projeto propõe um aplicativo Shiny para a análise abrangente dos dados olímpicos, explorando aspectos como a distribuição de medalhas por país, a evolução do desempenho dos atletas ao longo do tempo, a representatividade de gênero nas diferentes modalidades e o impacto de fatores socioeconômicos no sucesso olímpico das nações. O objetivo é tornar os dados compreensíveis tanto para especialistas quanto para o público em geral, promovendo o conhecimento sobre este evento milenar.

## Funcionalidades do Aplicativo

O aplicativo "Visualizações Olímpicas" está estruturado em três abas principais para facilitar a navegação e a exploração dos dados:

### Aba Introdução

* Apresenta uma página inicial com informações gerais sobre o projeto.

### Aba Países

Esta aba é dedicada à análise do desempenho olímpico dos países participantes, utilizando visualizações interativas. É subdividida em:

* **Mapa**
  
* **Evolução Temporal**

* **Análise País Sede**

* **Atletas por Medalha**


### Aba Atletas

Focada na análise do perfil e desempenho dos atletas olímpicos, esta aba também possui sub-seções:

* **Quadro de Medalhas**

* **Atletas por Ano**

* **Pirâmide Etária**

## Fontes dos Dados

O aplicativo utiliza dados de duas fontes principais:

1.  **Dados Históricos (1896 - 2020):** Contém informações detalhadas sobre atletas (idade, peso, altura, gênero), sua participação e desempenho nas competições (modalidades, medalhas), além de dados agregados por país. Os arquivos de dados incluem `summer.csv` e `winter.csv`. O arquivo `dictionary.csv` pode conter metadados ou descrições das variáveis.
    * *Fonte: Base dos Dados.*
2.  **Dados Olímpicos de 2024:** Concentra-se em dados dos países participantes dos Jogos Olímpicos de 2024, permitindo análises comparativas com o histórico.
    * *Fonte: Kaggle.*

**Tratamento dos Dados:** Incluiu a padronização de variáveis com inconsistências históricas (ex: nomes de países, recategorização de modalidades) e cruzamentos para identificar padrões relevantes.

## Tecnologias Utilizadas

* **Linguagem de Programação:** R
* **Framework para Aplicativo Web:** Shiny
* **Principais Pacotes R (Sugestão - adicione/remova conforme os que você usou):**
    * `shiny`
    * `tidyverse` (para manipulação de dados)
    * `ggplot2` (para gráficos estáticos e base para interativos)
    * `plotly` (para gráficos interativos)
    * `rnaturalearthdata` e `rnaturalearth` (para criação de mapas estáticos)
    * `leaflet` (para mapas interativos)
    * `DT` (para tabelas interativas)
    * *Entre outros com menos destaque.*

## Como Executar o Projeto Localmente

Para executar este aplicativo Shiny localmente, siga os passos abaixo:

1.  **Clone o repositório:**
     ```bash
    git clone [https://github.com/LKosfeld/Shiny-Olimpiadas.git](https://github.com/LKosfeld/Shiny-Olimpiadas.git)
    cd Shiny-Olimpiadas
    ```
2.  **Certifique-se de ter o R e o RStudio instalados.**
3.  **Instale os pacotes R necessários.**
    ```R
    install.packages(c("tidyverse", "shiny", "DT", "shinydashboard", "leaflet",
                       "rnaturalearthdata", "rnaturalearth", "shinycssloaders", "sf", "plotly"))
    ```
4.  **Abra o arquivo `Shiny_Olimpiadas.Rproj` no RStudio.** Isso definirá automaticamente o diretório de trabalho correto.
5.  **Execute o aplicativo:**
    * Abra o arquivo `App_Principal.R` no RStudio e clique no botão "Run App" na parte superior do editor.
    * Alternativamente, você pode executar no console R:
        ```R
        shiny::runApp("App_Principal.R")
        ```
