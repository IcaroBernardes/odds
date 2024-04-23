# 0. Setup inicial ################
## Carrega as bibliotecas
library(bslib)
library(sortable)
library(shiny)
library(glue)
library(purrr)
library(shinyjs)
library(stringr)
library(shinyWidgets)
library(dplyr)

## Carrega o modelo
modelo <- readRDS("assets/modelo.rds")

## Carrega os resultados
resultados <- readRDS("assets/resultados.RDS")

## Associa as diferentes formas dos nomes dos clubes
dicionario <- dplyr::tibble(
    imagem = c(
        'america-mg', 'athletico-pr', 'atletico-go', 'atletico-mg',
        'avai', 'bahia', 'botafogo', 'bragantino',
        'ceara', 'chapecoense', 'corinthians', 'coritiba',
        'criciuma', 'cruzeiro', 'csa', 'cuiaba',
        'figueirense', 'flamengo', 'fluminense', 'fortaleza',
        'goias', 'gremio', 'internacional', 'joinville',
        'juventude', 'palmeiras', 'parana', 'ponte-preta',
        'santa-cruz', 'santos', 'sao-paulo', 'sport',
        'vasco-da-gama', 'vitoria'
    ),
    simples = c(
        'América-MG', 'Athlético-PR', 'Atlético-GO', 'Atlético-MG',
        'Avaí', 'Bahia', 'Botafogo', 'Bragantino',
        'Ceará', 'Chapecoense', 'Corinthians', 'Coritiba',
        'Criciúma', 'Cruzeiro', 'CSA', 'Cuiabá',
        'Figueirense', 'Flamengo', 'Fluminense', 'Fortaleza',
        'Goiás', 'Grêmio', 'Internacional', 'Joinville',
        'Juventude', 'Palmeiras', 'Paraná', 'Ponte Preta',
        'Santa Cruz', 'Santos', 'São Paulo', 'Sport',
        'Vasco', 'Vitória'
    ),
    dados = c(
        'América (MG)', 'Ath Paranaense', 'Atl Goianiense', 'Atlético Mineiro',
        'Avaí', 'Bahia', 'Botafogo (RJ)', 'Bragantino',
        'Ceará', 'Chapecoense', 'Corinthians', 'Coritiba',
        'Criciúma', 'Cruzeiro', 'CSA', 'Cuiabá',
        'Figueirense', 'Flamengo', 'Fluminense', 'Fortaleza',
        'Goiás', 'Grêmio', 'Internacional', 'Joinville',
        'Juventude', 'Palmeiras', 'Paraná', 'Ponte Preta',
        'Santa Cruz', 'Santos', 'São Paulo', 'Sport Recife',
        'Vasco da Gama', 'Vitória'
    )
)

# 1. UI ################
ui <- page_fillable (
    
    ## Inclui o CSS e o controle de JS
    tags$head(includeCSS("www/styles.css")),
    shinyjs::useShinyjs(),
    
    layout_columns(
        col_widths = c(12, 12),
    navset_card_underline(
        id = "navegacao",

        ## Seleção do ano do campeonato
        nav_panel(
            title = "1. Ano",
            
            div(
                id = "containter-ano",
                "PREVISÃO DO BRASILEIRÃO",
                
                div(
                    id = "disclaimer",
                    p("⚠️"),
                    p("Esse app é um simples exercício.",
                      span("Não o use para realizar apostas esportivas."))
                ),
                
                selectizeInput(
                    inputId = "ano",
                    label = "Ano do campeonato",
                    choices = 2014:2024,
                    selected = 2024,
                    width = "300px"
                ),
                actionBttn(
                    inputId = "acessar",
                    label = "Escolher clubes",
                    size = "lg",
                    style = "minimal",
                    color = "primary"
                )
                
            )
        ),
        
        ## Seleção dos clubes a comparar
        nav_panel(
            title = "2. Clubes",
            uiOutput("listas"),
            uiOutput("resultado")
        ),
        
        ## Exibição dos resultados
        nav_panel(
            title = "3. Previsão",
            "Dynamically added content"
        )
    ),
    
    card(
        fill = FALSE,
        card_body(
            class = "bg-dark",
            "Modelo: Quemuel Baruque | App: Ícaro Bernardes | Dados: FBref"
        )
    )
    
    )
    
)

# 2. Server ################
server <- function(input, output) {
    
    ## Seleciona a 2a aba com o clique no botão de acesso
    observeEvent(req(input$acessar),{
        nav_select("navegacao", "2. Clubes")
    })
    
    ## Gera a lista de seleção dos clubes
    output$listas <- renderUI({
        
        req(input$ano)
        
        ### Lista os nomes dos clubes que competem no ano selecionado
        times <- resultados |> 
            dplyr::filter(season_end_year == input$ano) |> 
            dplyr::pull(home) |> 
            unique()
        
        ### Cria os itens da lista de seleção
        escudos <- dicionario |> 
            dplyr::filter(dados %in% times) |>
            dplyr::select(-dados) |> 
            purrr::pmap(\(imagem, simples) {
                tagList(
                    img(src = glue::glue("./escudos/{imagem}.svg"), class = "escudo"),
                    div(simples, class = "time")
                )
            })
        
        ### Nomeia os itens
        nomes <- dicionario |> 
            dplyr::filter(dados %in% times) |>
            dplyr::pull(dados)
        names(escudos) <- nomes
        
        ### Cria as listas
        sortable::bucket_list(
            header = "Arraste os clubes para montar a partida",
            group_name = "bucket_list_group",
            orientation = "horizontal",
            sortable::add_rank_list(
                text = "Clubes",
                labels = escudos,
                input_id = "rank_list_1"
            ),
            sortable::add_rank_list(
                text = "Partida",
                labels = NULL,
                input_id = "rank_list_2"
            )
        ) 
        
    })
    
    ## Condiciona a exibição dos resultados à seleção de um par de clubes
    observe({
        if (length(input$rank_list_2) == 2) {
            nav_show("navegacao", target = "3. Previsão")
        } else {
            nav_hide("navegacao", target = "3. Previsão")
        }
    })
    
    ## Controla a habilitação da lista de seleção e
    ## a exibição do "X" de versus
    observeEvent(req(input$rank_list_2), {
        
        shinyjs::toggleClass(
            selector = "#rank-list-rank_list_1 .rank-list-item",
            class = "disabled",
            condition = (length(input$rank_list_2) >= 2)
        )
        
        shinyjs::toggleClass(
            selector = "#rank_list_2",
            class = "versus",
            condition = (length(input$rank_list_2) >= 2)
        )
        
    })
    
    ## Efetua a predição
    predicao <- reactive({
        
        req(input$rank_list_2)
        if (length(input$rank_list_2) >= 2) {
            dados <- resultados |> 
                dplyr::filter(season_end_year == input$ano,
                              home == input$rank_list_2[1],
                              away == input$rank_list_2[2])
            
            predict(modelo, dados, type = "class")
        } else {
            NULL
        }
        
    })
    
    ## Exibe o resultado
    output$resultado <- renderUI({
        
        req(predicao())
        
        status <- dplyr::case_match(
            predicao(),
            "win" ~ "VITÓRIA",
            "draw" ~ "EMPATE",
            "lose" ~ "DERROTA"
        )
        
        div(strong(status), "DO MANDANTE", style = "text-align:center;")
        
    })
    
    
    
}

shinyApp(ui = ui, server = server)