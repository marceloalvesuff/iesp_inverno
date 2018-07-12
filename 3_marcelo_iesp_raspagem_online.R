
# --------------------------- Coleta e sistematização de dados na web ---------------------
# --------------------------- IESP --------------------------------------------------------
# --------------------------- Marcelo Alves -----------------------------------------------
# --------------------------- Scraping -----------------------------------------------


########## instalar pacotes


library(pacman)



p_load(rvest) # conexão à API do youtube
p_load(stringr) # tratamento de dados
p_load(tidyr) # tratamento de dados
p_load(tidyverse) # framework para datascience


# Primeiro, é importante limpar nosso ambiente com a função rm()

rm(list = ls())

## definir diretório de trabalho

# atalho CTRL SHIFT H



### Vamos checar a documentação do rvest https://cran.r-project.org/web/packages/rvest/rvest.pdf




######################## Extração de tabelas ######################## 
#
#
#############################################################   




################## Raspagem de Dados - 1 Tabela

##### Definindo a URL a ser raspada

url <- "https://pt.wikipedia.org/wiki/Lista_de_presidentes_do_Brasil"

###### vamos chamar a função para baixar a página

pagina <- read_html(url)

### Inspecionar a página

### Definir onde está a tabela e chamar função para tabular


# Buscar pela tag table

tabela <- pagina %>%
  html_node("table") %>%
  html_table(fill = T)



# buscar pela CSS .wikitable
tabela <- pagina %>%
  html_node(".wikitable") %>%
  html_table(fill = T)


### O resultado é o mesmo

####### Exercício  - Raspar tabela dos partidos do Brasil

# https://pt.wikipedia.org/wiki/Partidos_políticos_no_Brasil



###### E quando há varias tabelas em uma página?


page <- read_html("http://www.bls.gov/web/empsit/cesbmart.htm")

page %>% 
  html_nodes("table")

### Procurar pelo seletor

# inspecionar tabela - Copy Selector

tabela <-  page %>%
  html_nodes("#Table2") %>% 
  html_table(fill = TRUE)


## percebam que veio uma lista
class(tabela[[1]])


### Vamos extrair o df
library(dplyr)
tabela <- tabela[[1]]
names(tabela)[6] <- "Differences_perc"
names(tabela)[8] <- "AbsoluteDiffs_perc"

tabela$Benchmark <- gsub(",", "",tabela$Benchmark)
tabela$`Estimate(2)` <- gsub(",", "",tabela$`Estimate(2)`)
tabela$Differences_perc <- gsub(",", "",tabela$Differences_perc)
tabela$AbsoluteDiffs_perc <- gsub(",", "",tabela$AbsoluteDiffs_perc)

tabela2 <- tabela %>%
  transmute(Benchmark = as.numeric(Benchmark),
         Estimate = as.numeric(`Estimate(2)`),
         Differences = as.numeric(Differences_perc),
         Absolute_Differences = AbsoluteDiffs_perc)

tabela2 <- tabela2[-c(1,2),]
sapply(tabela2, class) # checar classe


# eliminar duas primeiras linhas



##

######################## Tabular página ######################## 
#
#
#############################################################   



############# Exemplo 2 - Tabular elementos de uma página

# O Objetivo é montar um dataset com informações de cervejarias brasileiras

# Usaremos o site Brejas, um fórum de avaliação público

### Vou testar o CSS Selector na página e criar um vetor para cada informação que preciso
# Depois vou combinar as colunas em um dataset

# URL
base <- "http://www.brejas.com.br/cervejaria/microcervejaria"
pagina <- "?page=1" # Vamos começar pela página 1
URL <- paste0(base, pagina) # Gerar url final
pg <- read_html(URL) # ler a página


# Agora começamos a buscar as informações na página

### Nomes

  Nome <- pg %>% 
    html_nodes(".jrContentTitle") %>% 
    html_text() 

# limpar

Nome <- str_replace_all(Nome, "\n|Hot", "")

# Links

Link <- pg %>% 
  html_nodes(".jrContentTitle") %>% 
  html_nodes("a")  %>% 
  html_attr("href")

# Links das imagens


tmp <- pg %>% 
  html_nodes("img") %>% 
  html_attr("src")

tmp[grep("review", tmp)]


### Raspar Estado
Estado <- pg %>% 
  html_nodes(".jrEstadobr") %>% 
  html_nodes(".jrFieldValue") %>% 
  html_text() 


#### Raspar Cidade

Cidade <- pg %>% 
  html_nodes(".jrCidade") %>% 
  html_nodes(".jrFieldValue") %>% 
  html_text() 


#### Visualizações

vis <- pg %>% 
  html_nodes(".jrListingInfo") %>% 
  html_nodes(".jrListingStatus") %>% 
  html_nodes("span") %>% 
  html_text() 

### limpar vis é o mais complicado, porque a informação veio em texto
# Mas podemos perceber que há um padrão

vis <- vis[vis != ""]

### somente vis - 1 a 30 - vis está de 3 em 3
Visualização <- vis[seq(from = 1, to = 30, by = 3)]
Visualização <- as.numeric(Visualização)


# Data Frame

df <- data.frame(Nome, Estado, Cidade, Link, Visualização)


# Vejamos o resultado final

View(df)



######################## Exercício ######################## 
#
#
#############################################################   



#### Exercício - Raspe as informações de Westworld do site IMDb



# Usar a url:
url <- html("https://www.imdb.com/title/tt0475784/")

# Nota
nota <- url %>% 
  html_nodes("strong span") %>%
  html_text() %>%
  as.numeric()
nota


# Elenco
elenco <- url %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()
elenco

url %>%
  html_nodes(".imdbRating") %>%
  html_nodes("strong") %>%
  html_text()

url %>%
  html_nodes("time") %>%
  html_text()

# Avaliação
avaliacao <- url %>%
  html_nodes("#titleUserReviewsTeaser p") %>%
  html_text()
avaliacao



### Exercício raspe a tabela com os principais filmes

# https://www.imdb.com/chart/top?ref_=nv_mv_250_6






######################## Navegar pelas páginas ################
#
#
#############################################################   




### Como construir uma rotina para navegar por várias páginas?


#### Criar Rotina de Repetição


## Criar vetores

Nome <- vector()
Link <- vector()
Estado <- vector()
Cidade <- vector()
Visualização <- vector()
Imagem <- vector()

### São 39 páginas

# criar link
base <- "http://www.brejas.com.br/cervejaria/microcervejaria?page="
#URL <- paste0(base, x)


for (x in 1:5) {
  URL <- paste0(base, x)
  pg <- read_html(URL)
  
  
  
  ### Nomes
  
  tmp <- pg %>% 
    html_nodes(".jrContentTitle") %>% 
    html_text() 
  
  # limpar
  
  tmp <- str_replace_all(tmp, "\n|Hot", "")
  Nome <- c(Nome, tmp)
  # Links
  
  tmp <- pg %>% 
    html_nodes(".jrContentTitle") %>% 
    html_nodes("a")  %>% 
    html_attr("href")
  Link <- c(Link, tmp)
  
  #Imagem
  
  tmp <- pg %>% 
    html_nodes("img") %>% 
    html_attr("src")
  
  
  # prevenir erro
  tmp <- tmp[grep("review", tmp)]
  
  Imagem <- c(Imagem, tmp)
  
  ### Raspar Estado
  tmp <- pg %>% 
    html_nodes(".jrEstadobr") %>% 
    html_nodes(".jrFieldValue") %>% 
    html_text() 
  
  Estado <- c(Estado, tmp)
  #### Raspar Cidade
  
  tmp <- pg %>% 
    html_nodes(".jrCidade") %>% 
    html_nodes(".jrFieldValue") %>% 
    html_text() 
  
  
  if (length(tmp) != 10) {
    # encontrar qual esta faltando e passar NA
    inf <- pg %>% 
      html_nodes(".jrTableColumnMain") %>% 
      html_text()
    inf <- inf[-1]
    
    tmp <- append(x = tmp, 
                  values = NA,
                  after = which(!grepl("Cidade", inf)) -1)
    
  }
  if (length(tmp) != 10) {
    # encontrar qual esta faltando e passar NA
    inf <- pg %>% 
      html_nodes(".jrTableColumnMain") %>% 
      html_text()
    inf <- inf[-1]
    
    tmp <- append(x = tmp, 
                  values = NA,
                  after = which(!grepl("Cidade", inf)) -1)
    
  }
  if (length(tmp) != 10) {
    # encontrar qual esta faltando e passar NA
    inf <- pg %>% 
      html_nodes(".jrTableColumnMain") %>% 
      html_text()
    inf <- inf[-1]
    
    tmp <- append(x = tmp, 
                  values = NA,
                  after = which(!grepl("Cidade", inf)) -1)
    
  }
  Cidade <- c(Cidade, tmp)
  #### Visualizações
  
  vis <- pg %>% 
    html_nodes(".jrListingInfo") %>% 
    html_nodes(".jrListingStatus") %>% 
    html_nodes("span") %>% 
    html_text() 
  
  ### limpar
  vis <- vis[vis != ""]
  ### somente vis - 1 a 30 - vis está de 3 em 3
  vis <- vis[seq(from = 1, to = 30, by = 3)]
  vis <- as.numeric(vis)
  
  Visualização <- c(Visualização, vis)
  
  ## Criar sorteio para espera
  Sys.sleep(sample(1))
  
  print(paste0("Coletado da página", " ", x))
  
}



# Corrigir Vis - eliminar NA
Visualização <-Visualização[!is.na(Visualização)]



# Data Frame

df <- data.frame(Nome, Estado, Cidade, Visualização, Link, Imagem)



write.csv2(df, "Brejas_cervejarias.csv")



######################## Imprensa ########################### 
#
#
#############################################################   


######################### Extrair notícias da Folha de São Paulo sobre Lula


url_base <- "http://search.folha.uol.com.br/search?q=lula&site=todos&results_count=3769&search_time=0.033&url=http%3A%2F%2Fsearch.folha.uol.com.br%2Fsearch%3Fq%3Dmerenda%26site%3Dtodos&sr="

dados_pesquisa <- data_frame()

for (i in 1:10){
  
  print(i)
  
  i <- (i - 1) * 25 + 1
  
  url_pesquisa <- paste(url_base, i, sep = "")
  
  pagina <- read_html(url_pesquisa)
  
  nodes_titulos <- html_nodes(pagina, ".c-headline__title")
  
  titulos <- html_text(nodes_titulos)
  
  links <- pagina %>% 
    html_nodes(".c-headline__content") %>% 
    html_nodes("a")  %>% 
    html_attr("href")
  
  descricao <- pagina %>% 
    html_nodes(".c-headline__standfirst") %>% 
    html_text()  
  
  data <- pagina %>% 
    html_nodes(".c-headline__dateline") %>% 
    html_text()  
  
  tabela_titulos <- data.frame(titulos, data, descricao, links)
  
  dados_pesquisa <- bind_rows(dados_pesquisa, tabela_titulos)
}


########## Projeto

# Defina um objetivo de raspagem do seu interesse de pesquisa/trabalho 
# Encontre as páginas a serem raspadas
# Defina a rotina de construção das urls
# Especifique os nodes que possuem os metadados
# Teste implemente a rotina de coleta
