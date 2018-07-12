##### Raspagem de Dados Agência Lupa

rm(list=ls())
### Vou começar pelo rtweet para conseguir os links

pacman::p_load(rvest)
pacman::p_load(lubridate)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
### Projeto de raspagem

# Começar levantando as URLS



### criar loop da url
base <- "https://piaui.folha.uol.com.br/lupa/"
t1 <- Sys.Date() -10
data <- format(t1, "%Y/%m") 


for (x in 1:33) {

 print(paste0(base, data)) 
  t1 <- t1  - months(1)
  data <- format(t1, "%Y/%m")
}


### campos a serem coletados
url <- paste0(base, data)
pg <- read_html(url)


# na primeira raspagem, só quero o link

titulo <- pg %>%
  html_nodes(".column-main") %>%
  html_nodes(".bloco-title") %>% 
  html_text() 

link <- pg %>%
  html_nodes(".column-main") %>%
  html_nodes(".bloco-title") %>% 
  html_nodes("a") %>% 
  html_attr("href")

tmp <- data.frame(titulo, link)

# cada pagina exibe 10 resultados

mais <- pg %>%
  html_nodes(".btnvermais") %>% 
  html_attr("href")


### loop

### criar loop da url
base <- "https://piaui.folha.uol.com.br/lupa/"
t1 <- Sys.Date() -10
data <- format(t1, "%Y/%m") 
titulo <- vector()
link <- vector()
publicado <- vector()
autor <- vector()

for (x in 1:3) {
  url <- paste0(base, data)
  
  print(paste0("Coletando de ", base, data)) 
  t1 <- t1  - months(1)
  data <- format(t1, "%Y/%m")
  
  pg <- read_html(url)
  
  t <- pg %>%
    html_nodes(".column-main") %>%
    html_nodes(".bloco-title") %>% 
    html_text() 
  
  titulo <- c(titulo , t)
  
  l <- pg %>%
    html_nodes(".column-main") %>%
    html_nodes(".bloco-title") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  link <-c(link , l)
  
  p <- pg %>%
    html_nodes(".column-main") %>%
    html_nodes(".bloco-meta") %>% 
    html_text() 
  publicado <-c(publicado , p)
  
  a <- pg %>%
    html_nodes(".column-main") %>%
    html_nodes(".bloco-autor") %>% 
    html_text() 
  
  autor <-c(autor , a)
  
  rm(t,l, p,a)
 
  ## possui botão ver mais?
  
  mais <- pg %>%
    html_nodes(".btnvermais") %>% 
    html_attr("href")
   
  if (length(mais) != 0) {
  
  while(mais!= url) {
    
      
    pg <- read_html(mais)
  url <- mais
  
  t <- pg %>%
    html_nodes(".column-main") %>%
    html_nodes(".bloco-title") %>% 
    html_text() 
  
  titulo <- c(titulo , t)
  
  l <- pg %>%
    html_nodes(".column-main") %>%
    html_nodes(".bloco-title") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  link <-c(link , l)
  
  a <- pg %>%
    html_nodes(".column-main") %>%
    html_nodes(".bloco-autor") %>% 
    html_text() 
  
  autor <-c(autor , a)
  
  rm(t,l, p,a)
    
    mais <- pg %>%
    html_nodes(".btnvermais") %>% 
    html_attr("href")
    if (length(mais) == 0) {mais <- url}
  }
    }
}




final <- data.frame(titulo, link)

write.csv2(final, "lupa_links.csv")

############################## Agora já tenho os links, vamos raspar as demais informações das materias
final$link <- as.character(final$link)


############ Teste


pg <- read_html(final$link[333])


# na primeira raspagem, só quero o link
library(stringr)


# publicado
pg %>%
  html_nodes(".column-main") %>%
  html_nodes(".bloco-meta") %>% 
  html_text() %>%
  str_replace_all("[\r\n]" , "")%>%
  trim()


#autor

pg %>%
  html_nodes(".column-main") %>%
  html_nodes(".bloco-autor") %>% 
  html_text() %>%
  str_replace_all("[\r\n]" , "") %>%
  trim()


# etiquetas

pg %>%
  html_nodes(".column-main") %>%
  html_nodes(c(".etiqueta-2")) %>% 
  html_text() 


# figuras

pg %>%
  html_nodes(".aligncenter") %>% 
  html_attr("src")



# texto

pg %>%
  html_nodes(".column-main") %>%
  html_nodes(c("span")) %>% 
  html_text() %>%
  paste(collapse=" ")



### Implementação


### loop
publicado <- vector()
label <- vector()
texto <- vector()
figura <- vector()

for (x in 1:nrow(final)) {
  
  pg <- read_html(final$link[x])
  
   a <- pg %>%
    html_nodes(".column-main") %>%
    html_nodes(".bloco-autor") %>% 
    html_text() %>%
    str_replace_all("[\r\n]" , "") %>%
    trim()
  
   publicado <-c(publicado , a)
  # etiquetas
  
  b <- pg %>%
    html_nodes(".column-main") %>%
    html_nodes(c(".etiqueta")) %>% 
    html_text() %>%
    paste(collapse=" ")
  
  label <-c(label , b)
  
  e <- pg %>%
    html_nodes(".aligncenter") %>% 
    html_attr("src") %>%
    paste(collapse=" ")
  figura <-c(figura , e)
  
  # texto
  
  c <- pg %>%
    html_nodes(".column-main") %>%
    html_nodes(c("span")) %>% 
    html_text() %>%
    paste(collapse=" ")
  
  texto <- c(texto, c)
  
  d <- pg %>%
    html_nodes(".column-main") %>%
    html_nodes(".bloco-autor") %>% 
    html_text() %>%
    str_replace_all("[\r\n]" , "") %>%
    trim()  %>%
    paste(collapse=" ")
  
  autor <- c(autor, d)
  
  print(x)
  
}

### limpeza e preparação dos dados

# gerar data frame
resultado <- data.frame(titulo, link, publicado, label, figura, texto)

# há dois tipos de labels, criar colua unificando
resultado$tmp <- paste0(resultado$label, resultado$figura)

### criar contagem das labels

resultado$FALSO <- str_count(resultado$tmp, "FALSO")
resultado$EXAGERADO <- str_count(resultado$tmp, "EXAGERADO")
resultado$CONTRADITORIO <- str_count(resultado$tmp, "CONTRADIT")
resultado$OLHO <- str_count(resultado$tmp, "OLHO")
resultado$INSUSTENTAVEL <- str_count(resultado$tmp, "INSUSTENTAVEL|INSUSTENTÁVEL")
resultado$CEDO <- str_count(resultado$tmp, "CEDO")
resultado$VERD_MAS <- str_count(resultado$tmp, "MAS")

## arrumar verdadeiro-mas

# eliminar ocorrencias com mas
resultado$previo <- gsub("VERDADEIRO, MAS","",resultado$tmp)
resultado$previo <- gsub("VERDADEIRO-MAS","",resultado$previo)

# contar verdadeiro e eliminar coluna temporaria
resultado$VERDADEIRO <- str_count(resultado$previo, "VERDADEIRO")
resultado$previo <- NULL

# exportar

write.csv2(resultado, "agencia_lupa.csv")
