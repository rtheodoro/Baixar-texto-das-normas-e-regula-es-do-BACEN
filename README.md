# Normative Data Analysis

### Descrição

Este repositório contém scripts em R para baixar, processar e analisar dados normativos do site do Banco Central do Brasil. O objetivo é extrair informações relevantes de normativos específicos, como tipos de normativos, números, assuntos e textos associados, para fins de análise e pesquisa.
Estrutura do Repositório

    webscrapping_txt_normas_bacen.R: Script para baixar dados normativos do site do Banco Central do Brasil em formato JSON, processá-los e extrair informações específicas.

### Resultados obtidos

```r
> names(normative_data)
 [1] "title"                   "RefinableString01"       "AssuntoNormativoOWSMTXT" "ResponsavelOWSText"      "listItemId"              "TipodoNormativoOWSCHCS"  "NumeroOWSNMBR"           "RevogadoOWSBOOL"         "HitHighlightedSummary"  
[10] "CanceladoOWSBOOL"        "data"                    "RefinableString03"       "RowNumber"

> names(normative_txt)
 [1] "Titulo"           "Tipo"             "DOU"              "Id"               "Data"             "DataTexto"        "Numero"           "Assunto"          "Revogado"         "Cancelado"        "Texto"            "Voto"            
[13] "Documentos"       "VersaoNormativo"  "NormasVinculadas" "Referencias"      "Atualizacoes"    
```

### Funcionalidades
Análise de Normativos

Os scripts baixam dados normativos em formato JSON do site do Banco Central do Brasil e extraem informações específicas.

### Definindo Parâmetros

Antes de executar as funções para baixar e processar os normativos, é necessário definir os parâmetros de data e termos de busca. Esta seção é essencial para delimitar o período e os termos de interesse nos dados a serem baixados.

```r

# Definindo parâmetros ----------------------------------------------------

ini_date <- "2020-01-01"
end_date <- lubridate::today()
terms <- c("Cooperativas de Crédito", "Cooperativa de Crédito")
```
Função download_normativos

Esta função baixa dados normativos do site do Banco Central do Brasil e os armazena em um data frame.

```r
# Função para baixar arquivos ---------------------------------------------
download_normativos <- function(terms, ini_date, end_date) {
  
  # Juntar os termos com " OR " e substituir espaços por "%20"
  terms_joined <- stringr::str_c(terms, collapse = " OR ") |> URLencode()
  
  # Pegando Qtd de linhas
  site <- glue::glue("https://www.bcb.gov.br/api/search/app/normativos/buscanormativos?querytext=ContentType:normativo%20AND%20contentSource:normativos%20AND%20{terms_joined}&rowlimit=15&startrow=0&sortlist=Data1OWSDATE:descending&refinementfilters=Data:range(datetime({ini_date}),datetime({end_date}))")
  json_file <- httr::GET(site) |> httr::content(as = "text") |> jsonlite::fromJSON()
  total_rows <- json_file$TotalRows
  
  startrow <- 0
  all_data <- data.frame()
  
  repeat {
    site <- glue::glue("https://www.bcb.gov.br/api/search/app/normativos/buscanormativos?querytext=ContentType:normativo%20AND%20contentSource:normativos%20AND%20{terms_joined}&rowlimit={total_rows}&startrow={startrow}&sortlist=Data1OWSDATE:descending&refinementfilters=Data:range(datetime({ini_date}),datetime({end_date}))")
    
    response <-  httr::GET(site) |> httr::content(as = "text") |> jsonlite::fromJSON() |> purrr::pluck("Rows") |>  as.data.frame()
    
    if (length(httr::GET(site) |> httr::content(as = "text") |> jsonlite::fromJSON() |> purrr::pluck("Rows")) < 13) {
      break
    }
    
    response <- response |>
      dplyr::mutate(
        RefinableString01 = stringr::str_replace_all(RefinableString01, "string;#", ""),
        AssuntoNormativoOWSMTXT = stringr::str_replace_all(AssuntoNormativoOWSMTXT, "<[^>]+>", ""),
        RefinableString03 = stringr::str_replace_all(RefinableString01, "string;#", ""),
        HitHighlightedSummary = stringr::str_replace_all(HitHighlightedSummary, "<[^>]+>", ""),
        NumeroOWSNMBR = stringr::str_replace_all(NumeroOWSNMBR, "\\..*$", "")
      )
    
    all_data <- rbind(all_data, response)
    print(startrow)
    startrow <- startrow + 500
  }
  
  return(all_data)
}
```
Função download_texto_normativo

Esta função baixa o conteúdo dos normativos específicos e extrai as informações relevantes, limpando o texto para remover entidades HTML e quebras de linha indesejadas.

```r
download_texto_normativo <- function(normative_data) {
  
  all_data <- data.frame()
  
  # Iterar sobre cada linha do data frame
  for (i in seq_len(nrow(normative_data))) {
    normativo_tipo <- normative_data$TipodoNormativoOWSCHCS[i]
    normativo_num <- normative_data$NumeroOWSNMBR[i]
    print(i)
    # Construir a URL com base no tipo de normativo
    if (normativo_tipo %in% c("Comunicado", "Ato de Diretor", "Ato do Presidente")) {
      normativo_tipo_encoded <- URLencode(normativo_tipo, reserved = TRUE)
      site <- glue::glue("https://www.bcb.gov.br/api/conteudo/app/normativos/exibeoutrasnormas?p1={normativo_tipo_encoded}&p2={normativo_num}")
    } else {
      # Codificar o tipo de normativo para URL
      normativo_tipo_encoded <- URLencode(normativo_tipo, reserved = TRUE)
      site <- glue::glue("https://www.bcb.gov.br/api/conteudo/app/normativos/exibenormativo?p1={normativo_tipo_encoded}&p2={normativo_num}")
    }
    
    # Fazer a requisição e obter o conteúdo JSON
    json_file <- httr::GET(site) |> 
      httr::content(as = "text") |> 
      jsonlite::fromJSON() |> 
      purrr::pluck("conteudo") |> 
      as.data.frame() |> 
      dplyr::mutate(Assunto = xml2::xml_text(xml2::read_html(paste0("<x>", Assunto, "</x>"))),
                    Assunto = stringr::str_replace_all(Assunto, "\n", " "),
                    Assunto = stringr::str_squish(Assunto),
                    #Texto = stringr::str_replace_all(Texto, "<[^>]+>", ""),
                    Texto = xml2::xml_text(xml2::read_html(paste0("<x>", Texto, "</x>"))),
                    Texto = stringr::str_replace_all(Texto, "\n", " "),
                    Texto = stringr::str_squish(Texto))
    
    all_data <- dplyr::bind_rows(all_data, json_file)
   
  }
  
  return(all_data)
}
```
### Limpeza e Formatação de Texto

O texto dos normativos é limpo para remover entidades HTML e quebras de linha indesejadas.
Requisitos

    R
    Pacotes: httr, jsonlite, purrr, dplyr, stringr, xml2, glue

### Instruções de Uso

    Clone o repositório para o seu ambiente local.
    Instale os pacotes necessários.
    Execute o script normativos_analysis.R para baixar, processar e analisar os dados normativos.

### Contribuição

Sinta-se à vontade para contribuir com melhorias e novas funcionalidades. Abra uma issue ou envie um pull request.
Licença

Este projeto está licenciado sob a MIT License.
