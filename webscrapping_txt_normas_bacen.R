#--------------------------------------------------------------------------#
#
# Nome do Script: webscraping_regulacoes_bacen.R
#
# Objetivo: baixar as normas e seus textos de acordo com parâmetros
#
# Autor: Ricardo Theodoro
# Email: rtheodoro@usp.br
# Data da criação: 2024-05-28
# 
#--------------------------------------------------------------------------#
#
# Notas: 
#   
#--------------------------------------------------------------------------#
options(scipen = 6, digits = 4)
#--------------------------------------------------------------------------#


# Definindo parametros ----------------------------------------------------

ini_date <- "2020-01-01"
end_date <- lubridate::today()
terms <- c("Cooperativas de Crédito", "Cooperativa de Crédito")


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

# Baixar os normativos ----------------------------------------------------
normative_data <- download_normativos(terms, ini_date, end_date)

# Função para baixar textos normativos ------------------------------------------------
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

# Baixar textos normativos ------------------------------------------------
normative_txt <- download_texto_normativo(normative_data)

# Salvar os dados --------------------------------------------------------

readr::write_csv(normative_txt, "normative_txt_2000a202405.csv")
readr::write_csv(normative_data, "normative_data_2000a202405.csv")

