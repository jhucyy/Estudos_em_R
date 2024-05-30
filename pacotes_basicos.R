
# Pacote Tidy -------------------------------------------------------------
library(tidyverse)


# separate() --------------------------------------------------------------


imdb <- read_rds("./imdb.rds")

imdb |> separate(generos, into = c("genero_1", "genero_2", "genero_3"),
                 sep = ",") |> view()



imdb <- imdb |> 
  separate(elenco, into = c("elenco1", "elenco2", "elenco3"),
           sep = ",")

# essa função separa elementos de uma coluna em várias colunas, conforme definido.



# unite() -----------------------------------------------------------------

 imdb |> 
  unite(col = "atuacao_principal", starts_with("elenco"), sep = "-")



# essa função faz o contrário da separate, ela pega várias colunas e transforma
# em uma.



# pivotagem ---------------------------------------------------------------

# pivot_longer()
# pivot_wider()

imdb |> 
  pivot_longer(cols = starts_with("atuacao"),
               names_to = "protagonismo", values_to = "ator_atriz") |> 
  select(titulo, ator_atriz, protagonismo) |> 
  pivot_wider(names_from = "protagonismo", values_from = "ator_atriz") 


# pivot_longer deixa o data set mais longo, ou seja, tranforma colunas em
# linhas.

# pivot_wider transforma linhas em colunas.



# outras operações --------------------------------------------------------

tab_romance_terror <- imdb %>%
  filter(ano >= 2010) %>%
  mutate(
    genero = case_when(
      stringr::str_detect(generos, "Romance") ~ "Romance",
      stringr::str_detect(generos, "Horror") ~ "Horror",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(genero)) %>%
  group_by(ano, genero) %>%
  summarise(receita_media = mean(receita, na.rm = TRUE))



tab_romance_terror |>  pivot_wider(names_from = "ano",
                                   values_from = "receita_media") |> 
  view()




# Exercícios --------------------------------------------------------------

#  Crie 5 novas colunas de idiomas na base imdb, cada uma com um dos 
# idiomas contidos na coluna idioma. Para os filmes com menos de 5 idiomas, 
# substitua os valores NA pela string “Inexistente”.













