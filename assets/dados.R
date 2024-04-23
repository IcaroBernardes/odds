library(worldfootballR)


## Obtém dados de todas as partidas desde 2014
partidas <- worldfootballR::fb_match_results(
  country = "BRA", gender = "M", season_end_year = 2014:2024
) |> 
  janitor::clean_names() |> 
  dplyr::tibble()

resultados <- partidas |> 
  ## calculando quantos pontos cada time fez em cada jogo
  mutate(
    home_pont = case_when(
      home_goals > away_goals ~ 3,
      home_goals == away_goals ~ 1,
      TRUE ~ 0
    ),
    away_pont = case_when(
      home_goals < away_goals ~ 3,
      home_goals == away_goals ~ 1,
      TRUE ~ 0
    )
  ) |> 
  # Calculando o aproveitamento
  mutate(wk = as.numeric(wk),
         pont_disp = 3*wk) |> 
  
  group_by(season_end_year) |> 
  arrange(date) |> 
  dplyr::ungroup() |> 
  
  mutate(
    pont_game_home = cumsum(home_pont),
    .by = c(season_end_year, home)
  ) |> 
  mutate(
    pont_game_away = cumsum(away_pont),
    .by = c(season_end_year, away)
  ) |> 
  
  # 3. Calculamos a pontuação acumulada
  mutate(pont_earn_home = pont_disp/pont_game_home,
         pont_earn_away = pont_disp/pont_game_home,
         sit_home = recode(home_pont, 
                           `1`="draw",
                           `3`="win",
                           `0`="lose"),
         sit_away = recode(away_pont, 
                           `1`="draw",
                           `3`="win",
                           `0`="lose")) |> 
  mutate(across(
    .cols = everything(),
    .fns = \(x) ifelse(is.infinite(x), 0, x)
  ))
  
## Salva os resultados
saveRDS(resultados, "assets/resultados.RDS")
  