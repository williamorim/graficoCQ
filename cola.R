library(dplyr)
library(ggplot2)
library(echarts4r)

# Import ------------------------------------------------------------------

# dados: https://bdmep.inmet.gov.br/

arquivos <- list.files("dados/", full.names = TRUE)

dados <- purrr::map2_dfr(
  arquivos,
  c("Santana", "Interlagos"),
  ~ readr::read_csv2(.x, skip = 10) |>
    mutate(estacao = .y),
) |>
  janitor::clean_names() |>
  janitor::remove_empty(which = "cols") |>
  rename(temp = temperatura_do_ar_bulbo_seco_horaria_c)

dados_outono <- dados |>
  filter(data_medicao >= "2022-03-20", data_medicao <= "2022-06-20") |>
  mutate(
    hora_medicao = as.numeric(stringr::str_sub(hora_medicao, 1, 2)),
    data_hora = lubridate::make_datetime(
      year = lubridate::year(data_medicao),
      month = lubridate::month(data_medicao),
      day = lubridate::day(data_medicao),
      hour = hora_medicao,
      min =  0,
      sec = 0
    )
  )

dados_outono |>
  ggplot(aes(x = data_hora, y = temp)) +
  geom_line() +
  geom_hline(yintercept = 15, color = "green") +
  facet_wrap(vars(estacao))

# Contando todas as falhas
tab_plot <- dados_outono |>
  filter(estacao == "Santana") |>
  mutate(
    media = mean(temp, na.rm = TRUE),
    sd = sd(temp, na.rm = TRUE),
    lim_acao_sup = media + 3 * sd,
    lim_acao_inf = media - 3 * sd,
    falha = case_when(
      hora_medicao %in% 7:22 & temp < 15 ~ temp,
      TRUE ~ NA_real_
    )
  )


tab_plot |>
  ggplot(aes(x = data_hora)) +
  geom_line(aes(y = temp)) +
  geom_line(aes(y = media), color = "yellow", linetype = 2) +
  geom_line(aes(y = lim_acao_sup), color = "red", linetype = 2) +
  geom_line(aes(y = lim_acao_inf), color = "red", linetype = 2) +
  geom_point(aes(y = falha), color = "orange", size = 1.5) +
  geom_hline(yintercept = 15, color = "green")

tab_plot |>
  e_chart(x = data_hora) |>
  e_line(serie = temp, symbol = "none", lineStyle = list(color = "black")) |>
  e_line(serie = media, lineStyle = list(color = "yellow"), symbol = "none") |>
  e_line(serie = lim_acao_sup, lineStyle = list(color = "red"), symbol = "none") |>
  e_line(serie = lim_acao_inf, lineStyle = list(color = "red"), symbol = "none") |>
  e_line(serie = falha, itemStyle = list(color = "orange"), legend = FALSE) |>
  e_tooltip()


# Contando apenas a primeira falha por dia
tab_plot <- dados_outono |>
  filter(estacao == "Santana") |>
  mutate(
    media = mean(temp, na.rm = TRUE),
    sd = sd(temp, na.rm = TRUE),
    lim_acao_sup = media + 3 * sd,
    lim_acao_inf = media - 3 * sd,
    falhou = ifelse(hora_medicao %in% 7:22 & temp < 15, TRUE, FALSE)
  ) |>
  group_by(data_medicao) |>
  mutate(
    hora_primeira_falha = first(hora_medicao[falhou]),
    falha = case_when(
      hora_medicao == hora_primeira_falha & falhou ~ temp,
      TRUE ~ NA_real_
    )
  ) |>
  ungroup()

tab_plot |>
  e_chart(x = data_hora) |>
  e_line(serie = temp, symbol = "none", lineStyle = list(color = "black")) |>
  e_line(serie = media, lineStyle = list(color = "yellow"), symbol = "none") |>
  e_line(serie = lim_acao_sup, lineStyle = list(color = "red"), symbol = "none") |>
  e_line(serie = lim_acao_inf, lineStyle = list(color = "red"), symbol = "none") |>
  e_line(serie = falha, itemStyle = list(color = "orange"), legend = FALSE) |>
  e_tooltip()

