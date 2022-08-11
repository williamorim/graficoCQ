library(dplyr)
library(ggplot2)
library(echarts4r)


# Importação --------------------------------------------------------------

# dados: https://bdmep.inmet.gov.br/

arquivos <- list.files("dados/", full.names = TRUE)

dados <- purrr::map2_dfr(
  arquivos,
  c("Santana", "Interlagos"),
  ~ readr::read_csv2(.x, skip = 10) |>
    mutate(estacao = .y)
)


# Manip -------------------------------------------------------------------

dados_outono <- dados |>
  janitor::clean_names() |>
  janitor::remove_empty(which = "cols") |>
  rename(
    dia = data_medicao,
    hora = hora_medicao,
    temperatura = temperatura_do_ar_bulbo_seco_horaria_c
  ) |>
  filter(dia >= "2022-03-20", dia <= "2022-06-20") |>
  mutate(
    hora = as.numeric(stringr::str_sub(hora, 1, 2)),
    dia_hora = lubridate::make_datetime(
      year = lubridate::year(dia),
      month = lubridate::month(dia),
      day = lubridate::day(dia),
      hour = hora,
      min = 0,
      sec = 0
    )
  )


# Apenas as séries
dados_outono |>
  ggplot(aes(x = dia_hora, y = temperatura)) +
  geom_line() +
  facet_wrap(vars(estacao))

# Série com limites e média
dados_outono |>
  group_by(estacao) |>
  mutate(
    media = mean(temperatura, na.rm = TRUE),
    sd = sd(temperatura, na.rm = TRUE),
    lim_acao_sup = media + 3 * sd,
    lim_acao_inf = media - 3 * sd
  ) |>
  ggplot(aes(x = dia_hora)) +
  geom_line(aes(y = temperatura)) +
  geom_line(aes(y = media), color = "orange", linetype = 2, size = 1.5) +
  geom_line(aes(y = lim_acao_sup), color = "red", linetype = 2, size = 1.5) +
  geom_line(aes(y = lim_acao_inf), color = "red", linetype = 2, size = 1.5) +
  geom_hline(yintercept = 15, color = "royalblue", size = 1) +
  facet_wrap(vars(estacao))


# Série com as horas com menos de 15 graus marcadas
dados_outono |>
  group_by(estacao) |>
  mutate(
    media = mean(temperatura, na.rm = TRUE),
    sd = sd(temperatura, na.rm = TRUE),
    lim_acao_sup = media + 3 * sd,
    lim_acao_inf = media - 3 * sd,
    flag_derrota = ifelse(hora %in% 7:22 & temperatura < 15, temperatura, NA)
  ) |>
  ggplot(aes(x = dia_hora)) +
  geom_line(aes(y = temperatura)) +
  geom_line(aes(y = media), color = "orange", linetype = 2, size = 1.5) +
  geom_line(aes(y = lim_acao_sup), color = "red", linetype = 2, size = 1.5) +
  geom_line(aes(y = lim_acao_inf), color = "red", linetype = 2, size = 1.5) +
  geom_hline(yintercept = 15, color = "royalblue", size = 1) +
  geom_point(aes(y = flag_derrota), color = "darkgreen") +
  facet_wrap(vars(estacao))



tab_plot <- dados_outono |>
  group_by(estacao) |>
  mutate(
    media = mean(temperatura, na.rm = TRUE),
    sd = sd(temperatura, na.rm = TRUE),
    lim_acao_sup = media + 3 * sd,
    lim_acao_inf = media - 3 * sd,
    flag_derrota = ifelse(hora %in% 7:22 & temperatura < 15, TRUE, FALSE)
  ) |>
  group_by(estacao, dia) |>
  mutate(
    primeira_hora_derrota = first(hora[flag_derrota]),
    flag_derrota_dia = ifelse(
      hora == primeira_hora_derrota,
      temperatura,
      NA
    )
  ) |>
  ungroup()

tab_plot |>
  ggplot(aes(x = dia_hora)) +
  geom_line(aes(y = temperatura)) +
  geom_line(aes(y = media), color = "orange", linetype = 2, size = 1.5) +
  geom_line(aes(y = lim_acao_sup), color = "red", linetype = 2, size = 1.5) +
  geom_line(aes(y = lim_acao_inf), color = "red", linetype = 2, size = 1.5) +
  geom_hline(yintercept = 15, color = "royalblue", size = 1) +
  geom_point(aes(y = flag_derrota_dia), color = "darkgreen") +
  facet_wrap(vars(estacao))


tab_plot |>
  filter(estacao == "Santana") |>
  mutate(meta = 15) |>
  e_chart(x = dia_hora) |>
  e_line(serie = temperatura, symbol = "none") |>
  e_line(serie = media, symbol = "none", lineStyle = list(color = "orange"), itemStyle = list(color = "orange")) |>
  e_line(serie = lim_acao_sup, symbol = "none", lineStyle = list(color = "red")) |>
  e_line(serie = lim_acao_inf, symbol = "none", lineStyle = list(color = "red")) |>
  e_line(serie = meta, symbol = "none", lineStyle = list(color = "royalblue")) |>
  e_scatter(serie = flag_derrota_dia, legend = FALSE, symbolSize = 15, itemStyle = list(color = "darkgreen"))
















