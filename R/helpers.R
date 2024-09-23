scale_zero_to_one <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

read_ai_exposure_file <- function(file) {
  read_csv(
    file
  ) %>%
    mutate(isco_level_2 = substr(isco_group, 1, 2)) %>%
    group_by(isco_level_2) %>%
    summarise(
      ai_product_exposure_score = mean(ai_product_exposure_score),
      felten_exposure_score = mean(felten_exposure_score, na.rm = T),
      webb_exposure_score = mean(webb_exposure_score, na.rm = T),
      beta_eloundou = mean(beta_eloundou, na.rm = T)
    )
}

format_twfe_oja_data <- function(oja, ai_exposure) {
  oja %>%
    select(OJA, dmax, idcountry, esco_level_2_short, idesco_level_2) %>%
    mutate(
      post_chatgpt = ifelse(
        dmax >= as.Date("2022-11-30"),
        1,
        0
      )
    ) %>%
    left_join(
      ai_exposure %>%
        rename(idesco_level_2 = isco_level_2) %>%
        mutate(idesco_level_2 = paste0("OC", idesco_level_2)),
      by = "idesco_level_2"
    ) %>%
    mutate(
      country_occupation_pair = paste0(idcountry, "_", idesco_level_2)
    ) %>%
    filter(
      !is.na(ai_product_exposure_score) # mostly missing idesco_level_2
    ) %>%
    mutate(
      log_OJA = log(OJA + 1), # +1 just in case to handle potenital 0s
      ai_product_exposure_score = scale_zero_to_one(ai_product_exposure_score),
      felten_exposure_score = scale_zero_to_one(felten_exposure_score),
      webb_exposure_score = scale_zero_to_one(webb_exposure_score),
      beta_eloundou = scale_zero_to_one(beta_eloundou)
      #log_ai_product_exposure_score = log(ai_product_exposure_score + 1)
    )
}

derive_sectoral_exposure <- function(ai_exposure, cedefop_sectoral) {
  cedefop_sectoral %>%
    filter(str_detect(occupation_code, "\\.")) %>%
    mutate(
      isco_level_2 = substr(occupation_code, 3, 4)
    ) %>%
    left_join(ai_exposure, by = c("isco_level_2" = "isco_level_2")) %>%
    group_by(country_code, nace_rev2_code, sector_name) %>%
    summarise(
      ai_product_exposure_score = weighted.mean(
        ai_product_exposure_score, n, na.rm = TRUE
      ),
      felten_exposure_score = weighted.mean(
        felten_exposure_score, n, na.rm = TRUE
      ),
      webb_exposure_score = weighted.mean(
        webb_exposure_score, n, na.rm = TRUE
      ),
      beta_eloundou = weighted.mean(
        beta_eloundou, n, na.rm = TRUE
      )
    ) %>%
    ungroup()
}

prep_nama_data <- function(nama_data, sectoral_exposure) {
  nama_data %>%
    select(
      nace_rev2_code = nace_r2,
      country_code = geo,
      value_qualifier,
      year = t,
      value,
    ) %>%
    left_join(
      sectoral_exposure,
      by = c("nace_rev2_code", "country_code")
    ) %>%
    filter(!is.na(ai_product_exposure_score)) %>%
    filter(year >= 2021) %>%
    group_by(country_code, nace_rev2_code) %>%
    mutate(max_year = max(year)) %>%
    filter(max_year == 2023) %>%
    ungroup() %>%
    select(-max_year) %>%
    mutate(
      post_chatgpt = ifelse(year > 2022, 1, 0),
      log_value = log(value) # no +1 needed, as this is a 100-based index
    )
}
