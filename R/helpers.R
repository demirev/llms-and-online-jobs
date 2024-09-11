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