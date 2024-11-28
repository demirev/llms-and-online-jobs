scale_zero_to_one <- function(x) {
	if (all(is.na(x) | is.nan(x))) {
		return(x)  # Return the original vector if all values are NA or NaN
	}
	x_clean <- x[!is.na(x) & !is.nan(x)]  # Remove NA and NaN values
	if (length(x_clean) == 0 || min(x_clean) == max(x_clean)) {
		return(x)  # Return original vector if all values are the same or no valid values
	}
	result <- (x - min(x_clean, na.rm = TRUE)) / (max(x_clean, na.rm = TRUE) - min(x_clean, na.rm = TRUE))
	result[is.na(x) | is.nan(x)] <- NA  # Preserve original NA and NaN values
	return(result)
}

read_ai_exposure_file <- function(file, level = 2) {
  res <- read_csv(file) %>%
    mutate(isco_level = substr(isco_group, 1, level)) %>%
    group_by(isco_level) %>%
    summarise(
      ai_product_exposure_score = mean(ai_product_exposure_score, na.rm = TRUE),
      felten_exposure_score = mean(felten_exposure_score, na.rm = TRUE),
      webb_exposure_score = mean(webb_exposure_score, na.rm = TRUE),
      beta_eloundou = mean(beta_eloundou, na.rm = TRUE)
    )
  
  colnames(res)[1] <- paste0("isco_level_", level)
  res
}

format_twfe_oja_data <- function(
  oja, ai_exposure, level = 2, t0 = as.Date("2022-11-30")
) {
	oja %>%
		select(
		  OJA, dmax, idcountry, 
			matches(paste0("esco_level_", level, "_short")), 
			matches(paste0("idesco_level_", level))
		) %>%
		mutate(
			post_chatgpt = ifelse(
				dmax >= as.Date("2022-11-30"),
				1,
				0
			)
		) %>%
		left_join(
			ai_exposure %>%
				rename_with(~ paste0("idesco_level_", level), matches(paste0("isco_level_", level))) %>%
				mutate(across(matches(paste0("idesco_level_", level)), ~ paste0("OC", .))),
			by = paste0("idesco_level_", level)
		) %>%
		mutate(
			country_occupation_pair = paste0(idcountry, "_", get(paste0("idesco_level_", level)))
		) %>%
		filter(
			!is.na(ai_product_exposure_score) # mostly missing idesco_level_2
		) %>%
		mutate(
			log_OJA = log(OJA + 1), # +1 just in case to handle potential 0s
			ai_product_exposure_score = scale_zero_to_one(ai_product_exposure_score),
			felten_exposure_score = scale_zero_to_one(felten_exposure_score),
			webb_exposure_score = scale_zero_to_one(webb_exposure_score),
			beta_eloundou = scale_zero_to_one(beta_eloundou)
		) %>%
		select(
		  OJA, log_OJA, dmax, post_chatgpt, idcountry, 
			matches(paste0("esco_level_", level, "_short")), 
			matches(paste0("idesco_level_", level)),
			country_occupation_pair,
			ai_product_exposure_score, felten_exposure_score, 
			webb_exposure_score, beta_eloundou
		) %>%
    mutate(
      dmax = as.Date(dmax),
      event_time = as.integer((year(dmax) - year(t0)) * 4 + (quarter(dmax) - quarter(t0)))
    )
}

format_delta_data <- function(
  data, n_periods = -1, base_date = "2022-11-30", level = 3, across_countries = TRUE
) {
 periods <- if (n_periods == -1) {
   c(min(data$dmax), max(data$dmax))
 } else if (is.infinite(n_periods)) {
   unique(data$dmax)
 } else {
   t0 <- as.Date(base_date) 
   all_periods <- sort(unique(data$dmax))
   t0_idx <- which.min(abs(all_periods - t0))
   if (all_periods[t0_idx] < t0) {
     period_range <- (t0_idx - n_periods - 1):(t0_idx + n_periods) # to_idx is post, add 1 period to pre to be equal
   } else {
     period_range <- (t0_idx - n_periods):(t0_idx + n_periods + 1) # to_idx is pre, add 1 period to post to be equal
   }
   periods <- all_periods[period_range[period_range > 0 & period_range <= length(all_periods)]]
 }
 
 # Define grouping variables based on level and across_countries
 esco_short_col <- paste0("esco_level_", level, "_short")
 idesco_col <- paste0("idesco_level_", level)
 group_cols <- c(esco_short_col, idesco_col)
 if (!across_countries) {
   group_cols <- c("idcountry", group_cols)
 }
 
 data %>%
   filter(dmax %in% periods) %>%
   group_by(
     post_chatgpt,
     across(all_of(group_cols)),
     dmax
   ) %>%
   summarize(
     OJA = sum(OJA),
     ai_product_exposure_score = mean(ai_product_exposure_score),
     felten_exposure_score = mean(felten_exposure_score),
     webb_exposure_score = mean(webb_exposure_score),
     beta_eloundou = mean(beta_eloundou),
     .groups = "drop"
   ) %>%
   group_by(
     post_chatgpt, 
     across(all_of(group_cols))
   ) %>%
   summarise(
     OJA = mean(OJA),
     ai_product_exposure_score = mean(ai_product_exposure_score),
     felten_exposure_score = mean(felten_exposure_score),
     webb_exposure_score = mean(webb_exposure_score),
     beta_eloundou = mean(beta_eloundou),
     .groups = "drop"
   ) %>%
   arrange(across(all_of(group_cols)), post_chatgpt) %>%
   group_by(across(all_of(group_cols))) %>%
   filter(n() == 2) %>%
   summarise(
    pre_OJA = OJA[1],
    post_OJA = OJA[2],
    delta_OJA = post_OJA - pre_OJA,
    delta_OJA_relative = delta_OJA / pre_OJA,
    delta_OJA_log = log(post_OJA / pre_OJA),
    ai_product_exposure_score = mean(ai_product_exposure_score),
    felten_exposure_score = mean(felten_exposure_score),
    webb_exposure_score = mean(webb_exposure_score),
    beta_eloundou = mean(beta_eloundou),
    .groups = "drop"
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


read_skill_mentions <- function(dir) {
  list.files(
    dir, 
    full.names = TRUE
  ) %>%
    map(function(file_name) {
      date <- str_extract(file_name, "\\d{4}_q\\d{1}") %>%
        str_replace("_q1", "-03-31") %>%
        str_replace("_q2", "-06-30") %>%
        str_replace("_q3", "-09-30") %>%
        str_replace("_q4", "-12-31") %>%
        as.Date()
      data <- read_csv(file_name)
      data <- data %>%
        mutate(dmax = date, dmin = date - months(12))
      data
    }) %>%
    bind_rows() %>%
    mutate(
      idcountry = ifelse(
        is.na(idcountry),
        countryset,
        idcountry
      )
    ) %>%
    select(-countryset) %>%
    mutate(
      isco_level_2 = substr(idesco_level_3, 3, 4),
      isco_level_3 = substr(idesco_level_3, 3, 5),
    ) %>%
    filter(esco_hier_level_0 == "skills") %>% # maybe also look at transversal skills and competences; knowledge 
    select(
      idcountry,
      isco_level_2,
      isco_level_3,
      esco_skill_level_2 = esco_hier_level_2,
      esco_skill_level_3 = esco_hier_level_3,
      dmin,
      dmax,
      mentions = Mention
    )
}
