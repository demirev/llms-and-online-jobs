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

derive_sectoral_exposure <- function(
    ai_exposure, cedefop_sectoral, zero_to_one = T
) {
  res <- cedefop_sectoral %>%
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
  
  if (zero_to_one) {
    res <- res %>%
      mutate(
        ai_product_exposure_score = scale_zero_to_one(ai_product_exposure_score),
        felten_exposure_score = scale_zero_to_one(felten_exposure_score),
        webb_exposure_score = scale_zero_to_one(webb_exposure_score),
        beta_eloundou = scale_zero_to_one(beta_eloundou)
      )
  }
  
  res
}

prep_nama_data <- function(nama_data, sectoral_exposure) {
  nama_data %>%
    select(
      nace_rev2_code = nace_r2,
      country_code = geo,
      value_qualifier,
      year = time,
      value = values,
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

run_twfe_exposure_models <- function(exposure_var, data, level) {
  esco_level <- paste0("idesco_level_", level)
  
  # Log-linear model with separate fixed effects
  model <- feols(
    as.formula(paste("log_OJA ~", exposure_var, ": post_chatgpt |", esco_level, "+ idcountry + dmax")),
    data = data,
    cluster = c("idcountry", esco_level)
  )
  
  return(model)
}

run_event_study_model <- function(exposure_var, data, level) {
  esco_level <- paste0("idesco_level_", level)
  
  model_event_study <- feols(
    as.formula(paste("log_OJA ~", exposure_var, ": i(event_time, ref = 0) |", esco_level, "+ idcountry")),
    data = data,
    cluster = c("idcountry", esco_level)
  )
  
  return(model_event_study)
}

extract_event_study_coefs <- function(model, exposure_var) {
  tidy_model <- tidy(model)
  
  # The interaction terms will be of the form exposure_var:event_time::X
  interaction_pattern <- paste0(exposure_var, ":event_time::")
  
  coefs <- tidy_model %>%
    filter(str_detect(term, interaction_pattern)) %>%
    mutate(
      event_time = as.numeric(str_extract(term, "(?<=event_time::)-?\\d+"))
    ) %>%
    arrange(event_time)
  
  return(coefs)
}

plot_event_study <- function(coefs, exposure_var) {
  # Get the name for the exposure variable
  var_name <- names(exposure_vars)[exposure_vars == exposure_var]
  
  ggplot(coefs, aes(x = event_time, y = estimate)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                      ymax = estimate + 1.96 * std.error), width = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(title = paste("Event Study for", var_name),
         x = "Event Time (quarters since ChatGPT release)",
         y = "Coefficient Estimate") +
    theme_minimal() +
    theme(text = element_text(family = "merriweather"))
}


plot_decile_coefficients <- function(model, exposure_var, conf_level = 0.95) {
  # Get the name for the exposure variable
  var_name <- names(exposure_vars)[exposure_vars == exposure_var]
  
  z_score <- qnorm(1 - (1 - conf_level)/2)
  
  coefs <- tidy(model) %>%
    mutate(
      decile = as.numeric(str_extract(term, "\\d+")),
      conf_low = estimate - z_score * std.error,
      conf_high = estimate + z_score * std.error
    )
  
  ggplot(coefs, aes(x = decile, y = estimate)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = paste("Decile Effects for", var_name),
      subtitle = paste0(conf_level * 100, "% Confidence Intervals"),
      x = "Exposure Score Decile",
      y = "Coefficient Estimate"
    ) +
    theme_minimal() +
    theme(text = element_text(family = "merriweather"))
}

# sensitivity analysis function
run_occupation_sensitivity <- function(data, exposure_vars, level = 3) {
  if (!(level %in% c(2, 3))) {
    stop("level must be either 2 or 3")
  }
  
  # Get unique occupations at specified level
  if (level == 2) {
    # Get level 2 occupations by removing last character
    occupations <- unique(substr(data$idesco_level_3, 1, nchar(data$idesco_level_3)-1))
  } else {
    occupations <- unique(data$idesco_level_3)
  }
  
  # For each occupation and exposure measure, run model excluding that occupation
  results <- expand_grid(
    occupation = occupations,
    exposure_var = exposure_vars
  ) %>%
    mutate(
      model = map2(occupation, exposure_var, function(occ, exp_var) {
        # Filter data differently based on level
        if (level == 2) {
          data_filtered <- data %>% 
            filter(substr(idesco_level_3, 1, nchar(occupation)) != occ)
        } else {
          data_filtered <- data %>% 
            filter(idesco_level_3 != occ)
        }
        
        model <- feols(
          as.formula(paste("delta_OJA_log ~", exp_var, " | idcountry")),
          data = data_filtered
        )
        
        # Extract coefficient, SE, and p-value
        coef_data <- tidy(model) %>%
          filter(term == exp_var) %>%
          select(estimate, std.error, p.value)
        
        return(coef_data)
      })
    ) %>%
    unnest(model)
  
  # Add baseline models for comparison
  baseline_results <- map_dfr(exposure_vars, function(exp_var) {
    model <- feols(
      as.formula(paste("delta_OJA_log ~", exp_var, " | idcountry")),
      data = data
    )
    
    tidy(model) %>%
      filter(term == exp_var) %>%
      select(estimate, std.error, p.value) %>%
      mutate(
        exposure_var = exp_var,
        occupation = "Baseline (All)"
      )
  })
  
  # Combine and format results
  results_table <- bind_rows(
    baseline_results,
    results
  ) %>%
    mutate(
      conf_low = estimate - 1.96 * std.error,
      conf_high = estimate + 1.96 * std.error
    ) %>%
    arrange(exposure_var, occupation != "Baseline (All)", abs(estimate))
  
  return(results_table)
}

run_country_sensitivity <- function(data, exposure_vars) {
  
  # Get unique countries at specified level
  countries <- unique(data$idcountry)
  
  # For each occupation and exposure measure, run model excluding that occupation
  results <- expand_grid(
    country = countries,
    exposure_var = exposure_vars
  ) %>%
    mutate(
      model = map2(country, exposure_var, function(cnt, exp_var) {
        # Filter data differently based on level
        data_filtered <- data %>% 
          filter(idcountry != cnt)
        
        model <- feols(
          as.formula(paste("delta_OJA_log ~", exp_var, " | idcountry")),
          data = data_filtered
        )
        
        # Extract coefficient, SE, and p-value
        coef_data <- tidy(model) %>%
          filter(term == exp_var) %>%
          select(estimate, std.error, p.value)
        
        return(coef_data)
      })
    ) %>%
    unnest(model)
  
  # Add baseline models for comparison
  baseline_results <- map_dfr(exposure_vars, function(exp_var) {
    model <- feols(
      as.formula(paste("delta_OJA_log ~", exp_var, " | idcountry")),
      data = data
    )
    
    tidy(model) %>%
      filter(term == exp_var) %>%
      select(estimate, std.error, p.value) %>%
      mutate(
        exposure_var = exp_var,
        country = "Baseline (All)"
      )
  })
  
  # Combine and format results
  results_table <- bind_rows(
    baseline_results,
    results
  ) %>%
    mutate(
      conf_low = estimate - 1.96 * std.error,
      conf_high = estimate + 1.96 * std.error
    ) %>%
    arrange(exposure_var, country != "Baseline (All)", abs(estimate))
  
  return(results_table)
}
