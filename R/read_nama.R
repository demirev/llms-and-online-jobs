library(tidyverse)

# initial data ----------------------------------------------------------------=
nama_10_lp <- read_delim(
  "data/eurostat_nama_10_prod/estat_nama_10_lp_a21.tsv", delim = "\t"
)
nama_10_lp_ulc <- read_delim(
  "data/eurostat_nama_10_prod/estat_nama_10_lp_ulc.tsv", delim = "\t"
)
nama_10_cp <- read_delim(
  "data/eurostat_nama_10_prod/estat_nama_10_cp_a21.tsv", delim = "\t"
)
nama_10_a10 <- read_delim(
  "data/eurostat_nama_10_prod/estat_nama_10_a10.tsv", delim = "\t"
)

# tidu up data ----------------------------------------------------------------
tidy_up_data <- function(df) {
  first_col_name <- names(df)[1] 
  num_separators <- str_count(first_col_name, ",")
  new_col_names <- str_split(first_col_name %>% str_remove("\\\\.*"), ",")[[1]]
  
  df %>%
    pivot_longer(
      cols = -1,
      names_to = "t",
      values_to = "value"
    ) %>%
    separate(
      col = first_col_name, 
      into = new_col_names,
      sep = ","
    ) 
}

nama_10_lp_tidy <- tidy_up_data(nama_10_lp)
nama_10_lp_ulc_tidy <- tidy_up_data(nama_10_lp_ulc) # NB no nace_r2
nama_10_cp_tidy <- tidy_up_data(nama_10_cp)
# NB asset 10 takes values Asset types N11N (net fixed assets), N11KN (net construction), N11MN (net machinery and equipment and weapon systems), N115N (net cultivated biological resources) and N117N (net intellectual property products) as well as N1132N (net ICT equipment).
nama_10_a10_tidy <- tidy_up_data(nama_10_a10)

nama_10_lp_rlrphw <- nama_10_lp_tidy %>%
  filter(na_item == "RLPR_HW" & unit == "I15") %>%
  mutate(t = as.numeric(trimws(t))) %>%
  mutate(
    # check if value ends in u or p
    value_qualifier = case_when(
      str_detect(value, "u$") ~ "unrevised",
      str_detect(value, "p$") ~ "provisional",
      TRUE ~ "final"
    ),
    value = value %>% str_remove("u|p|\\:") %>% trimws() %>% as.numeric()
  ) %>%
  filter(!is.na(value))

nama_10_cp_gvancs <- nama_10_cp_tidy %>%
  filter(na_item == "GVA_NCS" & unit == "I15") %>%
  mutate(t = as.numeric(trimws(t))) %>%
  mutate(
    # check if value ends in u or p
    value_qualifier = case_when(
      str_detect(value, "u$") ~ "unrevised",
      str_detect(value, "p$") ~ "provisional",
      TRUE ~ "final"
    ),
    value = value %>% str_remove("u|p|\\:") %>% trimws() %>% as.numeric()
  ) %>%
  filter(!is.na(value))

# dictionaries ----------------------------------------------------------------
asset10_dictionary <- c(
  "N11N" = "net fixed assets",
  "N11KN" = "net construction",
  "N11MN" = "net machinery and equipment and weapon systems",
  "N115N" = "net cultivated biological resources",
  "N117N" = "net intellectual property products",
  "N1132N" = "net ICT equipment"
)

na_item_dictionary <- c(
  "D1_SAL_HW" = "Compensation per employee per hour worked",
  "D1_SAL_PER" = "Compensation per employee",
  "EMP_HAB" = "Employment per head of population", #?
  "HW_EMP" = "Hours worked per employed person",
  "HW_HAB" = "Hours worked per head of population", #?
  "NLPR_HW" = "Nominal labour productivity per hour worked",
  "NLPR_PER" = "Nominal labour productivity per person employed",
  "NULC_HW" = "Nominal unit labour cost based on hour worked",
  "NULC_PER" = "Nominal unit labour cost based on person employed",
  "RLPR_HW" = "Real labour productivity per hour worked", # main interest from nama_10_lp_tidy
  "RLPR_PER" = "Real labour productivity per person employed",
  "GVA_NCS" = "Gross value added per unit of net fixed assets", # main interest from nama_10_cp_tidy
  "NCS_EMP" = "Net fixed assets per employed person",
  "NCS_GVA" = "Net fixed assets per unit of gross value added",
  "NCS_HW" = "Net fixed assets per hour worked"
)

table(nama_10_lp_tidy$unit, nama_10_lp_tidy$na_item)
  #                       D1_SAL_HW HW_EMP NULC_HW RLPR_HW RLPR_PER
  # EUR                      35280      0       0       0        0
  # HW                           0  35966       0       0        0
  # I15                          0  35035   34398   34594    36309
  # NAC                      35280      0       0       0        0
  # PC_EU27_2020_MEUR_CP     35280      0       0       0        0
  # PCH_10Y                      0      0   34398   34594    35868
  # PCH_3Y                       0      0   34398   34594    36309
  # PCH_5Y                       0      0   34398   34594    36309
  # PCH_PRE                      0  35966   34839   35035    36309

table(nama_10_lp_ulc_tidy$unit, nama_10_lp_ulc_tidy$na_item)
  #                     D1_SAL_HW D1_SAL_PER EMP_HAB HW_EMP HW_HAB NLPR_HW NLPR_PER NULC_HW NULC_PER RLPR_HW RLPR_PER
  # EUR                       1911       1960       0      0      0       0        0       0        0       0        0
  # HW                           0          0       0   1911   1862       0        0       0        0       0        0
  # I10                          0          0       0      0      0       0        0    1862     1911    1862     1911
  # I15                          0          0    1960   1862   1862       0        0    1862     1960    1862     1960
  # NAC                       1911       1960       0      0      0       0        0       0        0       0        0
  # PC                           0          0    1960      0      0       0        0       0        0       0        0
  # PC_EU27_2020                 0          0    1960   1911   1862       0        0       0        0       0        0
  # PC_EU27_2020_MEUR_CP      1911       1960       0      0      0       0        0       0        0       0        0
  # PC_EU27_2020_MPPS_CP      1764       1813       0      0      0    1764     1813       0        0       0        0
  # PCH_10Y                      0          0       0      0      0       0        0    1862     1862    1862     1911
  # PCH_3Y                       0          0       0      0      0       0        0    1862     1911    1862     1960
  # PCH_5Y                       0          0       0      0      0       0        0    1862     1911    1862     1960
  # PCH_PRE                      0          0    1960   1911   1862       0        0    1911     1960    1911     1960
  # PPS_EU27_2020             1862       1911       0      0      0       0        0       0        0       0        0

table(nama_10_cp_tidy$unit, nama_10_cp_tidy$na_item)
  #         GVA_NCS NCS_EMP NCS_GVA NCS_HW
  # I15       18865   25676   25676  25676
  # PCH_10Y   18865   25676   25676  25676
  # PCH_3Y    18865   25676   25676  25676
  # PCH_5Y    18865   25676   25676  25676
  # PCH_PRE   18865   25676   25676  25676

max(nama_10_lp_tidy$t)  # 2023
max(nama_10_cp_tidy$t)  # 2023

# plots ------------------------------------------------------------------------
nama_10_lp_rlrphw %>%
  filter(t >= 2015) %>%
  ggplot(aes(x = t, y = value, color = geo, linetype = nace_r2)) +
  geom_line() +
  facet_wrap(~ nace_r2, scales = "free_y", ncol = 2) +
  labs(title = "Labor Productivity Over Time by Country and Industry",
       x = "Year",
       y = "Labor Productivity",
       color = "Country",
       linetype = "Industry") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  # hide legend
  theme(legend.position = "none") +
  scale_x_continuous(
    breaks = seq(min(nama_10_lp_rlrphw$t), max(nama_10_lp_rlrphw$t), by = 5)
  )

# save data -------------------------------------------------------------------
write_csv(nama_10_lp_rlrphw, "results/intermediate_datasets/nama_10_lp_rlrphw.csv")
write_csv(nama_10_cp_gvancs, "results/intermediate_datasets/nama_10_cp_gvancs.csv")
