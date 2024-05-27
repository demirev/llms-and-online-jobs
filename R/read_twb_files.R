library(tidyverse)
library(xml2)

file_name <- "data/cedefop_skills_ovate/01.COUNTRIES AND OCCUPATIONS_2021.Q4.twb"

data_src_table <- file_name %>%
  read_xml() %>%
  xml_find_all("//relation[boolean(@table)]") %>%
  xml_attrs() %>%
  bind_rows()

excel_paths <- bind_cols(
  file_name %>%
    read_xml() %>%
    xml_find_all(
      "//named-connection[contains(@name, 'excel-direct')]"
    ) %>%
    xml_attrs() %>%
    bind_rows(),
  
  file_name %>%
    read_xml() %>%
    xml_find_all(
      "//named-connection[contains(@name, 'excel-direct')]//connection"
    ) %>%
    xml_attrs() %>%
    do.call(bind_rows, .)  
)

csv_paths <- bind_cols(
  file_name %>%
    read_xml() %>%
    xml_find_all(
      "//connection[contains(@class, 'textscan')]"
    ) %>%
    xml_attrs() %>%
    bind_rows(),
  
  file_name %>%
    read_xml() %>%
    xml_find_all(
      "//connection[contains(@class, 'textscan')]//connection"
    ) %>%
    xml_attrs() %>%
    do.call(bind_rows, .)  
)
