tabla <- base_esa %>% 
  filter(M2.12 <= 2) %>% 
  count(clase_torrado_psh_f, clase_torrado_f, wt = POND2R_FIN_n) %>%
  na.omit() %>%
  pivot_wider(names_from = clase_torrado_f, values_from = n) %>%
  tibble::column_to_rownames("clase_torrado_psh_f") %>% 
  as.matrix() %>%
  DescTools::ExpFreq()

xlsx::write.xlsx(tabla, "tabla.xlsx")
