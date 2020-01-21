#Key Cols: ("estu_genero","estu_fechanacimiento","fami_estratovivienda","estu_trabajaactualmente","fami_personashogar","fami_cuartoshogar")
# Value Cols: ("P81_ACT_ART","P112_DEPORTE","P82A_MUSICA","P82B_ESCRIBIR","P82C_BORDAR","P82D_PINTAR","P82E_ESCENOGRAFIA","P82F_TEATRO","P82J_DANZA","P88A_ASIST_TEATRO","P88B_ASIST_EXPOSICION","P88C_ASIST_DANZA","P88D_ASIST_CINE","P88E_CONCIERTO","P99_NRO_LIBROS Leyo","P53C_MUSEOS","P53A_RECORRIDOS","P53B_MONUMENTOS","P53D_FESTIVALES")

merge_by_distance <- function(df_left, df_right, key_cols, value_cols_right) {
  
  # prepare data sets
  
  base_left_adj <- df_left %>%
    select(keys_cols) 
  
  base_right_adj <- df_right %>%
    select(keys_cols) 
  
  # find most similar obs in right to paste in left
  
  whole_df <- df_left 
  
  for (value_col in c(paste(keys_cols, "derecha", sep = "_"), value_cols_right)) {
    whole_df[value_col] <- NA_character_
  }
  
  # set progress bar
  
  pb <- txtProgressBar(min = 0, 
                       max = nrow(whole_df),
                       style = 3)
  
  for (row_i in (1:nrow(whole_df))) {
    
    distance <- gower.dist(data.frame(base_left_adj[row_i, ]),
                           data.frame(base_right_adj))
    
    pos_index <- which.min(distance)
    
    # update df
    
    for (index_col in keys_cols) {
      
      index_col_izq <- paste(index_col, "derecha", sep = "_")
      whole_df[row_i, index_col_izq] <- df_right[pos_index, index_col]
      
    }
    
    for (index_col in value_cols_right) {
      whole_df[row_i, index_col] <- as.character(df_right[pos_index, index_col])
    }
    
    setTxtProgressBar(pb, row_i) 
  }
  
  close(pb)
  return(whole_df)
  
}
##### ejemplo #####

base1 <- data.frame("A" = rnorm(100),
                    "B" = sample(c("a", "b", "c"), size = 100, replace = T),
                    "D" = rnorm(100), stringsAsFactors = FALSE)

base2 <- data.frame("A" = rnorm(100),
                    "B" = sample(c("a", "b", "c"), size = 100, replace = T),
                    "C" = rnorm(100), stringsAsFactors = FALSE)

keys_cols <- c("A", "B")
value_cols <- c("C")
