# script for further variable selection and reduction

rm(list = ls())

library(tidyverse)
library(caret)
library(magrittr)

input_dir <- "./size_and_classif/data_1"
files.v <- dir(path=input_dir, pattern=".*rds")








start_time <- Sys.time()
for(i in seq_along(files.v)) {
  
  start_time2 <- Sys.time()
  
  input.tib <- readRDS(file = file.path(input_dir, files.v[i]))
  
  index <- sample(nrow(input.tib), 1000, replace = FALSE) %>%
    sort()
  
  input.tib <- input.tib[index, ]
  
 
  
  freq.tib <- gather(input.tib[, ], variable_name, variable_value, -token_id, na.rm = TRUE) %>%
    mutate(combined = paste(paste(variable_name, variable_value, sep = "=="))) %>%
    select( combined) %>%
    group_by( combined) %>%
    summarize(n())  %>%
    arrange(desc(`n()`))
  
  if ( i == 1) {
    combined_freq.tib <- freq.tib
    
  } else {
    
    combined_freq.tib <-bind_rows(combined_freq.tib, freq.tib)
    
  }
  
  
 
  end_time2 <- Sys.time()
  print(paste("completed loop ", i))
  print(end_time2 - start_time2)
  
}

end_time <- Sys.time()
end_time - start_time

combined_freq.tib


combined_rel_freq.tib %>%
  head() %>%
  View()

start_time <- Sys.time()
result <- xtabs(`n()` ~ combined, data = combined_freq.tib, sparse = FALSE)  ## works maybe
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
result_2 <- apply(result, 2, as.numeric)
end_time <-Sys.time()
print(end_time - start_time)



result_2 <- as_tibble(result) %>%
  arrange(., desc(n))






col_totals <- result_2 %>%
  colSums()

col_totals %>%
  summary()

culled_columns <- col_totals[which(col_totals >= 17) ]


culled_names <- names(culled_columns)


saveRDS(culled_names, file = "culed_names.rds")

start_time <- Sys.time()
combined_rel_freq.tib %>%
  filter(combined %in% culled_names)
end_time <-Sys.time()
print(end_time - start_time)

