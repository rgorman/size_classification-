input_dir <- "./size_and_classif/data_1"
files.v <- dir(path=input_dir, pattern=".*rds")

saveRDS(culled_cols, file = "./size_and_classif/src/2018-6-8_culled_var_vals.RDS")



culled_cols <- readRDS(file = "./size_and_classif/src/2018-6-8_culled_var_vals.RDS")


nom.v <- append("token_id", culled_cols)


start_time_1 <- Sys.time()
for (i in 36:45) { # reset to seq_along(files.v) when appropriate!!
  
  
  start_time_2 <- Sys.time()
  
  input.tib <- readRDS(file = file.path(input_dir, files.v[i]))
  
  
  
  
  
  
  
  freq.tib <- gather(input.tib[, ], variable_name, variable_value, -token_id, na.rm = TRUE) %>%
    mutate(combined = paste(paste(variable_name, variable_value, sep = "==")))
  
  culled.tib <- freq.tib %>%
    filter(combined %in% culled_cols)
  
  
  
  
  
  z.holder.m <- matrix(nrow = 1, ncol = length(nom.v))
  colnames(z.holder.m) <- nom.v
  
  z.holder.tib <- as_tibble(z.holder.m)
  
  
  
  start_time_3 <- Sys.time()
  for (j in 1:nrow(input.tib)) { 
    
    
    
    x <- filter(culled.tib, token_id==input.tib$token_id[j])
    z <- matrix(nrow = 1, ncol = nrow(x)+1)
    names_for_cols.v <- paste(x$combined)
    colnames(z) <- append("token_id", names_for_cols.v)
    z.tib <- as_tibble(z)
    z.tib[1, 1] <- input.tib[j, 1]
    z.tib[, 2:ncol(z.tib)] <- 1
    
    t_value <- colnames(z.tib[2:ncol(z.tib)]) %in% culled_cols
    
    if (TRUE %in% (colnames(z.tib[2:ncol(z.tib)]) %in% culled_cols) ) { # Condiiton to deal with tokens without a single selected variable
      
      z.holder.tib <- bind_rows(z.holder.tib, z.tib)
      
      
    } else {
      no.content.tib <- tibble(token_id = unlist(input.tib[j, 1]))
      z.holder.tib <- bind_rows(z.holder.tib, no.content.tib)
      
    }
    
    
    
   
    if (j %% 500 == 0) {
      
      end_time_3 <- Sys.time()
      print(paste("file", files.v[i], "token", j, "has been processed. Elapsed time = ", end_time_3 - start_time_3 ))
    }
    
    
  }
  
  end_time_3 <- Sys.time()
  print(end_time_3 - start_time_3)
  
  
  z.holder.tib <- z.holder.tib[-1, ]
  
  z.holder.tib <-  z.holder.tib %>% 
    replace(., is.na(.), as.numeric(0)) #this one!
  
  file_name <- gsub(".rds", "", files.v[i])
  
  
  saveRDS(z.holder.tib, file = paste0("./size_and_classif/data_2/", file_name, ".RDS"))
  
  rm(z.holder.tib)
  
  end_time_2 <- Sys.time()
  
  print(paste("File ", files.v[i], "has been processed.  Elapsed time = ", end_time_2-start_time_2))
  
}

end_time_1 <- Sys.time()

print(paste("total elapsed time = ", end_time_1 - start_time_1))




#############

input.tib[j-1, ]

test.tib <- gather(input.tib[j, ], variable_name, variable_value, -token_id, na.rm = TRUE) %>%
  mutate(combined = paste(paste(variable_name, variable_value, sep = "==")))

culled.test.tib <- test.tib %>%
  filter(combined %in% culled_cols)

colnames(x)
ncol(x)


if (TRUE %in% colnames(x) %in% culled_cols) {
  
  z.holder.tib <- bind_rows(z.holder.tib, z.tib)
  
  
} else {
  no.content.tib <- tibble(token_id = unlist(input.tib[j, 1]))
  z.holder.tib <- bind_rows(z.holder.tib, no.content.tib)
  
}


z.test.tib <- tibble(token_id = unlist(input.tib[j, 1]))

colnames(z.tib) %in% culled_cols

j <- 5257

TRUE %in% colnames(x) %in% culled_cols

which(z.holder.tib[1, ] == NA)

test.tib <- readRDS(file = "./size_and_classif/data_2/Aeschylus_Ch.rds.RDS")

ppp <- z.holder.tib[, 2:899] %>%
  colSums() %>%
  unlist()

sum(ppp)

ppp <- bind_rows(z.holder.tib, z.test.tib)
rm(z.test.tib)

j <- 1


TRUE %in% (colnames(z.tib[2:ncol(z.tib)]) %in% culled_cols)
TRUE %in% t_value


input.tib
part_1.tib <- input.tib[1:10000, ]
part_2.tib <- input.tib[10001:20000, ]
part_3.tib <- input.tib[20001:30000, ]
part_4.tib <- input.tib[30001:40000, ]
part_5.tib <- input.tib[40001:50000, ]
part_6.tib <- input.tib[50001:60000, ]
part_7.tib <- input.tib[60001:70000, ]
part_8.tib <- input.tib[70001:80000, ]
part_9.tib <- input.tib[80001:90000, ]
part_10.tib <- input.tib[90001:100000, ]
part_11.tib <- input.tib[100000:nrow(input.tib), ]



tib.list <- list()

tib.list[[11]] <- part_11.tib

part.names.v <- c("part_1.tib", "part_2.tib", "part_3.tib", "part_4.tib", "part_5.tib", "part_6.tib", "part_7.tib", "part_8.tib", "part_9.tib", "part_10.tib",
                  "part_11.tib")


for (i in 1:length(tib.list)) {
  
 destination <-  paste0("./size_and_classif/data_1/HomerOdy_Odyssey_part-", i, ".RDS")
 
 saveRDS(tib.list[[i]], file = destination)
 
  
}


part_12.tib <- input.tib[1:10000, ]
part_13.tib <- input.tib[1:10000, ]

part_1.tib <- input.tib[1:10000, ]


freq.tib

test.tib <- readRDS(file = paste0("./size_and_classif/data_2/HomerOdy_Odyssey_part-11.RDS"))
test.tib[1, 2:ncol(test.tib)] %>%
  sum()

var_sums <- apply(test.tib[, 2:ncol(test.tib)], 1, sum)

summary(var_sums)

sum(var_sums) / 8551


################# consolidate ody files

input_dir <- "./size_and_classif/data_3"
files.v <- dir(path=input_dir, pattern=".*RDS")
target.m <- matrix(nrow = 105602, ncol = 899)
colnames(target.m) <- nom.v
target.tib <- as_tibble(target.m)

start_ind.v <- seq(1, 100000, by = 10000)
end_ind.v <- seq(10000, 100000, by = 10000)

rm(target.m)

for (i in 1:10) {
  
  ody.tib <- readRDS(file = file.path(input_dir, files.v[i]))
  target.tib[start_ind.v[i]:end_ind.v[i], ] <- ody.tib
  
}


files.v

files.v <- append(files.v, files.v[3])
files.v <- files.v[-c(2, 3)]

i <- 1

target.tib[89999:90005, 1:5]

target.tib <- target.tib[-100000, ]

target.tib[100001:105602, ] <- test.tib[1:5602, ]

test.tib[2, 1]


saveRDS(target.tib, file = "./size_and_classif/data_2/HomerOdy_Odyssey_full.RDS")
