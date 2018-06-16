x <- 1:38

choose(38, x) %>%
  sum()


2^38


separate_vars.l <- readRDS(file = "./size_and_classif/results/2018-6-5_list_of_separate_vars.RDS")
separate_vars.tib <- readRDS(file = "./size_and_classif/results/2018-6-5_tib_of_combined_vars.RDS")

saveRDS(holder.list, file = "./size_and_classif/2018-6-5_list_of_separate_vars.RDS")
saveRDS(holder.tib, file = "./size_and_classif/2018-6-5_tib_of_combined_vars.RDS")

separate_vars.l[[2]]

atomic_vars <- readRDS(file = "./size_and_classif/src/2018-6-6_reduced_atomic_vars_list.RDS")


atomic_vars


most_frequent.list <- atomic_vars



input.dir <- "./output_2"

files.v <- dir(path=input.dir, pattern=".*xml")


var_names.v <- most_frequent.list %>%
  unlist() %>%
  unique()


generate_vars(selected_vars.list[[2]] [1], base.df)


selected_vars.list[[2]] [1] %>%
  paste(sep = "&")


generate_var_names(selected_vars.list[[2]])


first_name_holder.v <- NULL

seq_along(selected_vars.list)

for (i in seq_along(selected_vars.list)) {
  
  first_name_holder.v <- append(first_name_hoder.v, selected_vars.list[[i]]) %>%
    unlist()
  
}

first_name_holder.v <- append(first_name_hoder.v, selected_vars.list[[1]]) %>%
  unlist()



more_names.v <- sapply(selected_vars.list[[3]], generate_var_names)

first_name_hoder.v <- append(first_name_hoder.v, more_names.v)


generate_var_names(selected_vars.list[[2]])


for (k in 1:3) { 
  selected_vars.list[[k]] <- combn(var_names.v, k, simplify = FALSE) # make all possible combinations of variables
  nomina.v <- paste(length(var_names.v), "Choose",  k, collapse = " ") %>% # create names for elements in list
    append(nomina.v, .)
}


for (m in seq_along(selected_vars.list)) {
  
  start_time_2 <- Sys.time()
  
  # a <- sapply(selected_vars.list[[m]], generate_vars, df = base.df)
  
  if (m == 1) {
  
    nomina <- sapply(selected_vars.list[[m]], generate_var_names)  
    
  } else {
    
    x <- sapply(selected_vars.list[[m]], generate_var_names)
    nomina <- append(nomina, x)
    
  }
  
  
  
  #colnames(a) <- nomina
  
  #new_vars.m <- cbind(new_vars.m, a)
  
  #end_time <- Sys.time()
  
  #list_of_variable_tuples[[m]] <- rbind(list_of_variable_tuples[[m]], a)
  
  #end_time_2 <- Sys.time()
  
  # print(paste("variables for tuple of length", m, "time elapesed = ",   end_time_2 - start_time_2 ))
  
}


selected_vars.list[[2]]

lengths(selected_vars.list) %>%
  sum()



str_split(first_name_hoder.v[[42]], "_&_")




first_name_hoder.v[1:100]

already_used.v <- nomina

vars_to_keep.v <- NULL

for (i in seq_along(nomina) ) {
  
  if (nomina[i] %in% already_used.v) {
    
  } else {
    
    vars_to_keep.v <- append(vars_to_keep.v, nomina[i])
  }
  
}



names(selected_vars.list[[1]])


y <- sapply(vars_to_keep.v, str_split, "_&_")



y[250]

saveRDS(y, file = "./size_and_classif/src/bigram_var_names.RDS")


#######################################
#####################################

rm(list = ls())


require(XML)
require(tidyverse)
require(stringr)

generate_vars <- function(var.list, df) {
  a <- unlist(var.list)
  b <- df[, a]
  if (length(a) > 1) {
    c <- apply(b, 1, paste0, collapse = "/")
  } else {
    c <- b 
  }
  return(c)
}


generate_var_names <- function(var.list) {
  a <- unlist(var.list)
  b <- paste0(a, collapse = "_&_")
  return(b)
}


input.dir <- "./base_ngram_files"

files.v <- dir(path=input.dir, pattern=".*xml")

var_names.v <- readRDS(file = "./size_and_classif/src/bigram_var_names.RDS")

var_names.v <- var_names.v   %>% 
  unlist() %>%
  unique()

holder.tib <- tibble(variable_name="blank")
holder.list <- vector(mode = "list", length = 5)




for(i in seq_along(files.v)) {
  
  start_time_1 <- Sys.time()
  
  # read xml structure from file to .R object
  doc.object <- xmlTreeParse(file.path(input.dir, files.v[i]), useInternalNodes=TRUE)
  
  # extract all <word> elements and children into XmlNodeList object
  word.nodes <- getNodeSet(doc.object, "//word")
  
  
  word.list <- xmlApply(word.nodes, xmlToList)
  
  
  for (j in seq_along(var_names.v)) {
    
    if ( j == 1) {
      
      base.df <- word.list %>% map_chr(var_names.v[j]) %>%
        data.frame(check.names = FALSE, stringsAsFactors = FALSE)
      
    } else {
      
      base.df <- word.list %>% map_chr(var_names.v[j]) %>%
        cbind(base.df,  ., stringsAsFactors = FALSE)
      
    }
    
  } # end of j loop
  
  colnames(base.df) <- var_names.v
  
  index.v <- sample(nrow(base.df), 1000, replace = FALSE) %>%
    sort()
  
  base.df <- base.df[index.v, ]
  
  
   
  selected_vars.list <- list() # make empty list object to store result of loop
 nomina.v <- NULL # make empty vector to store names
   
  
   for (k in 1:3) { 
    selected_vars.list[[k]] <- combn(var_names.v, k, simplify = FALSE) # make all possible combinations of variables
     nomina.v <- paste(length(var_names.v), "Choose",  k, collapse = " ") %>% # create names for elements in list
       append(nomina.v, .)
   }
   
   names(selected_vars.list) <- nomina.v  # assign names to list elements
   
   selected_vars.list[[1]] <- selected_vars.list[[1]][-c(19:38)] 
  
  new_vars.m <- matrix(nrow = nrow(base.df), ncol = 1)
  
  list_of_variable_tuples <- vector(mode = "list", length = length(selected_vars.list))
  
  
  for (m in seq_along(selected_vars.list)) {
    
    start_time_2 <- Sys.time()
    
    a <- sapply(selected_vars.list[[m]], generate_vars, df = base.df)
    
    nomina <- sapply(selected_vars.list[[m]], generate_var_names)
    
    colnames(a) <- nomina
    
    new_vars.m <- cbind(new_vars.m, a)
    
    end_time <- Sys.time()
    
    list_of_variable_tuples[[m]] <- rbind(list_of_variable_tuples[[m]], a)
    
    end_time_2 <- Sys.time()
    
    print(paste("variables for tuple of length", m, "time elapesed = ",   end_time_2 - start_time_2 ))
    
  }
  
  new_vars.m <- new_vars.m[, -1]
  
  
  new_vars.m[str_detect(new_vars.m, "NA")] <- NA # add logical NA to cells
  
  
  
  new_vars.m[str_detect(new_vars.m, "NULL")] <- NA # add logical NA to cells
  
  new_vars.m <-tolower(new_vars.m)
  
  vars.tib <- as_tibble(new_vars.m)
  
  
  file_var_ct.tib <-   gather(vars.tib, variable_name, variable_value, na.rm = TRUE) %>%
    group_by(variable_name) %>%
    summarize(n())
  
  
  
  holder.tib <- bind_rows(holder.tib, file_var_ct.tib)
  
  
  # handle list object with separate tuples
  
  for (n in seq_along(list_of_variable_tuples)) {
    
    # list_of_variable_tuples[[n]] <- list_of_variable_tuples[[n]][-1, ]
    
    list_of_variable_tuples[[n]][str_detect(list_of_variable_tuples[[n]], "NA")] <- NA
    
    list_of_variable_tuples[[n]][str_detect(list_of_variable_tuples[[n]], "NULL")] <- NA
    
    list_of_variable_tuples[[n]] <- tolower(list_of_variable_tuples[[n]])
    
    list_of_variable_tuples[[n]] <- as_tibble(list_of_variable_tuples[[n]])
    
    list_of_variable_tuples[[n]] <-   gather(list_of_variable_tuples[[n]], variable_name, variable_value, na.rm = TRUE) %>%
      group_by(variable_name) %>%
      summarize(n())
    
    holder.list[[n]] <- rbind(holder.list[[n]], list_of_variable_tuples[[n]])
    
  }
  
  end_time_1 <- Sys.time()
  print(paste("end of loop", i, "time elapesed = ",   end_time_1 - start_time_1 ))
  
  
  
  
}

#####################

temp.list <- selected_vars.list
selected_vars.list <- y


selected_vars.list[[1]] <- selected_vars.list[[1]][-c(19:38)] 

culled_bigram_var_names.tib <- ordered.tib[1:1126, ]

str_split(culled_bigram_var_names.tib$variable_name, "_&_")

for (i in 1:10) {
  
z <-  str_split(culled_bigram_var_names.tib$variable_name[i], "_&_") 
  
}
i <- 1

generate_vars <- function(var.list, df) {
  a <- unlist(var.list)
  b <- df[, a]
  if (length(a) > 1) {
    c <- apply(b, 1, paste0, collapse = "/")
  } else {
    c <- b 
  }
  return(c)
}


generate_var_names <- function(var.list) {
  a <- unlist(var.list)
  b <- paste0(a, collapse = "_&_")
  return(b)
}

generate_vars(z, base.df)


####################


##########################


for (i in seq_along(files.v)) {
  
  
  start_time <- Sys.time()
  
  # read xml structure from file to .R object
  doc.object <- xmlTreeParse(file.path(input.dir, files.v[i]), useInternalNodes=TRUE)
  
  # extract all <word> elements and children into XmlNodeList object
  word.nodes <- getNodeSet(doc.object, "//word")
  
  
  word.list <- xmlApply(word.nodes, xmlToList)
  
  for (j in seq_along(var_names.v)) {
    
    if ( j == 1) {
      
      base.df <- word.list %>% map_chr(var_names.v[j]) %>%
        data.frame(check.names = FALSE, stringsAsFactors = FALSE)
      
    } else {
      
      base.df <- word.list %>% map_chr(var_names.v[j]) %>%
        cbind(base.df,  ., stringsAsFactors = FALSE)
      
    }
    
  } # end of j loop
  
  colnames(base.df) <- var_names.v
  
  index.v <- sample(nrow(base.df), 1000, replace = FALSE) %>%
    sort()
  
  base.df <- base.df[index.v, ]
  
  new_vars.m <- matrix(nrow = nrow(base.df), ncol = 1)
  nomina <- NULL
  
  for (k in 1:nrow(culled_bigram_var_names.tib) ) {
    
    var_atoms <-  str_split(culled_bigram_var_names.tib$variable_name[[k]], "_&_") %>%
      unlist()
    if (length(var_atoms) > 1) {
      combined_var <- base.df[, var_atoms] %>%
        apply(., 1, paste0, collapse = "/")
      
    } else {
      combined_var <- base.df[, var_atoms]
      
    }
    new_vars.m <- cbind(new_vars.m, combined_var)
    
    nomina <- append(nomina, culled_bigram_var_names.tib$variable_name[k])
    
    if (k %% 500 == 0) {
      cat("k loop", k, "\n")
      print(Sys.time())
    }
    
  } # end of loop k
  
  
  new_vars.m <- new_vars.m[, -1]
  
  
  new_vars.m[str_detect(new_vars.m, "NA")] <- NA # add logical NA to cells
  
  
  new_vars.m[str_detect(new_vars.m, "NULL")] <- NA # add logical NA to cells
  
  new_vars.m <-tolower(new_vars.m)
  
  nomina <-  c("token_id", nomina)
  
  
  file_name <- files.v[i] %>%
    gsub(".xml","", .)
  
  token_id <- seq_len(nrow(base.df)) %>%
    paste0(file_name, "_token_", .) 
  
  
  new_vars.m <- cbind(token_id, new_vars.m)
  
  colnames(new_vars.m) <- nomina
  
  if (i == 1) {
  
    sampled_vars.tib <- as_tibble(new_vars.m)  
    
  } else {
    
    v.tib <- as_tibble(new_vars.m)
    sampled_vars.tib <- bind_rows(sampled_vars.tib, v.tib)
  }
  
  end_time <- Sys.time()
  print(paste("end of loop", i, files.v[i]))
  print(end_time - start_time)
  
  
}


