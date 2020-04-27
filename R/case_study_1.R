library(data.table)
library(tidyverse)

# helper function ---------------------------------------------------------
my_op_func <- function(vec_0, a_vec)
{long_vec <- rep(0, L)
 long_vec[df2_index[, the_index]] <- a_vec
 
 total_vec <- vec_0 + long_vec
 
 return(total_vec)
}

# create fake data --------------------------------------------------------
df1 <- 
  data.table(id = 1L:1000000L,
             x = sample(1:5, 1000000, replace = TRUE)) 
df1 <- df1[order(id)]

df2 <- data.table(id = sample(1001L:1001000L, 100000, replace = FALSE))
temp_data <- map(1:1000, function(x) sample(1:3, 100000, replace = TRUE))
df2 <- df2[, c(paste0("i", 1:1000)) := temp_data]
df2 <- df2[order(id)]

# all ids -----------------------------------------------------------------
df1_ids <- df1[, .(id)]
df2_ids <- df2[, .(id)]

all_ids <- full_join(df1_ids, df2_ids) %>% setDT()
all_ids <- all_ids[order(id)]

# do the operations -------------------------------------------------------
L <- dim(all_ids)[1]
all_ids <- all_ids[, `:=`(the_index = 1:L)]

df1_index <- 
  df1_ids %>% 
  left_join(all_ids) %>% 
  select(the_index) %>% 
  setDT()
df1_index <- df1_index[order(the_index)]

df2_index <- 
  df2_ids %>% 
  left_join(all_ids) %>% 
  select(the_index) %>% 
  setDT()
df2_index <- df2_index[order(the_index)]

vec0 <- rep(0, L)
vec0[df1_index[, the_index]] <- df1[, x]

system.time({
for(i in 1:1000)
{the_total <- my_op_func(vec_0 = vec0, 
                         a_vec = df2[[paste0("i", i)]])
 vec0 <- the_total
}
the_mean <- the_total /1000  
})


