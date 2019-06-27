
# This code calculates the % of people in the default fund (no choice) and in funds with higher and lower risk exposure---------------------------------

  # 1. Males
  
    
      young_age_males  <-  funds_tidy %>% 
      filter(Age %in% c("<20", "20-25", "25-30", "30-35")) %>% 
      select(Age, Male, Fund) %>% 
      spread(Fund, Male) %>% 
      mutate(low_risk = C + D + E,   # assigned fund is B
             high_risk = A,
             default = B)
    
    middle_age_males <-  funds_tidy %>% 
      filter(Age %in% c("35-40", "40-45", "45-50", "50-55")) %>% 
      select(Age, Male, Fund) %>% 
      spread(Fund, Male) %>% 
      mutate(low_risk = D + E,       # assigned fund is C
             high_risk = A + B,
             default = C)
    
    old_age_males <-  funds_tidy %>% 
      filter(Age %in% c("55-60", "60-65", "65-70", ">70")) %>% 
      select(Age, Male, Fund) %>% 
      spread(Fund, Male) %>% 
      mutate(low_risk = E,
             high_risk = A + B + C,
             default = D)   # assigned fund is D
    
    males_tidy      <-  bind_rows(young_age_males, 
                                  middle_age_males, 
                                  old_age_males) %>% 
                        select(Age, low_risk, high_risk, default)
   
    rm(young_age_males, middle_age_males, old_age_males)
  
  
  # 2. Females
    
    young_age_females  <-  funds_tidy %>% 
      filter(Age %in% c("<20", "20-25", "25-30", "30-35")) %>% 
      select(Age, Female, Fund) %>% 
      spread(Fund, Female) %>% 
      mutate(low_risk = C + D + E,   # assigned fund is B
             high_risk = A,
             default = B)
    
    middle_age_females <-  funds_tidy %>% 
      filter(Age %in% c("35-40", "40-45", "45-50", "50-55")) %>% 
      select(Age, Female, Fund) %>% 
      spread(Fund, Female) %>% 
      mutate(low_risk = D + E,       # assigned fund is C
             high_risk = A + B,
             default = C)
    
    old_age_females <-  funds_tidy %>% 
      filter(Age %in% c("55-60", "60-65", "65-70", ">70")) %>% 
      select(Age, Female, Fund) %>% 
      spread(Fund, Female) %>% 
      mutate(low_risk = E,
             high_risk = A + B + C,
             default = D)   # assigned fund is D
    
    females_tidy      <-  bind_rows(young_age_females, middle_age_females, old_age_females) %>% 
                          select(Age, low_risk, high_risk, default)
    
    rm(young_age_females, middle_age_females, old_age_females)
  
  
  # 3. Pooling males and females
  
    df_aux_age_q <-  bind_rows(females_tidy, males_tidy)  %>% # pooling all affiliates
                        group_by(Age) %>% 
                        mutate(N = low_risk + high_risk + default,
                               low_risk = low_risk/N,
                               high_risk = high_risk/N,
                               default = default/N, 
                               date = as.Date(paste0(yyyy, "-", mm, "-30"))) %>% 
                        select(-N)
    
    df_aux_q <- bind_rows(females_tidy, males_tidy) 
    df_aux_q <- df_aux_q %>% 
                          summarise(low_risk = sum(low_risk)/sum(colSums(df_aux_q[,-1])),
                                   high_risk = sum(high_risk)/sum(colSums(df_aux_q[,-1])),
                                   default   = sum(default)/sum(colSums(df_aux_q[,-1]))) %>% 
                          mutate(date = as.Date(paste0(yyyy, "-", mm, "-30")))
  
    rm(females_tidy, males_tidy)