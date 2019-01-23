ex <- emoji_find[1:115,]

ex_result <- ex %>%
  group_by(id) %>%
  mutate(combine = ifelse(special_character==1 & lag(special_character) ==1, 
                          paste(dplyr::lag(escape,2),dplyr::lag(escape,1), escape, sep = ""), "")) %>%
  mutate(combine = ifelse(special_character==1 & lag(special_character)==0 & lead(special_character, default = 0)==0,
                          paste(dplyr::lag(escape,1),escape, sep = ""), combine))
         
         
         
         
    

    
    
