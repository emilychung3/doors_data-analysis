# lydia barnes, august 2024
# if someone is deep into a set of doors, they may struggle to respond to a cue to change contexts.
# this could be independent from, or anti-correlated with, how likely they are to make a setting error later in a trial
# both of these things are bundled into setting_errors. this function separates the two.

get_setting_stability <- function(data){
  
  data <- data %>% 
    group_by(sub,ses,t) %>% 
    mutate(
      scca = case_when(diff(c(1,door_cc))>0~1,.default=0), # recording transitions from selecting incorrect door to context correct door
      sccb = case_when(diff(c(0,door_cc))>0~1,.default=0),  
      select_cc = case_when(ses==2 & switch==1 ~ sccb, .default=scca), #changes into correct context
      soca = case_when(diff(c(0,door_oc))>0~1,.default=0), 
      socb = case_when(diff(c(1,door_oc))>0~1,.default=0), 
      select_oc = case_when(ses==2 & switch==1 ~ socb, .default=soca), #changes into other context
      select_oc_late = case_when(diff(c(0,t))==1~0,.default=select_oc)
    ) %>% 
    ungroup()
  
  x <- data %>%
    group_by(sub,ses,t) %>% 
    mutate(
      t_cc = cumsum(door_cc),
      t_oc = cumsum(door_oc)
      ) %>% 
    ungroup()
  
  return(data)
}