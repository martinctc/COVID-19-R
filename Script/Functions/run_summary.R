## Create summary on Global or by country splits

run_summary <- function(data, varname, country = FALSE, today_date = NULL){
  
  if(country == FALSE){
    
    data %>%
      select(-`Province/State`, -`Country/Region`, -Lat, -Long) %>%
      gather(Date, !!sym(varname))%>%
      group_by(Date) %>%
      summarise(!!sym(varname) := sum(!!sym(varname), na.rm = TRUE))
    
  } else if(country == TRUE){
    data %>%
      select(-`Province/State`, -Lat, -Long) %>%
      gather(Date, !!sym(varname), -`Country/Region`)%>%
      mutate_at(vars(Date), ~lubridate::mdy(.)) %>%
      group_by(Date, `Country/Region`) %>%
      summarise(!!sym(varname) := sum(!!sym(varname), na.rm = TRUE)) -> temp_ctry_df
    
    temp_ctry_df %>%
      filter(Date == today_date) %>%
      return()
  }
}


## Create summary, with country filters

run_summary_ctry <- function(data, varname, country_name = "United Kingdom"){
  data %>%
    filter(`Country/Region` == country_name) %>%
    select(-`Province/State`, -`Country/Region`, -Lat, -Long) %>%
    gather(Date, !!sym(varname))%>%
    group_by(Date) %>%
    summarise(!!sym(varname) := sum(!!sym(varname), na.rm = TRUE))

}
