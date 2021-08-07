### Libraries

library("flexdashboard")
library("readxl")
library("forcats")
library("plotly")
library("shiny") 
library("httr")
library("dplyr")
library("tidyr")
library("tools")
library("lubridate")



### Get data

url <- paste0(Sys.getenv("kobo_path"),
              Sys.getenv("kobo_data_file"),
              sep = "")
kobo_xl <- "neighborhood_nav.xlsx"
GET(url,
    write_disk(kobo_xl, overwrite = TRUE),
    authenticate(Sys.getenv("kobo_username"),
                 Sys.getenv("kobo_password")))
df_data <- read_excel(path = path.expand(kobo_xl))


### Rename variables
df_data$sub_time <- gsub("[A-Z]", " ", df_data$`_submission_time`)
df_data$sub_time <- as.POSIXlt(df_data$sub_time)

names(df_data) <- gsub("organization_group/", "", names(df_data))
names(df_data) <- gsub("doctors_without_walls_grp/", "", names(df_data))
names(df_data) <- gsub("healthcare_group/med_encounters_group/", "", names(df_data))
names(df_data) <- gsub("healthcare_group/health_volunteer_group/", "", names(df_data))
names(df_data) <- gsub("healthcare_group/med_mental_group/", "", names(df_data))
names(df_data) <- gsub("exits_group/", "", names(df_data))
names(df_data) <- gsub("shelter_group/", "", names(df_data))
names(df_data) <- gsub("hygiene_group/", "", names(df_data))
names(df_data) <- gsub("case_mgmt_group/", "", names(df_data))
names(df_data) <- gsub("animal_group/", "", names(df_data))
names(df_data) <- gsub("wrapping_up_group/", "", names(df_data))
names(df_data) <- gsub("clothing_group/", "", names(df_data))
names(df_data) <- gsub("meals_group/", "", names(df_data))
names(df_data) <- gsub("meal_role/", "", names(df_data))
names(df_data) <- gsub("coordinator_group/", "", names(df_data))
names(df_data) <- gsub("compassionate_care_group/", "", names(df_data))
names(df_data) <- gsub("weather/", "", names(df_data))
v_weather_levels <- c("cold", "comfortable", "warm", "rainy", "foggy", "smokey", "unknown")
v_warming_station_levels <- c("yes", "no", "unknown")
v_service_categories <- c("animals", "clothing", "case_mgmt", "coordinator", "healthcare", "meals", "hygiene", "exits", "other_service")

df_data %>%
  mutate(survey_date = as.Date(survey_date, "%m/%d/%y", tz ="America/Los_Angeles"),
         survey_date = round_date(df_data$survey_date, unit = "week", week_start = 4),
         weather = factor(weather, levels = v_weather_levels),
         warming_station = factor(warming_station, levels = v_warming_station_levels),
         
         start = as.Date(start, "%m/%d/%y", tz ="America/Los_Angeles"),
         end = as.Date(end, "%m/%d/%y", tz ="America/Los_Angeles"),
         organization_name = case_when(
           organization_name == "animal_care_not_listed" ~ organization_name_not_listed,
           organization_name == "clothing_not_listed" ~ organization_name_not_listed,
           organization_name == "case_mgmt_not_listed" ~ organization_name_not_listed,
           organization_name == "coordinator_not_listed" ~ organization_name_not_listed,
           organization_name == "healthcare_not_listed" ~ organization_name_not_listed,
           organization_name == "meals_not_listed" ~ organization_name_not_listed,
           organization_name == "hygiene_not_listed" ~ organization_name_not_listed,
           organization_name == "exits_not_listed" ~ organization_name_not_listed,
           organization_name == "other_service_not_listed" ~ organization_name_not_listed,
           TRUE ~ organization_name),
         organization_name = tolower(organization_name),
         organization_name = gsub(" ", "_", organization_name),
         organization_name = case_when(
           organization_name == "westmont" ~ "westmont_college",
           TRUE ~ organization_name),
         organization_service = as.factor(organization_service),
         organization_name = as.factor(organization_name)
  ) -> df_data



# Apply warming station and weather data to all rows

df_data %>%
  filter(organization_service == "coordinator")  %>%
  
  group_by(survey_date) %>%
  
  summarise(
    coord_warming_station = last(warming_station, order_by = end),
    coord_cold = last(cold, order_by = end),
    coord_comfortable = last(comfortable, order_by = end),
    coord_warm = last(warm, order_by = end),
    coord_rainy = last(rainy, order_by = end),
    coord_foggy = last(foggy, order_by = end),
    coord_smokey = last(smokey, order_by = end),
    coord_weather = last(weather, order_by = end)) %>%
  
  right_join(df_data, by = "survey_date") %>%
  
  replace_na(list(coord_warming_station = "unknown",
                  coord_weather = "unknown")) %>%
  
  mutate(warming_station = coord_warming_station,
         weather = coord_weather,
         cold = coord_cold,
         comfortable = coord_comfortable,
         warm = coord_warm,
         rainy = coord_rainy,
         foggy = coord_foggy,
         smokey = coord_smokey) -> df_data


# Only keep last entry

df_data %>% 
  group_by(survey_date, organization_name) %>%
  filter(sub_time == max(sub_time, na.rm = TRUE)) %>%
  ungroup() -> df_data






### Filter function

fn_filter_weather <- function(my_df){
  my_df %>%
    filter(
      (cold == TRUE & "cold" %in% input$input_weather) |
        (comfortable == TRUE & "comfortable" %in% input$input_weather) |
        (warm == TRUE & "warm" %in% input$input_weather) |
        (rainy == TRUE & "rainy" %in% input$input_weather) |
        (foggy == TRUE & "foggy" %in% input$input_weather) |
        (smokey == TRUE & "smokey" %in% input$input_weather) |
        (weather == "unknown" & "unknown" %in% input$input_weather),
      warming_station %in% input$input_warming_station)
}
fn_filter_dates <- function(my_df){
  my_df %>%
    filter(survey_date >= input$input_date_range[1],
           survey_date <= input$input_date_range[2])
}


### Graphing options

xaxis_options <- list(
  title = FALSE,
  autotick = FALSE,
  showticklabels = TRUE,
  tickangle = 45,
  gridcolor = "ghostwhite",
  type = 'date', 
  ticklabelmode = "instant",
  tick0=as.Date("2000-01-13"),
  dtick = 86400000.0 * 14 # Time between ticks in millisecs (1 day)
) 
yaxis_options <- list(
  title = "Count",
  rangemode = "tozero",
  gridcolor = "ghostwhite")
legend_options <- list(orientation = "h",
                       x = 0.5,
                       y = -0.2)

fn_mov_avg <- function(my_vector, window_width) {
  as.vector(stats::filter(my_vector, 
                          rep(1/window_width, window_width), 
                          sides=1));
}

westmont_colors <-  c("#9D2235", # Red
                      "#63666A", # Grey
                      "#CEB888", # Sand
                      "#00B398", # Green
                      "#004F71", # Blue
                      "#A1561C",
                      "#D50032", 
                      "#D29F13", # Gold
                      "#A1D6CA",
                      "#A7BDB1",
                      "#8B6F4E",
                      "#8A8D8F",
                      "#D9D9D6",
                      "#A7A8AA")
westmont_red <- I("#9D2235")
westmont_green <- I("#00B398")
westmont_grey <- I("#63666A")
westmont_sand <- I("#CEB888")
westmont_blue <- I("#004F71")
westmont_gold <- I("#D29F13")
outline_color <- "grey50"



### Value boxes

fn_value_box <- function(my_df, input_var){
  input_var <- enquo(input_var)
  my_df %>%
    fn_filter_weather() %>%
    fn_filter_dates() %>%
    select(survey_date, !!input_var) %>%
    summarise(my_sum = sum(!!input_var, na.rm = TRUE)) %>%
    as.numeric()
}


### Time series plots
fn_time_series_subplots <- function(my_data, my_x_var, my_count_var, my_ma_var,
                                    my_count_tooltip,
                                    my_mov_avg_tooltip){
  my_x_var <- enquo(my_x_var)
  my_count_var <- enquo(my_count_var)
  my_ma_var <- enquo(my_ma_var)
  my_count_tooltip <- enquo(my_count_tooltip)
  my_mov_avg_tooltip <- enquo(my_mov_avg_tooltip)
  
  my_data %>%
    plot_ly(x = my_x_var, 
            y = my_count_var,
            type = "scatter",
            mode = "markers",
            color = westmont_grey,
            text = my_count_tooltip,
            hoverinfo = "text",
            name = "Value") %>%
    add_lines(y = my_ma_var, 
              line = list(width = 4),
              color = westmont_red, 
              text = my_mov_avg_tooltip,
              hoverinfo = "text",
              name = "4 week moving average") %>%
    layout(xaxis = xaxis_options, 
           yaxis = yaxis_options, 
           legend = legend_options) -> p1
  
  my_data %>%
    plot_ly(y = my_count_var,
            type = "box",
            color = westmont_grey,
            opacity = 0.5,
            showlegend = FALSE,
            name = "Distribution",
            boxpoints = FALSE) %>%
    layout(xaxis = list(showticklabels = FALSE))-> p2
  
  subplot(p1,p2, nrows = 1, margin = 0, 
          widths = c(0.8, 0.2), shareY = TRUE) %>%
    plotly::config(displaylogo = FALSE,
                   modeBarButtons = list(list("toImage")))
}
fn_time_series_graph <- function(my_df, my_graphing_var, my_filter_expr){
  
  my_graphing_var <- enquo(my_graphing_var)
  my_filter_expr <- if (missing(my_filter_expr)){
    TRUE} else {
      enquo(my_filter_expr) 
    }
  my_df %>%
    fn_filter_weather() %>%
    filter(!!my_filter_expr) %>%
    group_by(survey_date) %>%
    summarise(my_sum = sum(!!my_graphing_var, na.rm = TRUE)) %>%
    mutate(my_mov_avg = fn_mov_avg(my_sum, 4),
           my_sum = ifelse(my_sum == 0, NA, my_sum),
           my_count_tooltip = paste0("Date: ", survey_date,
                                     "<br>Count: ", my_sum),
           my_mov_avg_tooltip = paste0("Date: ", survey_date,
                                       "<br>4wk avg.: ", round(my_mov_avg, digits = 1))
    ) %>%
    fn_filter_dates() -> my_graphing_data
  
  fn_time_series_subplots(my_graphing_data, 
                          survey_date,
                          my_sum, 
                          my_mov_avg,
                          my_count_tooltip,
                          my_mov_avg_tooltip)
  
}

df_data %>%
  select(survey_date,
         
         warming_station,
         cold,
         comfortable,
         warm,
         rainy,
         foggy,
         smokey,
         weather,
         
         organization_service,
         organization_name,
         num_volunteers,
         organization_name_not_listed,
         
         num_clinicians,
         num_packs,
         num_scribes,
         num_ccs,
         num_peacekeepers,
         num_med_encounters,
         num_med_men,
         num_med_women,
         num_med_caucasian,
         num_med_hispanic,
         num_med_black,
         num_med_over_65,
         num_med_under_65,
         num_mental_questions,
         num_diagnosed_mental_illness,
         num_interested_mental_services,
         num_showers,
         num_hygiene_kits,
         num_clothing,
         
         meal_role,
         provided_meal,
         served_meal,
         conversation,
         num_people_in_meal_line,
         num_meals,
         num_meal_bags,
         
         num_animals,
         num_case_mgmt_clients,
         num_diversions_initiated,
         num_ppl_compassionate_care,
         num_agency_referrals,
         num_people_remaining) %>%
  arrange(desc(survey_date)) -> df_download
