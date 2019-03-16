library(dplyr)
library(bigrquery)
library(glue)
library(ggplot)
library(ggplot2)
library(DBI)
#glue allows you to 

# Define your variables
min_year <- 2012
gear <- "trawlers"
flag <- "CHN"

# Query to get effort by mmsi and date after 
#locally defined variables are wrapped in {}
all_effort_by_mmsi_query <- glue::glue_sql(
  '
  Select
  date,
  mmsi,
  fishing_hours
  FROM
  `global-fishing-watch.global_footprint_of_fisheries.fishing_effort_byvessel`
  WHERE   
  EXTRACT(YEAR FROM PARSE_DATE("%F",date)) > {min_year}
  ',
  .con = BQ_connection
  )

# Query to extract only chinese trawlers
chinese_vessels_sub_query <- glue::glue_sql(
  "
  Select
  mmsi
  FROM
  `global-fishing-watch.global_footprint_of_fisheries.fishing_vessels`
  WHERE flag = {flag}
  AND geartype = {gear}
  ",
  .con = BQ_connection
  )

full_sql <- glue::glue_sql(
  "
  SELECT
  date,
  sum(fishing_hours) AS fishing_hours
  FROM ({all_effort_by_mmsi_query}) AS a
  LEFT JOIN  ({chinese_vessels_sub_query}) AS b
  ON a.mmsi = b.mmsi
  GROUP BY date
  ORDER BY date
  DESC
  ",
  .con = BQ_connection
)

china_trawlers_ts <- DBI::dbGetQuery(BQ_connection, 
                                full_sql)

moratoria_dates <- tibble(year = c(2013:2016)) %>% 
  mutate(start_date = lubridate::ymd(paste(year,"-06-01",sep = "")),
         end_date = lubridate::ymd(paste(year,"-08-01",sep = "")))

new_year_dates <- tibble(year = c(2013:2016),
                         start_date = c(lubridate::ymd("2013-02-07"),
                                        lubridate::ymd("2014-01-28"),
                                        lubridate::ymd("2015-02-16"),
                                        lubridate::ymd("2016-02-05")),
                         end_date = c(lubridate::ymd("2013-02-13"),
                                      lubridate::ymd("2014-02-3"),
                                      lubridate::ymd("2015-02-22"),
                                      lubridate::ymd("2016-02-11")))

ggplot() +
  geom_rect(data = moratoria_dates, 
            aes(xmin = start_date, 
                xmax = end_date,
                ymin = 0,
                ymax = Inf,
                fill = "navyblue"),
            alpha = 0.5, 
            show.legend = TRUE) +
  geom_rect(data = new_year_dates, 
            aes(xmin = start_date, 
                xmax = end_date,
                ymin = 0,
                ymax = Inf,
                fill = "dodgerblue"),
            alpha = 0.5,
            show.legend = TRUE) +
  geom_line(data = china_trawlers_ts %>% 
              mutate(date = lubridate::ymd(date)) %>% 
              filter(lubridate::year(date) > 2012),
            aes(x = date, y = fishing_hours), 
            size = 0.3)+
  theme_minimal() +
  theme(axis.ticks = element_line(size = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 6),
        legend.justification = "center",
        legend.position = "bottom",
        plot.margin = margin(2,2,2,2)) + 
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y ") +
  xlab("") +
  scale_y_continuous(expand = c(0, 0),
                     labels = scales::comma) +
  ylab("Fishing hours")+
  scale_fill_manual(values = c("orange", "dodgerblue"),
                    name = " ",
                    labels = c("Chinese New Year", "Moratoria")) +
  guides(colour = guide_legend(override.aes = list(alpha = .5)))
