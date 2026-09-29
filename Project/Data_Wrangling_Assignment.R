library(tidyverse)
library(nycflights13)

view(flights)
view(airports)
view(airlines)
view(planes)
view(weather)
??nycflights13
??flights
??airports
??weather

# use 'flights' and 'airports' table to find the 10 destination airports
# receiving the most amount of flights.
# Give the airport name and number of received flights

destinations <- flights |>
  group_by(dest) |>
  summarize(
    n = n()
  )|>
  left_join(
    airports |>
      select(faa, name),
      by = c("dest" = "faa")
  ) |>
  arrange(desc(n)) |>
  select(name, n) |>
  head(n=10)

destinations

# Interesting observation: The most flights receiving airports are the ones that provides
# connecting flights.



# Using 'flights', 'airports', and 'airlines', determine how many distinct destination airports 
# each airline served from NYC in 2013. Your result should be the airline's full name and number 
# of destinations the airline served.

origin <- flights |>
  group_by(carrier) |>
  summarize(
    destination_count = n_distinct(dest)
  ) |>
  left_join(airlines, by = "carrier") |>
  select(name, destination_count)|>
  arrange(desc(destination_count))

origin

# Using 'flights' and 'planes' (consider the 'tailnum' column), determine how many flights were 
# made aircraft of each airline manufacture. Which five manufacturers account for the most flights? 
# Your result should be five airline manufacturers and the number of flights accounted for by each of them.

manufactures <- flights |>
  left_join(
    planes |>
      select(tailnum, manufacturer), by = "tailnum"
  )|>
  group_by(manufacturer)|>
  summarize(
    Flight_Count = n()
  ) |>
  select(manufacturer, Flight_Count) |>
  arrange(desc(Flight_Count)) |>
  head(n=5)

manufactures

# There is a null value in the manufacturer column that has 66068 flight count.
# If it was to be excluded, then filter(!is.na(manufacturer)) after group_by() 
# will do it.




# Using 'flights' and 'planes' again, calculate the age of each plane in 2013 
# (for this question, remove rows where age cannot be determined). Create categories for 
# aircraft age, such as 0–9, 10–19, 20–29, and 30+ years old. For each category, calculate 
# the number of flights and mean departure delay. Leave a comment on anything that stood out 
# to you when deciding age ranges.

plane_age <- flights |>
  left_join(
    planes |>
      select(tailnum, year) |>
      rename(plane_year = year),
    by = "tailnum"
  ) |>
  mutate(
    age = 2013 - plane_year
  ) |>
  filter(!is.na(age))



plane_age |>
  mutate(
    age_group = case_when(
      age >= 0 & age <= 9 ~ "0-9",
      age >= 10 & age <= 19 ~ "10-19",
      age >= 20 & age <= 29 ~ "20-29",
      age >= 30 ~ "30+"
    )
  ) |>
  group_by(age_group) |>
  summarize(
    flights = n(),
    mean_delay = mean(dep_delay, na.rm = TRUE)
  )

# Interesting part was deciding how do I put breaks for age gap.
# Before separating the age group, I had to find the age by calculation the flight year
# and the plane year.



# Using 'flights' and 'weather' (check "?weather"), group the flights according to precipitation 
# (for example, flights during hours with no precipitation versus measurable precipitation; 
# you will need to decide what constitutes measurable precipitation) and compare the number of 
# flights and mean departure delay. Leave a comment on what you landed on for measurable 
# precipitation and why you chose that value.


weather_flight <- flights |>
  left_join(weather, by = c("origin","year", "month", "day", "hour")) |>
  mutate(
    precipitation = case_when(
      precip > 0 ~ "measurable",
      precip == 0 ~ "not-measurable"
    )
  )|>
  group_by(precipitation) |>
  summarize(
    flight_count = n(),
    mean_delay = mean(dep_delay, na.rm = TRUE)
  )

weather_flight

glimpse(weather)

# I considered precipitation greater than 0 inches to be measurable.
# I chose this because 0 represents no precipitation, while
# any numeric value > 1 represents measured precipitation.
