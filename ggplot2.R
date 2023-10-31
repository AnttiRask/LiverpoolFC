library(gghighlight)
library(tidyverse)

# add promoted teams and colors, comment out relegated teams update_me
x.premier.league.clubs <-
  tribble(
    ~Team, ~Team_538, ~TeamColor, ~Team_cln,
    'Arsenal', 'Arsenal', rgb(239, 1, 7, maxColorValue = 255), "Arsenal",
    'Aston Villa', 'Aston Villa', rgb(149,191,229, maxColorValue = 255), "Aston Villa",
    'Bournemouth', 'AFC Bournemouth', rgb(181, 14, 18, maxColorValue = 255), "Bournemouth",
    'Brentford', 'Brentford', rgb(227, 6, 19, maxColorValue = 255), "Brentford",
    'Brighton', 'Brighton and Hove Albion', rgb(0, 87, 184, maxColorValue = 255), "Brighton",
    'Burnley', 'Burnley', rgb(108, 29, 69, maxColorValue = 255), "Burnley",
    'Chelsea', 'Chelsea', rgb(3, 70, 148, maxColorValue = 255), "Chelsea",
    'Crystal Palace', 'Crystal Palace', rgb(27, 69, 143, maxColorValue = 255), "Crystal Palace",
    'Everton', 'Everton', rgb(39, 68, 136, maxColorValue = 255), "Everton",
    'Fulham', 'Fulham', rgb(0, 0, 0, maxColorValue = 255), "Fulham",
    'Liverpool', 'Liverpool', rgb(200, 12, 46, maxColorValue = 255), "Liverpool",
    'Luton', 'Luton Town', rgb(247, 143, 30, maxColorValue = 255), "Luton Town",
    'Man City', 'Manchester City', rgb(108, 171, 221, maxColorValue = 255), "Manchester City",
    'Man United', 'Manchester United', rgb(218, 41, 28, maxColorValue = 255), "Manchester United",
    'Newcastle', 'Newcastle', rgb(45, 41, 38, maxColorValue = 255), "Newcastle",
    'Nottm Forest', 'Nottingham Forest', rgb(229, 50, 51, maxColorValue = 255), "Nottingham Forest",
    'Sheffield United', 'Sheffield United', rgb(238,39,55, maxColorValue = 255), "Sheffield United",
    'Tottenham', 'Tottenham Hotspur', rgb(19, 34, 87, maxColorValue = 255), "Tottenham",
    'West Ham', 'West Ham United', rgb(122, 38, 58, maxColorValue = 255), "West Ham United",
    'Wolves', 'Wolverhampton', rgb(253, 185, 19, maxColorValue = 255), "Wolverhampton"
  ) %>%
  mutate(Team = str_trim(gsub(" ", "", Team)),
         Team_538 = str_trim(gsub(" ", "", Team_538)))

# load current premier league season update_me
y.2324 <- 
  read_csv('https://www.football-data.co.uk/mmz4281/2324/E0.csv') |>
  mutate(
    HomeTeam = str_replace_all(str_trim(gsub(" ", "", HomeTeam)), "[^[:alnum:]]", ""), 
    AwayTeam = str_replace_all(str_trim(gsub(" ", "", AwayTeam)), "[^[:alnum:]]", ""))

# create copy for separate use update_me
x.current.data <- 
  y.2324 |>
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG)

# format variables and create GameID update_me
x.current.data <-
  x.current.data |> 
  mutate(
    HomeTeam = str_trim(gsub(" ", "", HomeTeam)),  
    AwayTeam = str_trim(gsub(" ", "", AwayTeam)),
    GameID = paste0(HomeTeam, AwayTeam),
    Date = as.Date(as.character(Date), format = '%d/%m/%Y'))

# create df of home team results
x.home <-
  x.current.data |>
  mutate(Team = HomeTeam,
         GoalsScored = FTHG,
         GoalsConceded = FTAG) |>
  select(GameID, Date, Team, GoalsScored, GoalsConceded)

# create df of away team results
x.away <-
  x.current.data |>
  mutate(Team = AwayTeam,
         GoalsScored = FTAG,
         GoalsConceded = FTHG) |>
  select(GameID, Date, Team, GoalsScored, GoalsConceded)

# combine home and away dfs
x.data <- rbind(x.home, x.away) |>
  mutate(played = 1) |>
  arrange(Date, GameID) |>
  # logic for calculating goal differential and points earned
  mutate(GoalDifferential = GoalsScored - GoalsConceded,
         PointsEarned = if_else(GoalsScored > GoalsConceded, 3,
                                if_else(GoalsScored < GoalsConceded, 0, 1))) |>
  group_by(Team) |>
  # create week number
  mutate(Week = row_number()) |>
  ungroup()

# create a week df to complete
x.week.teams <-
  data.frame(Week = seq(1, max(x.data$Week), 1)) |>
  mutate(k = 1) |>
  inner_join(x.premier.league.clubs |>
               mutate(k = 1),
             by = 'k') |>
  select(-k)

# aggregate totals by team
x.data_aggregated <-
  x.data |> 
  right_join(x.week.teams,
             by = c("Week", "Team")) |>
  arrange(Date, GameID) |>
  mutate(PointsEarned = if_else(is.na(PointsEarned), 0, PointsEarned),
         GoalsScored = if_else(is.na(GoalsScored), 0, GoalsScored),
         GoalsConceded = if_else(is.na(GoalsConceded), 0, GoalsConceded),
         GoalDifferential = if_else(is.na(GoalDifferential), 0, GoalDifferential)) |>
  group_by(Team) |>
  mutate(PointsTally = cumsum(PointsEarned),
         GoalsScoredTally = cumsum(GoalsScored),
         GoalsConcededTally = cumsum(GoalsConceded),
         GoalDifferentialTally = cumsum(GoalDifferential)) |>
  ungroup()

# determine league position by week
x.rank <-
  x.data_aggregated |>
  group_by(Week) |>
  arrange(desc(PointsTally), desc(GoalDifferentialTally), desc(GoalsScoredTally)) |>
  # create league position
  mutate(Position = row_number()) |>
  ungroup()

# graph looks better starting from a common point of 0
x.week.zero <-
  x.data_aggregated |>
  select(Team_cln) |>
  distinct() |>
  mutate(Week = 0,
         PointsTally = 0) |>
  spread(Team_cln, PointsTally)

# data for points by week graph
pbw.data <-
  x.data_aggregated |>
  filter(!is.na(played)) |>
  select(Week, Team_cln, PointsTally) |>
  spread(Team_cln, PointsTally) |>
  bind_rows(x.week.zero) |>
  arrange(Week)

# Extract data for Week 10 and rank teams
week_10_data <- x.rank %>% 
  filter(Week == 10) %>% 
  arrange(Position) %>% 
  select(Team_cln, Position)

# Create a vector of team names ordered by their Week 10 rankings
ordered_teams <- week_10_data$Team_cln

# Reorder the factor levels for the 'name' variable based on Week 10 rankings
pbw.data_long <- pbw.data %>%
  pivot_longer(cols = Arsenal:Wolverhampton) %>%
  mutate(name = factor(name, levels = ordered_teams))

# ggplot2 w/ gghighlight
pbw.data_long %>%
  ggplot(aes(Week, value, color = name)) +
  geom_line(linewidth = 1) +
  # scale_color_manual(values = rep("#e90052", 20)) +
  scale_color_manual(values = )
  scale_x_continuous(
    breaks = seq(0, 10, by = 2)
  ) +
  scale_y_continuous(
    breaks = seq(0, 30, by = 10),
    labels = seq(0, 30, by = 10),
    limits = c(0, 30)
  ) +
  gghighlight(
    keep_scales      = TRUE,
    use_direct_label = FALSE
  ) +
  facet_wrap(vars(name)) +
  
  labs(
    x        = NULL,
    y        = NULL,
    title    = "Premier League Point Totals by Week",
    subtitle = "2023-2024"
  ) +
  theme_classic() +
  theme(
    legend.position = "none"
  )
