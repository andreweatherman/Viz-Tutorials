AP Weekly Grid
================
2023-12-26

``` r
library(tidyverse)
library(cbbdata)
library(rvest)
library(glue)
library(gt)
library(gtExtras)
library(magick)
library(here)
```

[So there’s a really cool AP voter graphic that gets published weekly on
the college basketball and college football subreddits by user
u/bakonydraco.](https://www.reddit.com/r/CollegeBasketball/comments/18lptk9/all_ap_voter_ballots_week_7/)
It’s designed as a ‘grid’ and surveys who voted for what team. The
publisher also ranks on deviation relative to aggregate (actual) poll. I
thought that it would be a fun little exercise to mimic the grid using
`gt` in R. I’m putting my own spin on the visualization by using the
`gt_theme_athletic` table theme from `cbbdata`, but you could get really
close to mirroring the actual grid by doing something different.

I don’t think a long and static format like this is particularly great
for social media, but this code could be very useful for conference-wide
ballots where the number of pollsters is considerably smaller.
Regardless, it’s a fun exercise and hopefully has some useful code.

## Grab the data

First up, we need to actually *get* the data. I’ve spoken with the
person who makes the visualization, and they scrape the official AP
website but mention that there are routinely some website errors each
week. As far as I can tell, there’s a much easier way to do this –
scrape CollegePollTracker.

I’m going to spare you in-depth explanations of web scraping in R, but
here is a nice and short function that uses `rvest` to grab pollster
ballot information for any given week and year. If you want a detailed
walk through, shoot me a message on Twitter and I’ll try to give you
one.

This function is going to grab the data and force it into a *wide*
format (i.e. voter column, first-place column, second-place column, …).

``` r
get_ap_voters <- function(year, week) {
  
  html <- read_html(glue('https://collegepolltracker.com/basketball/grid/{year}/week-{week}')) %>% 
    html_nodes('.gridRow')
  
  voter_data <- html %>%
    map_df(~{
      voter_name <- .x %>%
        html_node(".gridPollster a") %>%
        html_text(trim = TRUE)
      
      teams <- .x %>%
        html_nodes(".gridTeam img") %>% 
        html_attr("title")
      
      tibble(voter = voter_name, teams = list(teams))
    }) 
  
  voter_data_wide <- voter_data %>%
    unnest(teams) %>%
    mutate(rank = row_number(), .by = voter) %>%
    pivot_wider(names_from = rank, values_from = teams, names_prefix = "x")
  
  return(voter_data_wide)
  
}
```

Next up, we should probably include a row that has the *actual* AP
ballot, so let’s write a function that does just that. In reality, we
only need the team name and rank, but I’ve thrown in the extra
information in case you might have a use for it.

``` r
get_ap_poll <- function(year, week) {
  
  html <- read_html(glue('https://collegepolltracker.com/basketball/{year}/week-{week}'))
  
  teams_data <- html %>%
    html_nodes(".teamBar") %>%
    map_df(~{
      team <- .x %>% html_node(".teamName a") %>% html_text()
      rank <- .x %>% html_node(".teamRank") %>% html_text() %>% as.numeric()
      record <- .x %>% html_node(".teamRecord") %>% html_text()
      previous <- .x %>% html_node(xpath = ".//span[contains(@class, 'teamDataLabel') and contains(text(), 'Previous:')]/following-sibling::text()[1]") %>% html_text() %>% 
        str_squish() %>% as.numeric()
      high <- .x %>% html_node(xpath = ".//span[contains(@class, 'teamDataLabel') and contains(text(), 'High:')]/following-sibling::span[@class='rName']") %>% html_text()
      low <- .x %>% html_node(xpath = ".//span[contains(@class, 'teamDataLabel') and contains(text(), 'Low:')]/following-sibling::span[@class='rName']") %>% html_text()
      next_game <- .x %>% html_node(xpath = ".//span[contains(@class, 'teamDataLabel') and contains(text(), 'Next:')]/following-sibling::text()[1]") %>% html_text()
      points <- .x %>% html_node(".teamPoints b") %>% html_text() %>% as.numeric()
      
      tibble(team, rank, record, previous, high, low, next_game, points)
    }) %>% 
    # sometimes the ranking is NA so lets rank based on order (correct)
    mutate(rank = row_number())
  
  return(teams_data)
  
}
```

Our per-ballot data will be in a wide format, which is perfect for the
grid, but our actual poll information, which we just grabbed above, is
not. Let’s use `tidyr` to pivot that data to a format that can be joined
with our per-ballot frame.

``` r
ap_poll <- get_ap_poll(2023, 8) %>% 
  select(team, rank) %>% 
  filter(rank <= 25) %>% # we only want ranked teams
  mutate(voter = 'AP POLL') %>% 
  pivot_wider(names_from = rank, values_from = team, names_prefix = "x")
```

Now, let’s join the data.

``` r
polls <- bind_rows(ap_poll, get_ap_voters(2023, 8))
```

We also want to add a column to our table that represents the deviation
between a pollster’s ballot and the actual poll. We will use standard
deviation for this value. It’s easiest to create a new tibble that
forces everything to a *long* format.

``` r
polls_long <- polls %>%
  pivot_longer(cols = -voter, names_to = "team_rank", values_to = "team")

voters_dev <- polls_long %>% 
  filter(voter != 'AP POLL') %>% 
  left_join(get_ap_poll(2023, 8) %>% select(team, ap_rank = rank), by = "team") %>% 
  mutate(deviation = abs(as.numeric(gsub("x", "", team_rank)) - as.numeric(gsub("x", "", ap_rank)))) %>% 
  summarize(sd_votes = sd(deviation), .by = voter)

# join back
polls <- left_join(polls, voters_dev, by = 'voter') %>% 
  mutate(sd_votes = replace_na(sd_votes, 0))
```

In a perfect world, we would iterate through the website and create a
dictionary that could be used to match team names from
CollegePollTracker to `cbbdata` – and maybe I’ll do that in the future –
but for now, and since only nine names don’t match, we will just use
`case_when`.

We are also using `across` to replace the team names with team logos
from `cbd_teams`. This is a quick and dirty way of doing something
similar to a VLOOKUP.

``` r
team_logos <- cbd_teams() %>% 
  select(team_name = torvik_team, logo)

polls <- polls %>%
  # some team names do not match so fix them
  mutate(across(-c(voter, sd_votes), ~ case_when(
                .x == 'Colorado State' ~ 'Colorado St.',
                .x == 'Iowa State' ~ 'Iowa St.',
                .x == 'Miami (FL)' ~ 'Miami FL',
                .x == 'Michigan State' ~ 'Michigan St.',
                .x == 'Mississippi State' ~ 'Mississippi St.',
                .x == 'Ohio State' ~ 'Ohio St.',
                .x == 'Ole Miss' ~ 'Mississippi',
                .x == 'San Diego State' ~ 'San Diego St.',
                .x == 'UConn' ~ 'Connecticut',
                .default = .x)),
      across(-c(voter, sd_votes), ~map_chr(.x, ~team_logos$logo[team_logos$team_name == .x]))) %>% 
  setNames(c('voter', 1:25, 'sd'))
```

## Plot

That’s all the data that we need! Time for the fun part – plotting! We
will be using `gt` to create a similar ‘grid.’ Most things here are
pretty standard. I’m using `fmt_image` to render the image URL as the
actual team logo. This is distinctly different than using
`fmt_markdown`, which we typically would do with `cbd_gt_logos`, because
our image link is not wrapped in any tags; it’s just the actual URL and
`fmt_markdown` would create a hyperlink of it, not plot the image.

I like throwing my tables and plots into functions. It just makes
accessing things about the data itself a bit easier; like here, where I
need to grab the number of rows in the data (with `data_color` to ensure
that we do not color the first row).

``` r
plot_ap_table <- function(data) {
  
  polls %>% 
    arrange(sd) %>% 
    gt(id = 'ap') %>% 
    gt_theme_athletic() %>% 
    opt_row_striping() %>% 
    fmt_image(-c(voter, sd)) %>% 
    cols_move(sd, voter) %>% 
    fmt_number(sd, decimals = 2) %>% 
    data_color(sd, rows = 2:nrow(data), palette = 'nord::silver_mine', domain = 0:max(data$sd) + 0.5) %>% 
    gt_add_divider(columns = c(sd, `5`, `10`, `15`, `20`), color = 'black', include_labels = FALSE) %>%
    tab_style(locations = cells_body(rows = 1), style = cell_borders(sides = 'bottom', color = 'black', weight = px(1.5))) %>% 
    cols_align(columns = c(everything(), -voter), 'center') %>% 
    cols_align(columns = voter, 'left') %>% 
    tab_style(locations = cells_body(columns = voter),
              style = cell_text(weight = 'bold')) %>% 
    tab_options(data_row.padding = 1.2,
                heading.subtitle.font.size = 17,
                footnotes.font.size = 14,
                source_notes.font.size = 12) %>% 
    tab_footnote(locations = cells_column_labels(columns = sd), footnote = md("'SD' indicates the standard deviation of a voter's rankings from the AP POLL. Higher SD values mean greater divergence from<br>the AP poll, while lower values suggest closer alignment.")) %>% 
    tab_header(
      title = '2023-24 College Basketball AP Poll Ballot Tracker: Week 9 (Dec. 25)',
      subtitle = md('Tracking media ballots for the college basketball Associated Press (AP) poll during the week of December 18 - December 25, 2023.<br>Pollsters are sorted by standard deviation relative to aggregate poll.')
    ) %>% 
    tab_source_note(source_note = md('Data by CollegePollTracker<br>Viz. + Analysis by @andreweatherman (inspiration from u/bakonydraco)')) %>% 
    opt_css(
      '
      #ap .gt_heading {
         padding-bottom: 0px;
         padding-top: 6px
        }
      #ap .gt_subtitle {
         padding-top: 2px;
         padding-bottom: 6px;
        }
      '
    )
  
}
```

That’s all of the code that we need for our table! Let’s save an image
of it. Our table is pretty wide, so we need to set a width, with
`vwidth`, to capture the entire thing. Remember that we can use this
nice helper function to automatically trim our table after we save it,
so what we set as the width doesn’t really matter (as long as we get the
entire table).

``` r
crop_gt <- function(file, whitespace) {
  image_read(file) |> 
    image_trim() |> 
    image_border("white", glue('{whitespace}x{whitespace}')) |> 
    image_write(file)
}

gtsave_extra(plot_ap_table(polls), here('ap_pollster_grid', 'ap_pollster_grid.png'), vwidth = 1200, zoom = 3)
crop_gt(here('ap_pollster_grid', 'ap_pollster_grid.png'), '40')
```

<img src="ap_pollster_grid.png" width="3374" />
