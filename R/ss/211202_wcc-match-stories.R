
# PLOT WCC MATCH 'STORIES'
# 2021-12-02

# ----  CHOICES  --------------------------------------------------------------

p1_col <- "#58508d"           
p2_col <- "#bc5090"            
eval_col <- "#003f5c"  
equality_col <- "#555F61"

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, rvest, cowplot, cli, ggtext)


# ----  DATA  -----------------------------------------------------------------

url <- "https://github.com/michael1241/wcc_analysis/tree/master/analysed_pgns"
prefix <- "https://raw.githubusercontent.com/michael1241/wcc_analysis/master/analysed_pgns/"

pgn_names <- read_html(url) %>%
  rvest::html_elements(".js-navigation-open.Link--primary") %>%
  rvest::html_text()

# Exclude tournaments (1948, 2007)
pgn_names <- pgn_names[!(str_detect(pgn_names,
                                    "lichess_study_1948-world-championship-tournament_by_Lichess_2021.11.29.pgn"))]
pgn_names <- pgn_names[!(str_detect(pgn_names,
                                    "lichess_study_2007-world-championship-tournament_by_Lichess_2021.11.29.pgn"))]

pgn_links <- paste0(prefix, pgn_names)

lst_matchdata <- list()
lst_matchsummary <- list()

# For testing, only use a small sample of games
sample_size <- 1
pgn_links <- sample(pgn_links, sample_size)
pgn_links <- pgn_links[length(pgn_links)-12] # to manually test Carlsen-Caruana

# Get match data by parsing PGNs
for (m in seq(1:length(pgn_links))) {
  
  match <- readr::read_file(pgn_links[m])
  games <- str_split(match, "\\[Event")[[1]]
  
  match_games <- tibble::tibble(event = character(),
                                site = character(),
                                round = integer(),
                                white = character(),
                                black = character(),
                                result = character(),
                                evals = numeric(),
                                ply = integer(),
                                eco = character(),
                                opening = character())
  
  for (g in c(2:length(games))) {
    
    game <- games[[g]]
    
    event <- str_extract(game, "^.+\\]") %>% 
      str_remove("\\]") %>% 
      str_remove_all("\"")
    
    site <- str_extract(game, "\\[Site .+\\]") %>% 
      str_remove("\\[Site ") %>% 
      str_remove("\\]") %>% 
      str_remove_all("\"")
    
    round <- str_extract(game, "\\[Round .+\\]") %>% 
      str_remove("\\[Round ") %>% 
      str_remove("\\]") %>% 
      str_remove_all("\"")
    round <- as.integer(round)
    
    white <- str_extract(game, "\\[White .+\\]") %>% 
      str_remove("\\[White ") %>% 
      str_remove("\\]") %>% 
      str_remove_all("\"")
    
    black <- str_extract(game, "\\[Black .+\\]") %>% 
      str_remove("\\[Black ") %>% 
      str_remove("\\]") %>% 
      str_remove_all("\"")
    
    result <- str_extract(game, "\\[Result .+\\]") %>% 
      str_remove("\\[Result ") %>% 
      str_remove("\\]") %>% 
      str_remove_all("\"")
    
    eco <- str_extract(game, "\\[ECO .+\\]") %>% 
      str_remove("\\[ECO ") %>% 
      str_remove("\\]") %>% 
      str_remove_all("\"")
    
    opening <- str_extract(game, "\\[Opening .+\\]") %>% 
      str_remove("\\[Opening ") %>% 
      str_remove("\\]") %>% 
      str_remove_all("\"")
    
    # Skip forfeits
    if(
      ((white == "Fischer, Robert James") & (black == "Spassky, Boris V") & (round == 2)) |
      ((white == "Kramnik,V") & (black == "Topalov,V") & (round == 5))
       ){
     next 
    }
    
    evals <- str_extract_all(game, "\\{ \\[%eval [:graph:]{1,}\\] \\}")[[1]] %>% 
      str_remove_all("\\{ \\[%eval ") %>% 
      str_remove_all("\\] \\}")
    
    mate_evals <- evals[str_detect(evals, "#")]
    mate_evals <- str_remove_all(mate_evals, "#")
    mate_evals <- as.numeric(mate_evals)
    mate_evals <- (mate_evals / abs(mate_evals)) * ((21 - pmin(abs(mate_evals), 10)))
    evals[str_detect(evals, "#")] <- mate_evals
    evals <- as.numeric(evals)
    
    ply <- seq(1:length(evals))
    
    match_games <- match_games %>% 
      add_row(
        event = event,
        site = site,
        round = round,
        white = white,
        black = black,
        result = result,
        evals = evals,
        ply = ply,
        eco = eco,
        opening = opening
      )
  }
  
  # Get player names (sorted A-Z)
  players <- sort(c(match_games$white[1], match_games$black[1]))
  
  # Get match score data
  game_data <- match_games %>% 
    distinct(round, .keep_all = TRUE) %>% 
    arrange(round) %>% 
    mutate(p1 = rep(players[1], nrow(.)),
           p2 = rep(players[2], nrow(.))) %>% 
    mutate(pts1 = case_when(
      result == "1-0" & white == players[1] ~ 1,
      result == "0-1" & white == players[1] ~ 0,
      result == "1/2-1/2" & white == players[1] ~ 0.5,
      result == "1-0" & white == players[2] ~ 0,
      result == "0-1" & white == players[2] ~ 1,
      result == "1/2-1/2" & white == players[2] ~ 0.5,
      TRUE ~ NA_real_
    )) %>% 
    mutate(pts2 = case_when(
      result == "1-0" & white == players[2] ~ 1,
      result == "0-1" & white == players[2] ~ 0,
      result == "1/2-1/2" & white == players[2] ~ 0.5,
      result == "1-0" & white == players[1] ~ 0,
      result == "0-1" & white == players[1] ~ 1,
      result == "1/2-1/2" & white == players[1] ~ 0.5,
      TRUE ~ NA_real_
    )) %>% 
    mutate(post1 = cumsum(pts1),
           post2 = cumsum(pts2),
           margin = post1 - post2,
           pre1 = post1 - pts1,
           pre2 = post2 - pts2,
           premargin = pre1 - pre2) %>% 
    select(-c(evals, ply))
  
  lst_matchdata[[m]] <- match_games
  lst_matchsummary[[m]] <- game_data
  cli::cli_inform("{m}/{length(pgn_links)}")
}

cli::cli_inform("")
cli::cli_inform("all done!")



# ----  PLOT  -----------------------------------------------------------------

# Test method on a single match
selection <- sample(1:sample_size, 1)

matchdata <- lst_matchdata[[selection]]
matchsummary <- lst_matchsummary[[selection]]

matchsummary_sub <- matchsummary %>% 
  select(round, p1, p2, pts1, pts2, post1, post2, margin, pre1, pre2,
         premargin)

matchdata <- dplyr::left_join(matchdata, matchsummary_sub, by = c("round")) %>%
  mutate(player = ifelse(ply %% 2 == 1, white, black)) %>% 
  rename("eval" = evals)

# Calculate scaled evals using Lichess's eval charts scale
matchdata <- matchdata %>%
  arrange(round, ply) %>% 
  mutate(evalsc = 2 / (1 + exp(-0.004 * eval * 100)) - 1) %>% 
  mutate(evalsc = ifelse(white == p1, evalsc,
                                   (-1 * evalsc)
  )) %>%
  # Compute a scaled match eval measure - this is what gets plotted as the eval line
  mutate(mevalsc = premargin + evalsc)

# Ensure players/games are equally spaced from each other in the final plot
max_ply <- max(matchdata$ply)
matchdata <- matchdata %>%
  group_by(round) %>%
  mutate(game_plies = max(ply)) %>%
  mutate(rev_ply = max_ply * (ply / game_plies)) %>%
  mutate(ply_match = ifelse(round == 1, rev_ply, (max_ply * (round - 1)) + rev_ply))

# Make match eval line a consistent y-range across matches
y_scale_factor <- 5
min_eval <- min(matchdata$mevalsc)
max_eval <- max(matchdata$mevalsc)
y_range <- max_eval - min_eval
matchdata <- matchdata %>% 
  mutate(y_val = ((mevalsc - min_eval) / y_range) * y_scale_factor
  )

zero_val <- ((0 - min(matchdata$mevalsc)) / y_range) * y_scale_factor

players <- sort(c(matchdata$white[1], matchdata$black[1]))

# Supporting data
matchinfo <- matchdata %>%
  group_by(round) %>%
  summarise(
    first_ply = min(ply_match),
    last_ply = max(ply_match),
    start_margin = min(premargin),
    min_scaled_eval = min(y_val),
    max_scaled_eval = max(y_val),
    start_eval = first(y_val),
    final_eval = last(y_val),
    game_max_yrange = ((first(y_val) / y_scale_factor) * y_range) + min_eval + 1,
    game_min_yrange = ((first(y_val) / y_scale_factor) * y_range) + min_eval - 1,
    player_1 = p1[1],
    player_2 = p2[1],
    colour_1 = ifelse(p1[1] == white[1], paste0("W"), paste0("B")),
    colour_2 = ifelse(p2[1] == white[1], paste0("W"), paste0("B")),
    pts1 = min(pts1),
    pts2 = min(pts2),
    result = result[1],
    score = paste0(post1[1], "-", post2[1]),
    game_order = round[1],
    eco = eco[1],
    opening = opening[1]
  ) %>%
  arrange(first_ply) %>%
  mutate(end_margin = lead(start_margin))

# Make sure the end_margin value for the final game reflects the match result
matchinfo$end_margin[nrow(matchinfo)] <- matchdata$post1[nrow(matchdata)] - matchdata$post2[nrow(matchdata)]

# Make plot
plt_story <- ggplot(data = matchdata) +
  
  # # Vertical game separators
  geom_segment(
    data = matchinfo[2:nrow(matchinfo),],
    aes(
      x = first_ply / 2,
      xend = first_ply / 2,
      y = min(matchdata$y_val),
      yend = max(matchdata$y_val)
    ),
    colour = "grey",
    size = 0.2,
    linetype = "dotted"
  ) +
  
  # Area under player 2's match eval line
  geom_ribbon(data = matchdata, 
              aes(ymin = pmin(y_val, zero_val), 
                  ymax = zero_val, 
                  x = ply_match / 2, 
                  y = y_val), 
              fill = p2_col, 
              col = "#ffffff", 
              alpha = 0.3, 
              size = 0.1) +
  
  # Area under player 1's match eval line
  geom_ribbon(data = matchdata, 
              aes(ymin = zero_val, 
                  ymax = pmax(y_val, zero_val), 
                  x = ply_match / 2, 
                  y = y_val), 
              fill = p1_col, 
              col = "#ffffff", 
              alpha = 0.3, 
              size = 0.1) +
  
  # Match eval line
  geom_line(data = matchdata, aes(x = ply_match / 2, y = y_val), 
            col = eval_col, 
            size = 0.5) +
  
  # Match equality line
  geom_segment(
    data = matchinfo,
    aes(
      x = first_ply / 2, xend = last_ply / 2,
      y = zero_val, yend = zero_val
    ),
    colour = equality_col,
    size = 0.3,
    alpha = 1,
    linetype = "solid"
  ) +
  
  # Game numbers
  annotate("text",
           x = ((matchinfo$first_ply / 2) + (matchinfo$last_ply / 2)) / 2,
           y = max(matchdata$y_val),
           label = paste0(matchinfo$round),
           colour = "grey25",
           size = 2.6) +
  
  
   # c(min(matchdata$y_val - 3), max(matchdata$y_val + 3)
  


  # # Colours
  # geom_point(data = matchinfo,
  #            aes(x = ((first_ply + last_ply) / 2) / 2,
  #                y = max_scaled_eval + 0.19
  #            ),
  #            shape = 21,
  #            colour = "black",
  #            fill = ifelse(matchinfo$colour_1 == "B", "black", NA),
  #            size = 2
  # ) +
  #   geom_point(data = matchinfo,
  #              aes(x = ((first_ply + last_ply) / 2) / 2,
  #                  y = min_scaled_eval - 0.82
  #              ),
  #              shape = 21,
  #              colour = "black",
  #              fill = ifelse(matchinfo$colour_2 == "B", "black", NA),
  #              size = 2
  #   ) +
  
  # Player 1 label
  geom_richtext(
    data = matchinfo[1, ],
    aes(((max_ply * nrow(matchinfo)) / 2) * 0.49,
        y = max_eval + zero_val + 1,
        label = paste0(str_extract(players[1], "[:alpha:]+"))),
    color = p1_col,
    # family = title_font,
    fill = NA, label.color = NA,
    # label.padding = grid::unit(rep(0, 4), "pt"),
    size = 5,
    hjust = 1,
    vjust = 0.5
    ) +
      
  # Player 2 label
  geom_richtext(
    data = matchinfo[1, ],
    aes(((max_ply * nrow(matchinfo)) / 2) * 0.51,
        y = max_eval + zero_val + 1,
        label = paste0(str_extract(players[2], "[:alpha:]+"))),
        color = p2_col,
        # family = title_font,
        fill = NA, label.color = NA,
        size = 5,
        hjust = 0,
        vjust = 0.5
    ) +


  theme_cowplot() +
  
  # Axis labels, title etc - for now at least
  labs(title = paste0(players[1], " vs ", players[2]),
       subtitle = paste0(str_trim(matchsummary$event[1]), " ", matchsummary$site[1]),
       x = "Moves in match",
       y = "Match score difference + in-game evaluation",
       caption = "DRAFT - WORK IN PROGRESS") +
  
  ## Tick labels ----------------------------------------------------------
  annotate("text",
           x = ((max_ply * nrow(matchinfo)) / 2) * -0.012,
           y = (((unique(matchinfo$end_margin) - min_eval) / y_range) * y_scale_factor),
           label = paste0("+", abs(unique(matchinfo$end_margin))),
           colour = ifelse(unique(matchinfo$end_margin) > 0, p1_col, 
                           ifelse(unique(matchinfo$end_margin) < 0, p2_col, "grey")), 
           # family = plotinfo_font,
           size = 3
  ) +
  ## Team 1 "leading" arrow ------------------------------------------------
  geom_segment(aes(
    x = ((max_ply * nrow(matchinfo)) / 2) * -0.015,
    xend = ((max_ply * nrow(matchinfo)) / 2) * -0.015,
    y = (((0.2 - min_eval) / y_range) * y_scale_factor),
    yend = (((0.6 - min_eval) / y_range) * y_scale_factor)),
    arrow = arrow(length = unit(0.01, "npc"),
                  type = "closed"),
    colour = p1_col,
    size = 0.1
  ) +
  ## Team 2 "leading" arrow -----------------------------------------------
  geom_segment(aes(
    x = ((max_ply * nrow(matchinfo)) / 2) * -0.015,
    xend = ((max_ply * nrow(matchinfo)) / 2) * -0.015,
    y = (((-0.2 - min_eval) / y_range) * y_scale_factor),
    yend = (((-0.6 - min_eval) / y_range) * y_scale_factor)),
    arrow = arrow(length = unit(0.01, "npc"),
                  type = "closed"),
    colour = p2_col,
    size = 0.1
  ) +
  
  # Plot limits
  ylim(c(min(matchdata$y_val - 3), max(matchdata$y_val + 3))) +
  xlim(c(((max_ply * nrow(matchinfo)) / 2) * -0.03,
         ((max_ply * nrow(matchinfo)) / 2) * 1)) +
  
  # Remove gridlines
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank()
    # text = element_text(family = plotinfo_font, colour = "grey25")
  )


# Show plot
plt_story


# ISSUES
# - sthg's wrong with a sign in the match evals