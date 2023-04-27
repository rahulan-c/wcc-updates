# Compile game data from all prev WCC matches

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, rvest, cli)

# Get match PGNs
url <- "https://github.com/michael1241/wcc_analysis/tree/master/analysed_pgns"
prefix <- "https://raw.githubusercontent.com/michael1241/wcc_analysis/master/analysed_pgns/"

pgn_names <- read_html(url) %>%
  rvest::html_elements(".js-navigation-open.Link--primary") %>%
  rvest::html_text()

# We only want matches, so exclude the 1948 and 2007 tournaments
pgn_names <- pgn_names[!(str_detect(pgn_names,
                                    "lichess_study_1948-world-championship-tournament_by_Lichess_2021.11.29.pgn"))]
pgn_names <- pgn_names[!(str_detect(pgn_names,
                                    "lichess_study_2007-world-championship-tournament_by_Lichess_2021.11.29.pgn"))]
pgn_links <- paste0(prefix, pgn_names)
# pgn_links <- pgn_links[1:5] # JUST FOR TESTING CARLSEN-NEPO, DELETE LATER

# -----------------------------------------------------------------------------

# Iterate through each match PGN to parse and extract game and match details

lst_matchdata <- list()
lst_matchgames <- list()

for (m in seq(1:length(pgn_links))) {
  
  match <- readr::read_file(pgn_links[m])
  games <- str_split(match, "\\[Event")[[1]]
  games <- games[2:length(games)] # 1st element is always empty
  match_games <- vector(mode = "list", length = length(games))
  
  for (g in c(1:length(games))) {
    
    game <- games[[g]]
    
    event <- str_extract(game, "^.+\\]") %>% 
      str_remove("\\]") %>% 
      str_remove_all("\"") %>% 
      str_squish()
    
    site <- str_extract(game, "\\[Site .+\\]") %>% 
      str_remove("\\[Site ") %>% 
      str_remove("\\]") %>% 
      str_remove_all("\"") %>% 
      str_squish()
    
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
    
    # Extract move evals
    evals <- unlist(str_match_all(game, "(?<=eval ).*?(?=\\])"))
    
    # Convert mate evals (eg #-8, #4) to numbers
    mate_evals <- evals[str_detect(evals, "#")]
    mate_evals <- str_remove_all(mate_evals, "#")
    mate_evals <- as.numeric(mate_evals)
    mate_evals <- (mate_evals / abs(mate_evals)) * ((21 - pmin(abs(mate_evals), 10)))
    
    evals[str_detect(evals, "#")] <- mate_evals
    evals <- as.numeric(evals)
    
    # evals <- str_extract_all(game, "\\{ \\[%eval [:graph:]{1,}\\] \\}")[[1]] %>% 
    #   str_remove_all("\\{ \\[%eval ") %>% 
    #   str_remove_all("\\] \\}")
    # 
    # mate_evals <- evals[str_detect(evals, "#")]
    # mate_evals <- str_remove_all(mate_evals, "#")
    # mate_evals <- as.numeric(mate_evals)
    # mate_evals <- (mate_evals / abs(mate_evals)) * ((21 - pmin(abs(mate_evals), 10)))
    # evals[str_detect(evals, "#")] <- mate_evals
    # evals <- as.numeric(evals)
    
    ply <- seq(1:length(evals))
    
    game_data <- tibble::tibble( event = event,
                                 site = site,
                                 round = round,
                                 white = white,
                                 black = black,
                                 result = result,
                                 evals = evals,
                                 ply = ply,
                                 eco = eco,
                                 opening = opening)
    match_games[[g]] <- game_data 
  }
  
  # Get game details for compiling into a match summary dataset
  lst_matchsummary <- vector(mode = "list", length = length(match_games))
  for (n in c(1:length(match_games))) {
    lst_matchsummary[[n]] <- match_games[[n]][1,]
    lst_matchsummary[[n]]$plies <- max(match_games[[n]]$ply)
    lst_matchsummary[[n]]$eval_max <- max(match_games[[n]]$evals)
    lst_matchsummary[[n]]$eval_min <- min(match_games[[n]]$evals)
  }
  match_data <- data.table::rbindlist(lst_matchsummary, fill = T)
  lst_matchdata[[m]] <- match_data
  lst_matchgames[[m]] <- match_games
  cli::cli_alert_info("{m}/{length(pgn_links)} PGNs parsed")
}
cli::cli_alert_success("All PGNs parsed!")

# Collate all data
game_details <- data.table::rbindlist(lst_matchdata, fill = T)

rm(game_data, match_games, match_data, lst_matchsummary, black, eco, evals,
   event, g, game, games, m, match, mate_evals, n, opening, pgn_links, pgn_names,
   ply, prefix, result, round, site, url, white)


# Save datasets
# TODO


# Check for number of decisive games in first 6 games
decisive <- tibble::tibble(
  match = seq(1:length(lst_matchdata)),
  event = rep(NA_character_, length(lst_matchdata)),
  decisive = rep(NA_integer_, length(lst_matchdata)),
  draws = rep(NA_integer_, length(lst_matchdata))
)
for(m in c(1:length(lst_matchdata))){
  decisive$event[m] <- lst_matchdata[[m]]$event[1]
  decisive$decisive[m] <- as.integer(str_count(str_c(lst_matchdata[[m]]$result[1:7], collapse = " "), "1-0|0-1"))
  decisive$draws[m] <- as.integer(str_count(lst_matchdata[[m]]$result[1:7], "1/2-1/2"))
}

# Compile openings data
wcc_openings <- game_details %>%
  select(opening, result) %>% 
  group_by(opening) %>% 
  summarise(games = n(),
            wins_w = sum(result == "1-0"),
            wins_b = sum(result == "0-1"),
            draws = sum(result == "1/2-1/2")) %>%
  mutate(score = round((wins_w + (draws * 0.5)) / games, 2)) %>% 
  arrange(desc(games), desc(score))

best_white <- wcc_openings %>% 
  filter(score == 1)


###############

tmp <- game_details %>% 
  rowwise() %>% 
  mutate(p1 = str_sort(c(white, black))[1],
         p2 = str_sort(c(white, black))[2]) %>% 
  mutate(result_2 = case_when(
    result == "1-0" & p1 == white ~ "p1_win",
    result == "0-1" & p1 == white ~ "p2_win",
    result == "1-0" & p1 == black ~ "p2_win",
    result == "0-1" & p1 == black ~ "p1_win",
    TRUE ~ NA_character_
  ))

tmp %>% 
  filter()


## Identify games where both colours had advantages >= +2

for
         