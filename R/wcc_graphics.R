# WCC UPDATE PLOTS

# ----  USER CHOICES  ---------------------------------------------------------

# Pick a match
choice <- "carlsen-nepo"  # "carlsen-karjakin", "carlsen-caruana", "carlsen-nepo"

# Player names (alphabetical by surname)
p1 <- "Magnus Carlsen"
p2 <- "Ian Nepomniachtchi"

# Colours
p1_col <- "blue"           
p2_col <- "red"            
eval_col <- "black"  
equality_col <- "grey"


# ---- PRELIMINARIES ----------------------------------------------------------
# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, rvest, cowplot, cli, ggtext, here,
               patchwork)


# ----  READ DATA  ------------------------------------------------------------

# Get match data by parsing PGNs
pgn_path <- paste0(here::here(), "/data/", choice, ".pgn")
match <- readr::read_file(pgn_path)
games <- str_split(match, "\\[Event")[[1]]

# Site
site <- str_extract(games[[2]], "\\[Site .+\\]") %>% 
  str_remove("\\[Site ") %>% 
  str_remove("\\]") %>% 
  str_remove_all("\"") %>% 
  str_squish()

# Short player names
p1_short <- str_to_title(sort(unlist(str_split(choice, "-")))[1])
p2_short <- str_to_title(sort(unlist(str_split(choice, "-")))[2])

p1_pgn <- paste0(str_split(p1, " ")[[1]][2], ", ", 
                 str_split(p1, " ")[[1]][1])
p2_pgn <- paste0(str_split(p2, " ")[[1]][2], ", ", 
                 str_split(p2, " ")[[1]][1])

# Extract game data

match_games <- vector(mode = "list", length = length(games) - 1)

for (g in c(2:length(games))) {
  
  game_data <- tibble::tibble(round = integer(),
                              white = character(),
                              black = character(),
                              result = character(),
                              eval = numeric(),
                              ply = integer(),
                              eco = character(),
                              opening = character())
  
  game <- games[[g]]
  
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
  
  # Extract move evals
  evals <- unlist(str_match_all(game, "(?<=eval ).*?(?=\\])"))
  
  # Convert mate evals (eg #-8, #4) to numbers
  mate_evals <- evals[str_detect(evals, "#")]
  mate_evals <- str_remove_all(mate_evals, "#")
  mate_evals <- as.numeric(mate_evals)
  mate_evals <- (mate_evals / abs(mate_evals)) * ((21 - pmin(abs(mate_evals), 10)))
  
  evals[str_detect(evals, "#")] <- mate_evals
  evals <- as.numeric(evals)
  
  ply <- seq(1:length(evals))
  
  game_data <- game_data %>% 
    add_row(
      round = round,
      white = white,
      black = black,
      result = result,
      eval = evals,
      ply = ply,
      eco = eco,
      opening = opening
    )
  
  match_games[[g-1]] <- game_data
}

rm(game_data)

# ---- MAKE GAME EVAL PLOTS ---------------------------------------------------

match_plots <- vector(mode = "list", length = length(games) - 1)

for(g in c(1:length(match_plots))){

  game <- match_games[[g]]
  
  # Calculate scaled evals using Lichess's eval charts scale
  game <- game %>%
    arrange(round, ply) %>% 
    mutate(evalsc = 2 / (1 + exp(-0.004 * eval * 100)) - 1) %>% 
    mutate(evalsc = ifelse(white == p1_pgn, evalsc,
                           (-1 * evalsc)
    ))
  
  match_plots[[g]] <- ggplot(game, 
                             aes(x = ply/2, 
                                 y = evalsc)) +
  
    # Area under player 1's match eval line
    geom_ribbon(aes(ymin = pmin(evalsc, 0), 
                    ymax = 0, 
                    x = ply / 2, 
                    y = evalsc), 
                fill = p2_col, 
                col = "#ffffff", 
                size = 1) +
    
    # Area under player 1's match eval line
    geom_ribbon(aes(ymin = 0, 
                    ymax = pmax(evalsc, 0), 
                    x = ply / 2, 
                    y = evalsc), 
                fill = p1_col, 
                col = "#ffffff", 
                size = 0.1) +
    
    # Eval line
    geom_line(col = eval_col, 
              size = 1) +
    
    # Equality line
    geom_segment(
      aes(
        x = 0, xend = max(ply) / 2,
        y = 0, yend = 0
      ),
      colour = equality_col,
      size = 0.3,
      alpha = 1,
      linetype = "solid"
    ) +
    
    # Show full y-axis
    ylim(c(-1, 1)) +
    
    # Plot title
    labs(title = paste0("Game ", g),
         x = "Move",
         y = "Win prob") +
    
    cowplot::theme_minimal_grid()
}

# ---- PREPARE GRAPHIC --------------------------------------------------------

graphic <- (match_plots[[1]] + match_plots[[2]] + match_plots[[3]]) /
  (match_plots[[4]] + match_plots[[5]] + match_plots[[6]]) /
  (match_plots[[7]] + match_plots[[8]] + match_plots[[9]])

graphic <- graphic + patchwork::plot_annotation(
  title = "Tiresome title",
  subtitle = "Soporific subtitle",
  caption = "Source: me"
)

graphic
