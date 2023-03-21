# ============================================================================= 
#             WORLD CHESS CHAMPIONSHIP 2023 GRAPHICS FOR LICHESS
# =============================================================================

# Graphic(s) for the upcoming Ding-Nepo World Championship match
# Inspired by Simran Parwani's work for FiveThirtyEight in 2021: https://53eig.ht/3EJG2ln
# See also their 2018 charts: https://53eig.ht/2TCOYBN.


# ----  OPTIONS  --------------------------------------------------------------

# USER OPTIONS

# Pick a match
choice <- "carlsen-karjakin"    # "carlsen-karjakin", "carlsen-caruana", "carlsen-nepo", "ding-nepo"
todays_game <- 12
todays_result <- "draw"   # "p1_win", "p2_win", "draw"
output_format <- "png"

# PLOT OPTIONS

# Colours

p1_col <- "#58508d"               
p2_col <- "#bc5090"
draw_col <- "#373D3F"
  
eval_col <- "#010a1c"             # eval line on game plots  
equality_col <- "#8C979A"
header_col <- "#373D3F"
title_col <- "#000000"
subtitle_col <- "#000000"
plot_title_col <- "#000000"
grid_col <- "#E6E6E6" # C1C7C9
axis_label_col <- "#373D3F"
footer_line_col <- "#8C979A"
unplayed_text_col <- "#373D3F"
footer_text_col <- "#373D3F"
  
result_palette <- list("draw" = draw_col,
                       "p1" = p1_col,
                       "p2" = p2_col)
  
# Fonts
sans_font <- "noto"
mono_font <- "cutive" # unused

# Path to Lichess logo (for footer)
logo_path <- paste0(here::here(), "/lichess_logo.png")


# ---- PRELIMINARIES ----------------------------------------------------------

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, rvest, cowplot, cli, ggtext, here,
               patchwork, showtext, sysfonts, gridtext, grid, glue)

showtext::showtext_auto()
showtext::showtext_opts(dpi=320)
sysfonts::font_add_google("Noto Sans", family = "noto")
# sysfonts::font_add_google("Cutive Mono", family = "cutive")

# ---- MATCH PARAMETERS -------------------------------------------------------

latest_game <- todays_game

if(choice == "ding-nepo"){
  p1 <- "Ding Liren"
  p2 <- "Ian Nepomniachtchi"
  site <- "Astana"
  p1_elo <- "2500"
  p2_elo <- "2500"
  p1_flag <- "CHI"
  p2_flag <- "FID"
  match_length <- 15
  match_year <- 2023
  dates <- c(
    "Sun 9/4", "Mon 10/4", "Wed 12/4", "Thu 13/4", "Sat 15/4", "Sun 16/4", 
    "Tue 18/4", "Thu 20/4", "Fri 21/4", "Sun 23/4", "Mon 24/4", "Wed 26/4", 
    "Thu 27/4", "Sat 29/4", "Sun 30/4"
  )
}

if(choice == "carlsen-nepo"){
  p1 <- "Magnus Carlsen"
  p2 <- "Ian Nepomniachtchi"
  p1_elo <- "2500"
  p2_elo <- "2500"
  site <- "Dubai"
  p1_flag <- "NOR"
  p2_flag <- "FID"
  match_length <- 15
  match_year <- 2021
  dates <- c(
    "Sun 9/4", "Mon 10/4", "Wed 12/4", "Thu 13/4", "Sat 15/4", "Sun 16/4", 
    "Tue 18/4", "Thu 20/4", "Fri 21/4", "Sun 23/4", "Mon 24/4", "Wed 26/4", 
    "Thu 27/4", "Sat 29/4", "Sun 30/4"
  )
}

if(choice == "carlsen-caruana"){
  p1 <- "Magnus Carlsen"
  p2 <- "Fabiano Caruana"
  p1_elo <- "2835"
  p2_elo <- "2832"
  site <- "London"
  p1_flag <- "NOR"
  p2_flag <- "USA"
  match_length <- 13
  match_year <- 2018
  dates <- c(
    "Fri 9/11", "Sat 10/11", "Mon 12/11", "Tue 13/11", "Thu 15/11", "Fri 16/11", 
    "Sun 18/11", "Mon 19/11", "Wed 21/11", "Thu 22/11", "Sat 24/11", "Mon 26/11", 
    "Wed 28/11"
  )
}

if(choice == "carlsen-karjakin"){
  p1 <- "Magus Carlsen"
  p2 <- "Sergey Karjakin"
  p1_elo <- "2500"
  p2_elo <- "2500"
  site <- "New York"
  p1_flag <- "NOR"
  p2_flag <- "RUS"
  match_length <- 13
  match_year <- 2016
  dates <- c(
    "Fri 9/11", "Sat 10/11", "Mon 12/11", "Tue 13/11", "Thu 15/11", "Fri 16/11", 
    "Sun 18/11", "Mon 19/11", "Wed 21/11", "Thu 22/11", "Sat 24/11", "Mon 26/11", 
    "Wed 28/11"
  )
}

game_titles <- c(paste0("Game ", c(1:(match_length - 1))), "Tiebreaks")


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

# Text to print for today's game result
todays_result_text <- ifelse(todays_result == "draw",
                               todays_result,
                               glue("win for {ifelse(todays_result == 'p1_win', 
                                    p1, p2)}"))

# Extract game data

# Make list to store match game data
match_games <- vector(mode = "list", length = match_length)

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

# Store game eval plots in new list
match_plots <- vector(mode = "list", length = match_length)

# Extract game results/data
game_summary <- tibble(
  game = game_titles,
  p1_col = rep(NA_character_, length(game_titles)),
  p2_col = rep(NA_character_, length(game_titles)),
  result = rep(NA_character_, length(game_titles)),
  eco = rep(NA_character_, length(game_titles)),
  opening = rep(NA_character_, length(game_titles))
)

for(g in c(1:length(match_games))){
  if((!(is.null(match_games[[g]]))) & (g <= latest_game)){
    game_summary$p1_col[g] <- ifelse(str_detect(match_games[[g]]$white[1], p1_short), "white", "black")
    game_summary$p2_col[g] <- ifelse(str_detect(match_games[[g]]$white[1], p1_short), "black", "white")
    game_summary$result[g] <- match_games[[g]]$result[1]
    game_summary$eco[g] <- match_games[[g]]$eco[1]
    game_summary$opening[g] <- match_games[[g]]$opening[1]
  }
}

game_summary <- game_summary %>% 
  mutate(result_plotlabel_text = case_when(
    result == "1/2-1/2" ~ "Draw",
    result == "1-0" & p1_col == "white" ~ glue("{p1_short} wins"),
    result == "1-0" & p2_col == "white" ~ glue("{p2_short} wins"),
    result == "0-1" & p1_col == "black" ~ glue("{p1_short} wins"),
    result == "0-1" & p2_col == "black" ~ glue("{p2_short} wins"),
    TRUE ~ ""
  )) %>% 
  mutate(result_plotlabel_colour = case_when(
    result_plotlabel_text == glue("{p1_short} wins") ~ result_palette$p1,
    result_plotlabel_text == glue("{p2_short} wins") ~ result_palette$p2,
    result_plotlabel_text == "Draw" ~ result_palette$draw,
    TRUE ~ NA_character_
  ))


# Make game plots
for(g in c(1:length(match_games))){

  # Only plot evals for played games up to the "latest" game (specified in options)
  if((!(is.null(match_games[[g]]))) & (g <= latest_game)){
    
    # If the "latest" game is the tiebreaks, plot the last classical game instead
    # Only relevant for testing
    
    game <- match_games[[g]]
    
    if(game_titles[g] == "Tiebreaks") {
      game <- match_games[[g - 1]]
      game <- tibble::as_tibble(game)
    }
  
    # Make SF evals consistent across game plots
    game <- game %>%
      arrange(ply) %>% 
      mutate(evalp = ifelse(str_detect(white, p1_short), eval, eval * -1))
    
    # Create plot
    match_plots[[g]] <- ggplot(game, 
                               aes(x = ply/2, 
                                   y = evalp)) +
      
      # Shade regions where the eval favours player 1
      geom_rect(aes(
        xmin = ply/2,
        xmax = lead(ply)/2,
        ymin = 0,
        ymax = pmax(pmin(evalp, 2.2), 0)),
        fill = p1_col,
        col = NA,
        size = 0) +
      
      # geom_ribbon(aes(
      #   x = ply/2,
      #   ymin = 0, 
      #   ymax = pmax(pmin(evalp, 2.2), 0)),
      #   fill = p1_col,
      #   col = p1_col,
      #   size = 0.07,
      #   outline.type = "upper") +
    
      # Shade regions where the eval favours player 2
      geom_rect(aes(
        xmin = ply/2,
        xmax = lead(ply)/2,
        ymin = pmin(pmax(evalp, -2.2), 0), 
        ymax = 0),
        fill = p2_col,
        col = NA,
        size = 0) +
      
      # 
      
      # Plot the eval line on top
      geom_step(col = eval_col,
                size = 0.25) +
      
      # # Equality line
      # geom_segment(
      #   aes(
      #     x = 0, xend = max(ply) / 2,
      #     y = 0, yend = 0
      #   ),
      #   colour = grid_col,
      #   size = 0.1,
      #   alpha = 1,
      #   linetype = "solid"
      # ) +
      
      # Restrict the y-axis to [-2.2, 2.2]
      ylim(c(-2.2, 2.2)) +
      
      # Add a subtitle with the game number 
      labs(subtitle = paste0(game_titles[g], " - ", 
                             game_summary$result_plotlabel_text[g]),
           x = "Move",
           y = "Eval") +
      
      # Indicate players' colours - TEMPORARY
      geom_richtext(aes(x = 0,
                        y = 2),
                    label = str_extract(game_summary$p1_col[g], "^.{1}"),
                    family = sans_font,
                    size = 1.5,
                    hjust = 0.5,
                    vjust = 0.5,
                    colour = p1_col,
                    fill = NA,
                    label.colour = NA,
                    label.padding = grid::unit(rep(0, 4), "pt")
                    ) +
      geom_richtext(aes(x = 0,
                        y = -2),
                    label = str_extract(game_summary$p2_col[g], "^.{1}"),
                    family = sans_font,
                    size = 1.5,
                    hjust = 0.5,
                    vjust = 0.5,
                    colour = p2_col,
                    fill = NA,
                    label.colour = NA,
                    label.padding = grid::unit(rep(0, 4), "pt")
      ) +
      
      
      
      
      # # Add label with game result
      # geom_richtext(aes(x = 30,
      #                   y = -1),
      #               label = game_summary$result_plotlabel_text[g],
      #               family = sans_font,
      #               size = 1,5,
      #               hjust = 0.5,
      #               vjust = 0.5,
      #               colour = game_summary$result_plotlabel_colour[g],
      #               fill = NA,
      #               label.colour = NA,
      #               label.padding = grid::unit(rep(0, 4), "pt")
      #               ) +
      
      # Customise plot look
      ggplot2::theme_void() +
      ggplot2::theme(
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = grid_col, linewidth = 0.1),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        # axis.text.y = element_text(family = sans_font, colour = axis),
        # axis.text.x = element_blank(),
        axis.text = element_text(family = sans_font, colour = axis_label_col, size = 3.5),
        plot.title = element_blank(),
        plot.subtitle = element_textbox(family = sans_font, size = 3.2, hjust = 0.5, 
                                        colour = plot_title_col,
                                        margin(t = 0, l = 0, r = 0, b = 0)),
        plot.caption = element_blank(),
        plot.margin = margin(0.3, 0.3, 0.3, 0.3, "mm")
      )
  }
  
  else {
    
    # Add boxes for unplayed games/tiebreaks
    
    game <- match_games[[1]]
    
    match_plots[[g]] <- ggplot(game, 
                               aes(x = ply/2, 
                                   y = evalp)) +
      
      theme_void() +
      ggtext::geom_richtext(aes(0,0),
                            label = dates[g], 
                            family = sans_font,
                            size = 1.3,
                            text.colour = unplayed_text_col,
                            label.colour = "#ffffff") +
      # add game number
      labs(subtitle = game_titles[g],
           x = "Move",
           y = "Eval") +
  
      ylim(c(-2.2, 2.2)) +
      theme(
        panel.border = element_rect(colour = grid_col, linewidth = 0.2, fill = NA),
        axis.title = element_blank(),
        plot.subtitle = element_textbox(family = sans_font, size = 3.2, hjust = 0.5,
                                        colour = plot_title_col),
        plot.margin = margin(0.3, 0.3, 0.3, 0.3, "mm")
      )
      }
  }

# ---- HEADER -----------------------------------------------------------------

# Placeholder
header_placeholder <- gridtext::textbox_grob(text = "Header PLACEHOLDER",
                                            halign = 0.5, valign = 0.5,
                                            width = unit(1, "npc"),
                                            height = unit(1, "npc"),
                                            gp = grid::gpar(fontsize = 7, col = "blue"),
                                            box_gp = grid::gpar(col = "red"))

# Actual
# header <- gridtext::richtext_grob(text = glue("**Game {todays_game}** ended in a **{todays_result_text}**"),
#                                   halign = 0.1, valign = 0.5,
#                                   gp = grid::gpar(fontsize = 7, 
#                                                   col = header_col,
#                                                   fontfamily = sans_font))

header <- ggplot() +
  ggtext::geom_richtext(
    data = game,
    aes(x = 1, y = 0),
    label = paste0("<b>Game ", todays_game, "</b>",
                   " ended in a ", todays_result_text),
    family = sans_font,
    size = 2.3,
    fill = NA, label.color = NA,
    # lineheight = 0.6
  ) +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

# ---- TODAY'S GAME -----------------------------------------------------------

# Placeholder
today_placeholder <- gridtext::textbox_grob(text = "Today's game - PLACEHOLDER",
                                halign = 0.5, valign = 0.5,
                                width = unit(1, "npc"),
                                height = unit(1, "npc"),
                                gp = grid::gpar(fontsize = 7, col = "blue"),
                                box_gp = grid::gpar(col = "red"))

# Actual

today <- match_plots[[todays_game]] +
  labs(title = glue("Game {todays_game} ended in a <span style='color: {game_summary$result_plotlabel_colour[todays_game]}'></span> {todays_result_text}"),
       x = "Moves",
       y = "Engine evaluation") +
  
  
  ggplot2::theme(
    plot.title = ggtext::element_markdown(family = sans_font, size = 5.4, lineheight = 0.4, 
                                        colour = subtitle_col, hjust = 0.5),
    plot.subtitle = element_blank(),
    # axis.title = element_text(family = sans_font, colour = axis_label_col,
    #                           size = 3.5)
  ) # +
  
  # # Add custom annotations to "today's game plot"
  # geom_richtext(
  #   aes(x = 27, y = -1,
  #       label = glue::glue("An example of an annotation")
  #   ),
  #   family = sans_font,
  #   colour = footer_text_col,
  #   fill = NA, label.color = NA,
  #   label.padding = grid::unit(rep(0, 4), "pt"),
  #   size = 1.4,
  #   hjust = 0.5,
  #   vjust = 1
  # )

# ---- MATCH SCORE TRACKER ----------------------------------------------------

# Placeholder
score_placeholder <- gridtext::textbox_grob(text = "A match score graphic will go here",
                                halign = 0.5, valign = 0.5,
                                width = unit(1, "npc"),
                                height = unit(1, "npc"),
                                gp = grid::gpar(fontsize = 6, col = footer_line_col))

# Actual

# Extract both players' points per game to date
p1_points <- rep(0.5, latest_game) # just for testing
p2_points <- rep(0.5, latest_game) # ditto
p1_points_print <- stringr::str_replace(p1_points, "0.5", "\u00bd")
p2_points_print <- stringr::str_replace(p2_points, "0.5", "\u00bd")

# Tiebreak points should be empty
p1_points_print <- c(p1_points_print, rep("", match_length - length(p1_points)))
p2_points_print <- c(p2_points_print, rep("", match_length - length(p2_points)))

# Make score graphic
score <- grid::grobTree(
  
  # Game numbers
  grid::textGrob(c(1:(match_length-1), "T"), x = c(seq(0.25, 0.85, by = (0.85 - 0.25) / (match_length - 1))), y = 0.75,
                 hjust = 0.5, vjust = 0.5,
                 gp = grid::gpar(fontsize = 4, col = footer_text_col)),
  
  # # Score heading
  # grid::textGrob("Score", x = 0.9, y = 0.75,
  #                hjust = 0.5, vjust = 0.5,
  #                gp = grid::gpar(fontsize = 4, col = "blue")),
  
  # P1 name
  grid::textGrob(p1_short, x = 0.1, y = 0.45,
                 hjust = 0, vjust = 0.5,
                 gp = grid::gpar(fontsize = 4, col = footer_text_col)),
  
  # P2 name
  grid::textGrob(p2_short, x = 0.1, y = 0.2,
                 hjust = 0, vjust = 0.5,
                 gp = grid::gpar(fontsize = 4, col = footer_text_col)),
  
  # P1 points
  grid::textGrob(p1_points_print, x = c(seq(0.25, 0.85, by = (0.85 - 0.25) / (match_length - 1))), y = 0.45,
                 hjust = 0.5, vjust = 0.5,
                 gp = grid::gpar(fontsize = 4.5, col = footer_text_col)),
  
  # P1 match score
  grid::textGrob(sum(p1_points), x = 0.92, y = 0.45,
                 hjust = 0.5, vjust = 0.5,
                 gp = grid::gpar(fontsize = 5.5, col = "black")),
  
  # P2 points
  grid::textGrob(p2_points_print, x = c(seq(0.25, 0.85, by = (0.85 - 0.25) / (match_length - 1))), y = 0.2,
                 hjust = 0.5, vjust = 0.5,
                 gp = grid::gpar(fontsize = 4.5, col = footer_text_col)),
  
  # P2 match score
  grid::textGrob(sum(p2_points), x = 0.92, y = 0.2,
                 hjust = 0.5, vjust = 0.5,
                 gp = grid::gpar(fontsize = 5.5, col = "black"))
  
  
)

# Footer element
footer <- grid::grobTree(
  grid::linesGrob(
    x = grid::unit(c(0, 1), "npc"),
    y = grid::unit(0.8, "npc"),
    gp = grid::gpar(col = footer_line_col)),
  
  grid::textGrob("Analysis by Stockfish 15.1",
                 x = 0, 
                 hjust = 0, 
                 vjust = 0.5, 
                 gp = grid::gpar(
                   fontsize = 4,
                   col = footer_text_col)),
  
  grid::rasterGrob(png::readPNG(logo_path), 
                   x = 0.9, 
                   y = 0.5,
                   width = 0.2))

# ---- ALL GAMES --------------------------------------------------------------

all_games <- patchwork::wrap_plots(match_plots, ncol = 3)


# ---- COMPILE/SAVE FINAL IMAGE -----------------------------------------------

graphic <- (today / score_placeholder / all_games / footer) + 
  plot_layout(ncol = 1, heights = c(4, 1, 10, 0.5))

# # Save image as PNG
# filename <- paste0("test.", output_format)
# ggplot2::ggsave(filename = filename,
#                 plot = graphic, 
#                 device = output_format,
#                 path = paste0(here::here(), "/outputs/"),
#                 width = 712, # 712
#                 height = 1451, # 1451
#                 units = "px")

graphic
