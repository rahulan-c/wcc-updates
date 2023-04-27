# ============================================================================= 
#                WORLD CHAMPIONSHIP MATCH VISUSALISATIONS
# =============================================================================

# IDEA
# Make graphics visualising the latest status of the World Championship match 
# to share via Lichess blogs and social media posts. Initial design based on 
# FiveThirtyEight's graphics for the 2021 Carlsen-Nepo match, eg 
# https://53eig.ht/3EJG2ln (credit: Simran Parwani). Also note their 2018 graphics: 
# https://53eig.ht/2TCOYBN.

# NOTES
# - currently optimised for a 4-column layout
# - lots still left to do

# ----  USER CHOICES ----------------------------------------------------------

test_mode <- FALSE
output_format <- "png"
all_games_cols <- 4 

choice <- "nepo-ding"    # "carlsen-karjakin", "carlsen-caruana", "carlsen-nepo", "nepo-ding"
todays_game <- 2
todays_result <- "p1_win"         # "p1_win", "p2_win", "draw"



# Enter annotations for "today's game"
add_annotations <- FALSE

annotations <- tibble::tibble(
  x_pos = c(10),
  comment = c("Carlsen's 25...a5 steered the<br> 
  game towards a draw, and<br>
              a playoff for the title!"),
  x_adj = c(0.2),
  y_pos = c(2.2),
  x_just = c(0),
  y_just = c(1)
)

# for carlsen-caruana g12, x_pos should be 50
# reduce to 10 for other matches, ignore result ofc


# ---- FONT CHOICES -----------------------------------------------------------

font_1 <- "Noto Sans"
font_2 <- "cutive" # cutive, Roboto Mono


# ---- COLOUR CHOICES ---------------------------------------------------------

colours_538 <- c("#e99676", "#b73a0b", "#6fd1d7", "#36a2a8")
colours_test_1 <- c("#8ECDB5", "#59B692", "#CD8EA7", "#B6597E")
colours_test_2 <- c("#ADACD0", "#7D7BB4", "#94C2E5", "#569FD6")
colours_test_3 <- c("#C7A3D4", "#B07CC2", "#bbdab0", "#8EC27C")
colours_test_4 <- c("#21586d", "#21586d", "#813a3b", "#813a3b")
colours_test_5 <- c("#0c6182", "#0c6182", "#8a3132", "#8a3132") # 25% more saturated than 4
colours_test_6 <- c("#006994", "#006994", "#922724", "#922724")

# PICK COLOUR SCHEME
colours <- colours_test_6


p1_col <- colours[1]             
p1_dark_col <- colours[2]
p2_col <- colours[3]      
p2_dark_col <- colours[4]

draw_col <- "#8C979A"                 # draws
draw_dark_col <- "#8C979A"
eval_col <- "#010a1c"             # eval line on game plots  
equality_col <- "#8C979A"         # x-axis on game plots

# header_col <- "#373D3F"           # titles?

plot_title_col <- "black"       # game plot titles
plot_subtitle_col <- "purple"  # game plot subtitles

annotation_col <- "black"      # today's game annotations

grid_col <- "#cbcbcb"             # game plot gridlines
axis_label_col <- "#8C979A"       # game plot axis labels

footer_line_col <- "#8C979A"      # graphic footer line
unplayed_text_col <- "#373D3F"    # text for unplayed games
footer_text_col <- "#373D3F"      # graphic footer text
  


# ---- SETUP ------------------------------------------------------------------

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, tibble, data.table, lubridate, rvest, cowplot, cli, ggtext, here,
               patchwork, sysfonts, gridtext, grid, glue, extrafont,
               conflicted, tictoc, fs, readr, stringr, showtext)

tictoc::tic("Produced graphic")

conflicted::conflict_prefer("today", "lubridate")
conflicted::conflict_prefer("now", "lubridate")
conflicted::conflict_prefer("year", "lubridate")
conflicted::conflict_prefer("month", "lubridate")
conflicted::conflict_prefer("day", "lubridate")
conflicted::conflict_prefer("hour", "lubridate")
conflicted::conflict_prefer("minute", "lubridate")

showtext::showtext_auto()
showtext::showtext_opts(dpi=500)
sysfonts::font_add_google("Noto Sans", family = "Noto Sans")
sysfonts::font_add_google("Cutive Mono", family = "cutive")

# Load fonts
# extrafont::loadfonts(device = "all", quiet = TRUE) 


result_palette <- list("draw" = draw_dark_col,
                       "p1" = p1_dark_col,
                       "p2" = p2_dark_col)

# Path to Lichess logo (for footer)
logo_path <- paste0(here::here(), "/lichess_logo.png")
white_pawn_path <- paste0(here::here(), "/w_pawn_svg_NoShadow.png")
black_pawn_path <- paste0(here::here(), "/b_pawn_svg_NoShadow.png")


latest_game <- todays_game

if(choice == "nepo-ding"){
  p1 <- "Ian Nepomniachtchi"
  p2 <- "Ding Liren"
  site <- "Astana"
  p2_elo <- "2500"
  p1_elo <- "2500"
  p2_flag <- "CHI"
  p1_flag <- "FID"
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
    "Fri 26/11", "Sat 27/11", "Sun 28/11", "Tue 30/11", "Wed 1/12", "Fri 3/12", 
    "Sat 4/12", "Sun 5/12", "Tue 7/12", "Wed 8/12", "Fri 10/12", "Sat 11/12", 
    "Sun 12/12", "Tue 14/12", "Wed 15/12"
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
    "Fri 11/11", "Sat 12/11", "Mon 14/11", "Tue 15/11", "Thu 17/11", "Fri 18/11", 
    "Sun 20/11", "Mon 21/11", "Wed 23/11", "Thu 24/11", "Sat 26/11", "Mon 28/11", 
    "Wed 30/11"
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
if(choice == "nepo-ding"){
  p1_short <- "Nepo"
  p2_short <- "Ding"
} else{
  p1_short <- str_to_title(sort(unlist(str_split(choice, "-")))[1])
  p2_short <- str_to_title(sort(unlist(str_split(choice, "-")))[2])
}


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

# ---- GAME PLOTS -------------------------------------------------------------

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
      mutate(evalp = ifelse(str_detect(white, p1_short), 
                            pmax(pmin(eval, 5), -5), 
                            pmax(pmin(eval * -1, 5), -5)))
    
    # Create plot
    match_plots[[g]] <- ggplot(game, 
                               aes(x = ply/2, 
                                   y = evalp)) +
      
      # Restrict the y-axis to [-2.2, 2.2]
      ylim(c(-2.2, 2.2)) +
      
      # Shade regions where the eval favours player 1
      geom_rect(aes(
        xmin = ply/2,
        xmax = lead(ply)/2,
        ymin = 0,
        ymax = pmax(pmin(evalp, 2.2), 0)),
        fill = p1_col,
        col = p1_col,
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
        col = p2_col,
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
      
      # # Restrict the y-axis to [-2.2, 2.2]
      # ylim(c(-2.2, 2.2)) +
      
      
      
      
      labs(
        
        # Plot titles: "Game [g]    [result]"
        title = paste0(
          "<b>", game_titles[g], "</b>    ", "<b><span style = 'color:", 
          game_summary$result_plotlabel_colour[g],
          ";'>", game_summary$result_plotlabel_text[g], "</span></b>"
          ),
        
        x = "Move",
        y = "Eval") +
      
      # Indicate players' colours
      
      geom_point(aes(x = 0,
                     y = 2),
                 shape = 21,
                 colour = "black",
                 fill = ifelse(game_summary$p1_col[g] == "white", "white", "black"),
                 size = 0.8,
                 stroke = 0.3
      ) +
      geom_point(aes(x = 0,
                     y = -2),
                 shape = 21,
                 colour = "black",
                 fill = ifelse(game_summary$p2_col[g] == "white", "white", "black"),
                 size = 0.8,
                 stroke = 0.3
      ) +
      
      scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120),
                         labels = c("0", "10", "20", "30", "40", "50", "60", "70", "80", "90", "100", "110", "120")) +
      
      # Customise plot look
      ggplot2::theme_void() +
      ggplot2::theme(
        # plot.title.position = "plot",
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = grid_col, linewidth = 0.1),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        # axis.text.y = element_text(family = "Noto Sans", colour = axis),
        axis.text.x = element_text(family = font_1, colour = axis_label_col, size = 3.5),
        axis.text.y = element_blank(),
        # axis.text.x = element_blank(),
        
        plot.title = ggtext::element_textbox(
          family = font_1, 
          size = 4.2, 
          lineheight = 1,
          hjust = 0, 
          vjust = 0.5,
          colour = plot_title_col,
          padding = margin(0.3, 0.3, 0.3, 0.3, "mm")#,
          # margin(t = 0, l = 0, r = 5.5, b = 0, "mm"),
          # fill = ifelse(game_summary$result_plotlabel_text[g] == "Draw", NA,
          #               ifelse(game_summary$result_plotlabel_text[g] == paste0(p1_short, " wins"), p1_col, p2_col))
          ),
        
        # plot.subtitle = ggtext::element_textbox(
        #   family = "Noto Sans", 
        #   size = 3.6, 
        #   hjust = 0.5, 
        #   vjust = 0.5,
        #   colour = plot_subtitle_col,
        #   margin(t = 0, l = 0, r = 0, b = 0, "mm")),
        
        plot.subtitle = element_blank(),
        
        plot.caption = element_blank(),
        plot.margin = margin(0, 2, 2, 0, "mm"))
    
    # Only show y-axis labels for left-most plots
    if((g - 1 == 0) || 
       ((g - 1) %% all_games_cols == 0)){
      match_plots[[g]] <- match_plots[[g]] +
        scale_y_continuous(breaks = c(-2, -1, 0, 1, 2),
                           labels = c("+2", "+1", "EVEN", "+1", "+2")) +
        
        theme(axis.text.y = element_text(family = font_1, colour = axis_label_col, size = 3.5),
              axis.text.x = element_text(family = font_1, colour = axis_label_col, size = 3.5))
    }

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
                            family = font_1,
                            size = 1.5,
                            text.colour = unplayed_text_col,
                            label.colour = "#ffffff") +
      # add game number
      labs(title = glue("**{game_titles[g]}**"),
           x = "Move",
           y = "Eval") +
  
      ylim(c(-2.2, 2.2)) +
      
      theme(
        panel.border = element_rect(colour = grid_col, linewidth = 0.2, fill = NA),
        axis.title = element_blank(),
        
        plot.title = ggtext::element_textbox(
          family = font_1, 
          size = 4.2, 
          hjust = 0, 
          vjust = 0.5,
          padding = margin(0.3, 0.3, 0.3, 0.3, "mm"),
          colour = plot_title_col),
        
        # plot.subtitle = ggtext::element_textbox(family = "Noto Sans", size = 3.2, hjust = 0.5,
        #                                 colour = plot_title_col),
        
        plot.margin = margin(0, 2, 2, 0, "mm")
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
#                                                   fontfamily = "Noto Sans"))

header <- ggplot() +
  ggtext::geom_richtext(
    data = game,
    aes(x = 1, y = 0),
    label = paste0("<b>Game ", todays_game, "</b>",
                   " ended in a ", todays_result_text),
    family = font_1,
    size = 2.3,
    fill = NA, label.color = NA,
    # lineheight = 0.6
  ) +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

# ---- TODAY'S GAME -----------------------------------------------------------

# Placeholder
today_placeholder <- patchwork::wrap_elements(gridtext::textbox_grob(text = "Today's game - PLACEHOLDER",
                                halign = 0.5, valign = 0.5,
                                width = unit(1, "npc"),
                                height = unit(1, "npc"),
                                gp = grid::gpar(fontsize = 7, col = "blue"),
                                box_gp = grid::gpar(col = "red")))

# Copy eval plot for "today's game" (so any annotations only appear under "today's game")
today_plot <- match_plots[[todays_game]]

# Construct element
today_element <- today_plot +
  labs(title = "Today's game",
       x = "Evaluation",
       y = "Move") +
  
  theme_void() +
  
  ylim(c(-3, 9)) +
  
  scale_y_continuous(breaks = c(-2, -1, 0, 1, 2),
                     labels = c("+2", "+1", "EVEN", "+1", "+2")) +
  
  theme(
    plot.title.position = "plot",
    # plot.title = element_blank(),
    plot.title = ggtext::element_markdown(family = font_1, size = 6, lineheight = 1, hjust = 0.5),
    plot.subtitle = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = grid_col, linewidth = 0.1),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(family = font_1, colour = axis_label_col, size = 4.5)
  ) # +
  
  # # Add "Today's game" text above plot
  # annotate("text",
  #          x = max(match_games[[todays_game]]$ply)/4,
  #          y = 2.5,
  #          label = glue::glue("Today's game"),
  #          size = 2,
  #          colour = "black",
  #          family = font_1,
  #          hjust = 0.5,
  #          vjust = 0.5
  # )
  
  

# Add annotations to plot
if(add_annotations){
  today_element <- today_element +
    geom_richtext(
      aes(x = annotations$x_pos/2 + annotations$x_adj,
          y = annotations$y_pos,
          label = annotations$comment),
      family = font_1,
      colour = annotation_col,
      size = 1.2,
      fill = NA, 
      label.color = NA,
      label.padding = grid::unit(rep(0, 4), "pt"),
      hjust = annotations$x_just,
      vjust = annotations$y_just
    )
}

# ---- MATCH SCORE TRACKER ----------------------------------------------------

# Count number of wins for each player and draws
num_p1_wins <- sum(str_count(game_summary$result_plotlabel_text, p1_short))
num_p2_wins <- sum(str_count(game_summary$result_plotlabel_text, p2_short))
num_draws <- todays_game - num_p1_wins - num_p2_wins

# Convert to player scores in character format
p1_score <- num_p1_wins + (num_draws * 0.5)
p1_score <- ifelse(is.integer(p1_score - 0.5),
                   paste0(
                     as.character(p1_score - 0.5),
                     "\u00bd"),
                   as.character(p1_score))

p2_score <- num_p2_wins + (num_draws * 0.5)
p2_score <- ifelse(is.integer(p2_score - 0.5),
                   paste0(
                     as.character(p2_score - 0.5),
                     "\u00bd"),
                   as.character(p2_score))



# Placeholder
score_placeholder <- gridtext::textbox_grob(text = "A match score graphic will go here",
                                halign = 0.5, valign = 0.5,
                                width = unit(1, "npc"),
                                height = unit(1, "npc"),
                                gp = grid::gpar(fontsize = 6, col = footer_line_col))


# FiveThirtyEight (2021) style element
score_538 <- grid::grobTree(
  
  # # Line above match score
  # grid::linesGrob(
  #   x = grid::unit(c(0.2, 0.4), "npc"),
  #   y = grid::unit(0.9, "npc"),
  #   gp = grid::gpar(col = footer_line_col,
  #                   lty = "solid",
  #                   lwd = 0.3)),
  # 
  # grid::linesGrob(
  #   x = grid::unit(c(0.6, 0.8), "npc"),
  #   y = grid::unit(0.9, "npc"),
  #   gp = grid::gpar(col = footer_line_col,
  #                   lty = "solid",
  #                   lwd = 0.3)),
  
  
  
  grid::textGrob(
    label = glue("Match score after 2 games"),
    x = 0.5, y= 0.7,
    hjust = 0.5,
    vjust = 0.5,
    gp = grid::gpar(
      fontsize = 6,
      fontfamily = font_1,
      # fontface = "bold",
      col = "black")
  ),
  
  # # "Current score"
  # gridtext::richtext_grob(
  #   text = glue("**CURRENT RECORD**"),
  #   x = 0,
  #   hjust = 0,
  #   vjust = 0.5,
  #   gp = grid::gpar(
  #     fontsize = 7,
  #     fontfamily = font_1,
  #     col = "black")
  # ),
  
  
  # P1 name
  grid::textGrob(
    label = glue("{str_to_upper(p1)}"),
    x = 0.4,
    hjust = 1,
    vjust = 0.5,
    gp = grid::gpar(
      fontsize = 7,
      fontfamily = font_1,
      fontface = "bold",
      col = p1_dark_col)
  ),
  
  # P1 wins
  gridtext::richtext_grob(
    text = glue("**{p1_score}**"),
    x = 0.5,
    hjust = 1,
    vjust = 0.5,
    padding = unit(c(.5, 2, .5, 2), "mm"),
    gp = grid::gpar(
      fontsize = 7,
      fontfamily = font_1,
      col = "white"),
    box_gp = grid::gpar(
      fill = p1_dark_col
    )
  ),
  
  # P2 name
  grid::textGrob(
    label = glue("{str_to_upper(p2)}"),
    x = 0.6,
    hjust = 0,
    vjust = 0.5,
    gp = grid::gpar(
      fontsize = 7,
      fontfamily = font_1,
      fontface = "bold",
      col = p2_dark_col)
  ),
  
  # P2 wins
  gridtext::richtext_grob(
    text = glue("**{p2_score}**"),
    x = 0.5,
    hjust = 0,
    vjust = 0.5,
    padding = unit(c(.5, 2, .5, 2), "mm"),
    gp = grid::gpar(
      fontsize = 7,
      fontfamily = font_1,
      col = "white"),
    box_gp = grid::gpar(
      fill = p2_dark_col
    )
  ),
  
  # Text underneath
  grid::textGrob(
    label = glue("First to 7.5 points wins"),
    x = 0.5, y = 0.35,
    hjust = 0.5,
    vjust = 0.5,
    gp = grid::gpar(
      fontsize = 4,
      fontfamily = font_1,
      fontface = "italic",
      col = footer_line_col)
  )# ,
  
  # # Line below 
  # grid::linesGrob(
  #   x = grid::unit(c(0, 1), "npc"),
  #   y = grid::unit(0.05, "npc"),
  #   gp = grid::gpar(col = footer_line_col,
  #                   lty = "dotted",
  #                   lwd = 0.5)
  #   )
)



# # Tabular-style score element
# score_table <- grid::grobTree(
# 
#   # Game numbers
#   grid::textGrob(c(1:(match_length-1), "T"), x = c(seq(0.25, 0.85, by = (0.85 - 0.25) / (match_length - 1))), y = 0.75,
#                  hjust = 0.5, vjust = 0.5,
#                  gp = grid::gpar(fontsize = 4, col = footer_text_col)),
# 
#   # # Score heading
#   # grid::textGrob("Score", x = 0.9, y = 0.75,
#   #                hjust = 0.5, vjust = 0.5,
#   #                gp = grid::gpar(fontsize = 4, col = "blue")),
# 
#   # P1 name
#   grid::textGrob(p1_short, x = 0.1, y = 0.45,
#                  hjust = 0, vjust = 0.5,
#                  gp = grid::gpar(fontsize = 4, col = footer_text_col)),
# 
#   # P2 name
#   grid::textGrob(p2_short, x = 0.1, y = 0.2,
#                  hjust = 0, vjust = 0.5,
#                  gp = grid::gpar(fontsize = 4, col = footer_text_col)),
# 
#   # P1 points
#   grid::textGrob(p1_points_print, x = c(seq(0.25, 0.85, by = (0.85 - 0.25) / (match_length - 1))), y = 0.45,
#                  hjust = 0.5, vjust = 0.5,
#                  gp = grid::gpar(fontsize = 4.5, col = footer_text_col)),
# 
#   # P1 match score
#   grid::textGrob(sum(p1_points), x = 0.92, y = 0.45,
#                  hjust = 0.5, vjust = 0.5,
#                  gp = grid::gpar(fontsize = 5.5, col = "black")),
# 
#   # P2 points
#   grid::textGrob(p2_points_print, x = c(seq(0.25, 0.85, by = (0.85 - 0.25) / (match_length - 1))), y = 0.2,
#                  hjust = 0.5, vjust = 0.5,
#                  gp = grid::gpar(fontsize = 4.5, col = footer_text_col)),
# 
#   # P2 match score
#   grid::textGrob(sum(p2_points), x = 0.92, y = 0.2,
#                  hjust = 0.5, vjust = 0.5,
#                  gp = grid::gpar(fontsize = 5.5, col = "black"))
# 
# 
# )

# Pick score element (score_538, score_table, score_placeholder)
score_element <- score_538

# ---- FOOTER -----------------------------------------------------------------

footer <- grid::grobTree(
  
  # Line above footer
  grid::linesGrob(
    x = grid::unit(c(0, 1), "npc"),
    y = grid::unit(0.7, "npc"),
    gp = grid::gpar(col = footer_line_col,
                    lwd = 0.3)),
  
  # Footer text
  grid::textGrob(
    label = paste0("Analysis by Stockfish 15.1 NNUE"),
                   x = 0, y = 0.5,
                   hjust = 0, vjust = 1,
                   gp = grid::gpar(
                     fontsize = 4.5,
                     fontfamily = font_1,
                     col = footer_text_col)
  ), 
  
  grid::textGrob(
    label = paste0("Design inspired by Simran Parwani's graphics for FiveThirtyEight"),
    x = 0, y = 0.24,
    hjust = 0, vjust = 1,
    gp = grid::gpar(
      fontsize = 4.5,
      fontfamily = font_1,
      col = footer_text_col)
  ), 
  
  
  # gridtext::richtext_grob(
  #   text = paste0("Analysis by Stockfish 14+ NNUE<br>
  #               Inspired by Simran Parwani's graphics for FiveThirtyEight"),
  #   x = 0, y = 0.5,
  #   hjust = 0, vjust = 1,
  #   gp = grid::gpar(
  #     fontsize = 4.5,
  #     fontfamily = font_2,
  #     col = footer_text_col)
  #   ),
    
  # Lichess logo (on right)
  grid::rasterGrob(png::readPNG(logo_path), 
                   x = 0.91, 
                   y = 0.27,
                   width = 0.17))

# ---- ALL GAMES --------------------------------------------------------------

all_games <- patchwork::wrap_plots(match_plots, ncol = all_games_cols)

# patchwork::wrap_


# ---- COMPILE/SAVE FINAL IMAGE -----------------------------------------------

graphic <- (today_element / score_element / all_games / footer) + 
  plot_layout(ncol = 1, heights = c(3, 2, 7.5, 0.5))

# Save image
test_status <- ifelse(test_mode, "TEST_", "")

filename <- paste0(test_status,
                   p1_short, "_", p2_short, "_", "Game_", todays_game, "_",
                   str_remove_all(now("UTC"), "-|:|\\s"),
                   ".", 
                   output_format)

ggplot2::ggsave(filename = filename,
                plot = graphic,
                device = output_format,
                path = paste0(here::here(), "/outputs/"),
                width = 90, # 712px
                height = 120, # 1451px
                units = "mm",
                dpi = 500)

tictoc::toc(log = T)

fs::file_show(paste0(here::here(), "/outputs/", filename))

              