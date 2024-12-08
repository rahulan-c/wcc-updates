
# FINAL WCC GRAPHIC
# UNFINISHED

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, tibble, data.table, lubridate, rvest, cowplot, cli, ggtext, here,
               patchwork, sysfonts, gridtext, grid, glue, extrafont,
               conflicted, tictoc, fs, readr, stringr, showtext, lorem)

tictoc::tic("Produced final graphic")

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

# Load fonts and colour choices
source(glue::glue(here::here(), "/R/fonts_colours.R"))

# Read game data

source(glue::glue(here::here(), "/R/read_pgn.R"))

gametiles_per_row <- 5
gametiles_per_col <- 4
gametile_width <- 4/5
gametile_height <- 2/3
graphic_margin_left <- 1/4
graphic_margin_bottom <- 1/4
graphic_margin_between_x <- 1
graphic_margin_between_y <- 1
graphic_margin_top <- 1/4
graphic_margin_right <- 1/4
graphic_width <- graphic_margin_left + graphic_margin_right + 
  (gametiles_per_row * gametile_width) +
  ((gametiles_per_row - 1) * graphic_margin_between_x)
graphic_height <- graphic_margin_bottom + graphic_margin_top + 
  (gametiles_per_col * gametile_height) +
  ((gametiles_per_col - 1) * graphic_margin_between_y)


originsx <- rep(graphic_margin_left + 
  ((seq(1:gametiles_per_row) - 1) * gametile_width) +
  ((seq(1:gametiles_per_row) - 1) * graphic_margin_between_x),
  times = gametiles_per_col)

originsy <- rev(rep(graphic_margin_bottom + 
  ((seq(1:gametiles_per_col) - 1) * gametile_height) +
  ((seq(1:gametiles_per_col) - 1) * graphic_margin_between_y),
  each = gametiles_per_row))

# Get game data ready for plotting
matchmoves <- matchmoves %>% 
  mutate(plysc = ply / gamesummary$plies[round],
         evalsc = 2 / (1 + exp(-0.004 * (eval * 100))) - 1,
         plybig = originsx[round] + (plysc * gametile_width),
         evalbig = originsy[round] + (gametile_height / 2)  + (evalsc * (gametile_height / 2)),
         evalzero = originsy[round] + (gametile_height / 2),
         evalmax = originsy[round] + gametile_height,
         evalmin = originsy[round]
         )

# Make big plot

bigplot <- ggplot(data = matchmoves) +
  
  theme_void() +
  
  # geom_point(data = matchmoves,
  #            aes(x = plybig,
  #                y = evalbig),
  #            size = 0.25,
  #            col = "darkgrey") +
  
  # GAME PLOTS ----
  
  ## Y-axes  ----

  annotate(
    "segment",
    x = originsx,
    xend = originsx,
    y = originsy,
    yend = originsy + gametile_height,
    colour = "black",
    size = 0.1
           ) +
  
  ## X-axes  ----

  annotate(
    "segment",
    x = originsx,
    xend = originsx + gametile_width,
    y = originsy + (gametile_height/2),
    yend = originsy + (gametile_height/2),
    colour = "black",
    size = 0.1
  ) +
  
  # Shade regions where the eval favours player 1
  geom_rect(data = matchmoves,
            aes(
    xmin = plybig,
    xmax = plybig*1.01,
    ymin = evalzero,
    ymax = pmax(pmin(evalbig, evalmax), evalzero)),
    fill = p1_col,
    col = p1_col,
    size = 0) +
  
  # Shade regions where the eval favours player 2
  geom_rect(data = matchmoves,
            aes(
    xmin = plybig,
    xmax = plybig*1.01,
    ymin = pmin(pmax(evalbig, evalmin), evalzero), 
    ymax = evalzero),
    fill = p2_col,
    col = p2_col,
    size = 0) +
  
  
  xlim(c(0, 
         graphic_margin_left + (gametile_width * gametiles_per_row) + (graphic_margin_between_x * (gametiles_per_row - 1)) + graphic_margin_right
         )
       ) +
  
  ylim(
    0,
    graphic_margin_bottom + (gametile_height * gametiles_per_col) + (graphic_margin_between_y * (gametiles_per_col - 1)) + graphic_margin_top
  ) +
  
  # Game numbers - TEMPORARY
  annotate("text",
           x = originsx + (gametile_width * 0.05),
           y = originsy + (gametile_height * 0.05),
           label = seq(1:length(originsx)),
           size = 4,
           colour = "darkgrey",
           family = font_1,
           hjust = 0,
           vjust = 0
  ) +
  
  # GAME ANNOTATIONS ----
  
  ## Game 1 ----
  ggtext::geom_richtext(aes(originsx[1], originsy[1]),
                        label = glue("**Game 1** featured something."),
                        
                        family = font_1,
                        size = 1.8,
                        hjust = 0, vjust = 1,
                        fill = NA, label.color = NA,
                        label.padding = grid::unit(rep(0, 4), "pt"),
                        colour = "#222021") +
  
  ## Game 2 ----
  ggtext::geom_richtext(aes(originsx[2], originsy[2]),
                        label = glue("In **game 2**, something *else* happened."),
                        family = font_1,
                        size = 1.8,
                        hjust = 0, vjust = 1,
                        fill = NA, label.color = NA,
                        label.padding = grid::unit(rep(0, 4), "pt"),
                        colour = "#222021") +

  
  ## OTHER ELEMENTS ----

  # Title ----
  ggtext::geom_richtext(aes(0, graphic_height),
                        label = "**How the match unfolded**",
                        family = font_1,
                        size = 4,
                        hjust = 0, vjust = 0,
                        fill = NA, label.color = NA,
                        label.padding = grid::unit(rep(0, 4), "pt"),
                        colour = "#222021") # +



  # # Credits ----
  # grid::textGrob(
  #   label = glue("Credits: {lorem::ipsum(1, avg_words_per_sentence = 15)}"),
  #   x = graphic_width, y = 0,
  #   hjust = 1, vjust = 1,
  #   gp = grid::gpar(
  #     fontsize = 1.15,
  #     fontfamily = font_1,
  #     col = "#222021")
  # )
  


ggplot2::ggsave(filename = "test_final_graphic.png",
                plot = bigplot,
                device = "png",
                path = paste0(here::here(), "/outputs/"),
                width = 250, 
                height = 141, 
                units = "mm",
                dpi = 500)

tictoc::toc(log = T)

# fs::file_show(paste0(here::here(), "/outputs/", "test_final_graphic.png"))