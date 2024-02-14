#
# Author: Andrew Disher
# Date: 2/13/2024
# Topic: Tidy Tuesday - Valentines Spending
#
# TASK: Download data for Tidy Tuesday.

# ------------------------------
# ----- Libraries/Packages -----
# ------------------------------

box::use(
  dplyr[`%>%`, case_when, mutate], 
  ggplot2[...],
  ggtext[...],
  monochromeR[generate_palette],
  scales[percent],
  showtext[showtext_auto],
  sysfonts[font_add_google],
  tidyr[pivot_longer],
  tidytuesdayR[tt_load]
)

# ------------------------------------------
# ----- Download Data from GitHub Repo ----- 
# ------------------------------------------

tuesdata <- tt_load('2024-02-13')

historical_spending <- tuesdata$historical_spending
gifts_age <- tuesdata$gifts_age
gifts_gender <- tuesdata$gifts_gender

# -----------------
# ----- Fonts -----
# -----------------

font_add_google("Ubuntu", "ubuntu")
showtext_auto(enable = F)

# -----------------------------
# ----- Color Definitions -----
# -----------------------------
"#480000" "#5C1D1D" "#713A3A" "#865757" "#9B7474" "#B09191" "#C5AEAE" "#DACCCC"
color_palette <- generate_palette(colour = "#480000", 
                                  modification = "go_lighter", 
                                  n_colours = 8)
bg_color <- "#ffffff"
text_color <- color_palette[1]

# ----------------------
# ----- Label Text -----
# ----------------------

"Younger indivudals are typically more frivolous Valentines Day 
       spenders while older individuals favor greeting cards or don't spend 
       anything. "

plot_title <- "<b style = 'font-size:20px;'>What to buy on Valentines Day!</b> <br></br>
       <span>When it comes to what we buy for the ones we love on Valentines
       Day, the results vary drastically among age groups. <b>70% of 18-24 year olds</b> who spent money love indulging 
       on Candy while only <b>42% of those aged 65+</b> who spent money do. On the other hand, <b>33% of 18-24 year olds</b>
       who spent money purchase greeting cards while <b>44% of 65+ individuals</b> who spent money purchase them. On the whole,
       young love is more frivolous and enduring love needs no embellishment. Or we just get lazier/cheaper as we age!</span>"

x_lab <- "Age Group"

y_lab <- "Percent of <br></br>
          people  <br></br>
          engaging <br></br>
          in given <br></br>
          spending <br></br>
          habits"

# -------------------------------------
# ----- Function to Format Labels -----
# -------------------------------------

addPercent <- function(x) {
  paste0(x, "%")
}

# --------------------------
# ----- Build the Plot -----
# --------------------------

gifts_age %>% 
  pivot_longer(cols = SpendingCelebrating:GiftCards,
               names_to = "Type", 
               values_to = "Value") %>% 
  mutate(Type = case_when(Type == "GreetingCards" ~ "Bought Greeting Cards",
                          Type == "SpendingCelebrating" ~ "Spent Money at All",
                          Type == "Candy" ~ "Bought Candy",
                          Type == "Flowers" ~ "Bought Flowers",
                          Type == "Clothing" ~ "Bought Clothing",
                          Type == "EveningOut" ~ "Spent the Evening Out",
                          Type == "GiftCards" ~ "Bought Gift Cards",
                          Type == "Jewelry" ~ "Bought Jewelry")) %>% 
  ggplot(mapping = aes(x = Age, y = Value, color = Age, fill = Age)) +
  facet_wrap(~Type, ncol = 2) + 
  geom_col() + 
  geom_text(mapping = aes(label = addPercent(Value)),
            # color = "white", 
            vjust = -.5) +
  scale_y_continuous(limits = c(0, 100),
                     labels = addPercent) + 
  theme_minimal() +
  labs(title = plot_title,
       x = "",
       y = "") + 
  scale_fill_manual(values = color_palette) + 
  scale_color_manual(values = color_palette) + 
  theme(
    strip.background = element_blank(), 
    strip.text = element_textbox(
      size = 12,
      color = "white", fill = color_palette[2], box.color = color_palette[2], #4A618C
      halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
      padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
    ), 
    panel.grid = element_blank(),
    legend.position = "bottom", 
    legend.direction = "horizontal",
    plot.title.position = "plot",
    plot.background = element_rect(fill = bg_color, colour = bg_color),
    plot.title = element_textbox_simple(
      size = 13,
      lineheight = 1.5,
      padding = margin(4, 4, 4, 4),
      margin = margin(0, 0, 15, 0),
      fill = "#ffc7c6", 
      r = grid::unit(3, "pt"),
      box.color = "#ffc7c6",
      linetype = 1,
      color = text_color
    ),
    # axis.title.x = element_textbox_simple(
    #   width = NULL,
    #   padding = margin(3, 3, 3, 3),
    #   margin = margin(7, 0, 0, 0),
    #   linetype = 1,
    #   r = grid::unit(5, "pt"),
    #   fill = "#ffc7c6",
    #   box.color = "#ffc7c6",
    #   color = text_color
    # ),
    # axis.title.y = element_textbox_simple(
    #   width = NULL,
    #   # orientation = "left-rotated",
    #   padding = margin(3, 3, 3, 3),
    #   margin = margin(0, 7, 0, 0),
    #   linetype = 1,
    #   r = grid::unit(5, "pt"),
    #   fill = "#ffc7c6",
    #   box.color = "#ffc7c6",
    #   color = text_color
    # )
  ) + 
  guides(color = guide_legend(nrow = 1), 
         fill = guide_legend(nrow = 1))








