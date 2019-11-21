# Tim Roy
# 2019-11-20

# Canada Leader Wikipedia Trends Bar Chart Race

# install packages
pacman::p_load(
  wikipediatrend, # get wiki trends
  tidyverse, # data wrangling
  ggplot2, # plotting
  gganimate, # animating
  anchors # replacing values
)

#--------------------------------------#
#         Getting Wiki Searches        #
#--------------------------------------#

# Set party leaders for period since wiki has data
# English leader wiki pages
en_bq <- c("Yves-François_Blanchet",
           "Gilles_Duceppe",
           "Mario_Beaulieu", # En
           "Martine_Ouellet",
           "Rhéal_Fortin",
           "Daniel_Paillé",
           "Vivian_Barbot")
en_lib <- c("Justin_Trudeau",
            "Bob_Rae",
            "Michael_Ignatieff",
            "Stéphane_Dion",
            "Bill_Graham_(Canadian_politician)", # En
            "Paul_Martin",
            "Jean_Chrétien")
en_green <- c("Joan_Russow",
              "Chris_Bradshaw",
              "Jim_Harris_(politician)", # En
              "Elizabeth_May"#,
              #"Jo-Ann_Roberts"
              )
en_ndp <- c("Alexa_McDonough",
            "Jagmeet_Singh",
            "Tom_Mulcair", # En
            "Nycole_Turmel",
            "Jack_Layton")
cons <- c("Andrew_Scheer",
          "Rona_Ambrose",
          "Stephen_Harper")

# French leadrs
fr_bq <- c("Yves-François_Blanchet",
           "Gilles_Duceppe",
           "Mario_Beaulieu_(bloquiste)", #Fr
           "Martine_Ouellet",
           "Rhéal_Fortin",
           "Daniel_Paillé",
           "Vivian_Barbot")
fr_lib <- c("Justin_Trudeau",
            "Bob_Rae",
            "Michael_Ignatieff",
            "Stéphane_Dion",
            "Bill_Graham", # Fr
            "Paul_Martin",
            "Jean_Chrétien")
fr_green <- c(#"Joan_Russow",
              #"Chris_Bradshaw",
              "Jim_Harris", # Fr
              "Elizabeth_May"#,
              #"Jo-Ann_Roberts"
              )
fr_ndp <- c("Alexa_McDonough",
            "Jagmeet_Singh",
            "Thomas_Mulcair", # Fr
            "Nycole_Turmel",
            "Jack_Layton")

# Get wikitrends
# there is no data prior to December 2007

# English
en_trend <- wp_trend(
  page = c(en_bq, en_lib, en_green, en_ndp, cons),
  lang = "en", # set lang
  from = "2007-12-01",
  to = Sys.Date()
)
# French
fr_trend <- wp_trend(
  page = c(fr_bq, fr_lib, fr_green, fr_ndp, cons),
  lang = "fr", # set lang
  from = "2007-12-01",
  to = Sys.Date()
)

# --------------------------- #
# Prepare data for animations #
# --------------------------- #

# convert leader names to lower case to match en_trend$article
en_bq <- tolower(en_bq)
en_lib <- tolower(en_lib)
en_green <- tolower(en_green)
en_ndp <- tolower(en_ndp)
cons <- tolower(cons)

fr_bq <- tolower(fr_bq)
fr_lib <- tolower(fr_lib)
fr_green <- tolower(fr_green)
fr_ndp <- tolower(fr_ndp)

# get parties for each leader in English
en_trend$party <- ifelse((
  en_trend$article == cons[1]) | 
    (en_trend$article == cons[2]) | 
    (en_trend$article == cons[3]), "Conservative Party",
  ifelse(
    en_trend$article == en_bq[1] | 
      en_trend$article == en_bq[2] | 
      en_trend$article == en_bq[3] |
      en_trend$article == en_bq[4] |
      en_trend$article == en_bq[5] |
      en_trend$article == en_bq[6] |
      en_trend$article == en_bq[7] , "Bloc Québécois",
    ifelse(
      en_trend$article == en_lib[1] | 
        en_trend$article == en_lib[2] | 
        en_trend$article == en_lib[3] |
        en_trend$article == en_lib[4] |
        en_trend$article == en_lib[5] |
        en_trend$article == en_lib[6] |
        en_trend$article == en_lib[7] , "Liberal Party",
      ifelse(
        en_trend$article == en_green[1] | 
          en_trend$article == en_green[2] | 
          en_trend$article == en_green[3] |
          en_trend$article == en_green[4] , "Green Party",
        "New Democratic Party"
      ))))
# get parties for each leader in French
fr_trend$party <- ifelse((
  fr_trend$article == cons[1]) | 
    (fr_trend$article == cons[2]) | 
    (fr_trend$article == cons[3]), "Conservative Party",
  ifelse(
    fr_trend$article == fr_bq[1] | 
      fr_trend$article == fr_bq[2] | 
      fr_trend$article == fr_bq[3] |
      fr_trend$article == fr_bq[4] |
      fr_trend$article == fr_bq[5] |
      fr_trend$article == fr_bq[6] |
      fr_trend$article == fr_bq[7] , "Bloc Québécois",
    ifelse(
      fr_trend$article == fr_lib[1] | 
        fr_trend$article == fr_lib[2] | 
        fr_trend$article == fr_lib[3] |
        fr_trend$article == fr_lib[4] |
        fr_trend$article == fr_lib[5] |
        fr_trend$article == fr_lib[6] |
        fr_trend$article == fr_lib[7] , "Liberal Party",
      ifelse(
        fr_trend$article == fr_green[1] | 
          fr_trend$article == fr_green[2] , "Green Party",
        "New Democratic Party"
      ))))

# Get cumulative views variable (cum_views)
# each obs is the sum of all the prev obs plus itself
en_trend <- en_trend %>% 
  group_by(article) %>% 
  mutate(cum_views = cumsum(views))
fr_trend <- fr_trend %>% 
  group_by(article) %>% 
  mutate(cum_views = cumsum(views))
  
# Rank according to cumulative views for each day and keep top 10 for each day
en_trend <- en_trend %>%
  group_by(date) %>%
  arrange(-cum_views) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 10)
fr_trend <- fr_trend %>%
  group_by(date) %>%
  arrange(-cum_views) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 10)

# Create leaders var that looks good
# convert first letters in names to uppercase
# replace "_" by " "
unique(en_trend$article)
en_trend_fin <- en_trend
en_trend_fin <- replace.value(en_trend_fin, "article", "gilles_duceppe", "Gilles Duceppe")
en_trend_fin <- replace.value(en_trend_fin, "article", "bob_rae", "Bob Rae")
en_trend_fin <- replace.value(en_trend_fin, "article", "stéphane_dion", "Stéphane Dion")
en_trend_fin <- replace.value(en_trend_fin, "article", "jean_chrétien", "Jean Chrétien")
en_trend_fin <- replace.value(en_trend_fin, "article", "michael_ignatieff", "Michael Ignatieff")
en_trend_fin <- replace.value(en_trend_fin, "article", "paul_martin", "Paul Martin")
en_trend_fin <- replace.value(en_trend_fin, "article", "justin_trudeau", "Justin Trudeau")
en_trend_fin <- replace.value(en_trend_fin, "article", "elizabeth_may", "Elizabeth May")
en_trend_fin <- replace.value(en_trend_fin, "article", "jagmeet_singh", "Jagmeet Singh")
en_trend_fin <- replace.value(en_trend_fin, "article", "jack_layton", "Jack Layton")
en_trend_fin <- replace.value(en_trend_fin, "article", "andrew_scheer", "Andrew Scheer")
en_trend_fin <- replace.value(en_trend_fin, "article", "rona_ambrose", "Rona Ambrose")
en_trend_fin <- replace.value(en_trend_fin, "article", "stephen_harper", "Stephen Harper")
 
head(en_trend_fin); tail(en_trend_fin)

summary(en_trend_fin$cum_views)

unique(fr_trend$article)
fr_trend_fin <- fr_trend
fr_trend_fin <- replace.value(fr_trend_fin, "article", "gilles_duceppe", "Gilles Duceppe")
fr_trend_fin <- replace.value(fr_trend_fin, "article", "bob_rae", "Bob Rae")
fr_trend_fin <- replace.value(fr_trend_fin, "article", "stéphane_dion", "Stéphane Dion")
fr_trend_fin <- replace.value(fr_trend_fin, "article", "jean_chrétien", "Jean Chrétien")
fr_trend_fin <- replace.value(fr_trend_fin, "article", "michael_ignatieff", "Michael Ignatieff")
fr_trend_fin <- replace.value(fr_trend_fin, "article", "paul_martin", "Paul Martin")
fr_trend_fin <- replace.value(fr_trend_fin, "article", "justin_trudeau", "Justin Trudeau")
fr_trend_fin <- replace.value(fr_trend_fin, "article", "elizabeth_may", "Elizabeth May")
fr_trend_fin <- replace.value(fr_trend_fin, "article", "jagmeet_singh", "Jagmeet Singh")
fr_trend_fin <- replace.value(fr_trend_fin, "article", "jack_layton", "Jack Layton")
fr_trend_fin <- replace.value(fr_trend_fin, "article", "andrew_scheer", "Andrew Scheer")
fr_trend_fin <- replace.value(fr_trend_fin, "article", "rona_ambrose", "Rona Ambrose")
fr_trend_fin <- replace.value(fr_trend_fin, "article", "stephen_harper", "Stephen Harper")
fr_trend_fin <- replace.value(fr_trend_fin, "article", "alexa_mcdonough", "Alexa_McDonough")
fr_trend_fin <- replace.value(fr_trend_fin, "article", "andrew_scheer", "Andrew Scheer")             
fr_trend_fin <- replace.value(fr_trend_fin, "article",  "bill_graham", "Bill Graham")                                  
fr_trend_fin <- replace.value(fr_trend_fin, "article", "daniel_paillé", "Daniel Paillé")                         
fr_trend_fin <- replace.value(fr_trend_fin, "article",  "jim_harris",  "Jim Harris" )          
fr_trend_fin <- replace.value(fr_trend_fin, "article", "mario_beaulieu_(bloquiste)", "Mario Beaulieu")
fr_trend_fin <- replace.value(fr_trend_fin, "article", "martine_ouellet", "Martine Ouellet")           
fr_trend_fin <- replace.value(fr_trend_fin, "article", "nycole_turmel", "Nycole Turmel"    )         
fr_trend_fin <- replace.value(fr_trend_fin, "article", "rhéal_fortin", "Rhéal Fortin"    )          
fr_trend_fin <- replace.value(fr_trend_fin, "article", "stéphane_dion", "Stéphane Dion"  )           
fr_trend_fin <- replace.value(fr_trend_fin, "article", "thomas_mulcair", "Thomas Mulcair"   )         
fr_trend_fin <- replace.value(fr_trend_fin, "article", "vivian_barbot" , "Vivian Barbot"   )
fr_trend_fin <- replace.value(fr_trend_fin, "article", "yves-françois_blanchet", "Yves François Blanchet" )

head(fr_trend_fin); tail(fr_trend_fin)

summary(fr_trend_fin$cum_views)


# --------------------------- #
# Save plots for animation #
# --------------------------- #

require(hrbrthemes)

# Set party colours
cols <- c("Bloc Québécois"  = "blue",
          "Conservative Party" = "dodgerblue",
          "Green Party" = "green3",
          "Liberal Party" = "red",    
          "New Democratic Party" = "orange")

p_en <- en_trend_fin %>% 
  ggplot(aes(x= -rank, # order x-axis by rank
             y = cum_views,  # cumulative views of wiki page
             group = article # group by party leader
             )) +
  geom_tile(aes(y = cum_views/2,
                height = cum_views, 
                fill = party # colour by party colour
                ), width = 0.9) +
  geom_text(aes(label = article), # insert name of party leader 
            hjust = "right",  # move to the right
            colour = "black", 
            fontface = "bold", 
            nudge_y = -100000 # move text 100000 down Y-axis
            ) + 
  geom_text(aes(label = scales::comma(cum_views)),  
            hjust = "left", nudge_y = 100000, colour = "grey30"
            ) + # move text 100000 up Y-axis
  coord_flip(clip="off") + # lets text run off the left of the plot
  scale_fill_manual(name = 'Party', # name of legend 
                    values = cols) + # party color values
  scale_x_discrete("") + # Remove x-lab
  scale_y_continuous("",labels=scales::comma) + # reformats view numbers with commas
  hrbrthemes::theme_ipsum(plot_title_size = 32, 
                          subtitle_size = 24, 
                          caption_size = 20, 
                          base_size = 20) +
  theme(panel.grid.major.y=element_blank(), 
        panel.grid.minor.x=element_blank(),
        legend.position = c(0.4, 0.2),
        plot.margin = margin(1,1,1,2,"cm"),
        axis.text.y=element_blank()) + # Remove y-axis text
  # gganimate code to transition by date:
  transition_time(date) +
  ease_aes('cubic-in-out') +
  # Labels
  labs(title='English Wikipedia Page Views for Canadian Leaders',
       subtitle='Date: {round(frame_time,0)}',
       caption='Source: wikipediatrend
       timroy.me, @_timroy')

p_fr <- fr_trend_fin %>% 
  ggplot(aes(-rank, y = cum_views, group = article)) +
  geom_tile(aes(y = cum_views/2, height = cum_views, fill = party), width = 0.9) +
  geom_text(aes(label = article), 
            hjust = "right", colour = "black", 
            fontface = "bold", nudge_y = - 10000
  ) + # move text 100000 down Y-axis
  geom_text(aes(label = scales::comma(cum_views)),  
            hjust = "left", nudge_y = 10000, colour = "grey30"
  ) + # move text 100000 up Y-axis
  coord_flip(clip="off") + # lets text run off the left of the plot
  scale_fill_manual(name = 'Party', values = cols) +
  scale_x_discrete("") +
  scale_y_continuous("",labels=scales::comma) + # reformats view numbers with commas
  hrbrthemes::theme_ipsum(plot_title_size = 32, subtitle_size = 24, caption_size = 20, base_size = 20) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.position = c(0.4, 0.2),
        plot.margin = margin(1,1,1,2,"cm"),
        axis.text.y=element_blank()) +
  # gganimate code to transition by year:
  transition_time(date) +
  ease_aes('cubic-in-out') +
  labs(title='French Wikipedia Page Views for Canadian Leaders',
       subtitle='Date: {round(frame_time,0)}',
       caption='Source: wikipediatrend
       timroy.me, @_timroy')

# -------------- #
# Animate & Save #
# -------------- #
 
en_wiki_race <- animate(p_en, nframes = 750, fps = 25, end_pause = 100, width = 1200, height = 900)

anim_save("wiki_race_en.gif", en_wiki_race)

fr_wiki_race <- animate(p_fr, nframes = 750, fps = 25, end_pause = 100, width = 1200, height = 900)

anim_save("wiki_race_fr.gif", fr_wiki_race)
