################################################################################
#   SIMPSONS-ANALYSE                                                           #
#   ----------------                                                           #
#   A. INIT                                                                    #
#   B. FUNKTIONEN                                                              # 
#       - plot_tfidf                                                           #
#       - plot_tf                                                              #
#       - plot_senti_afinn                                                     #
#       - plot_senti_bing                                                      #
#       - plot_senti_nrc                                                       #
#   C. DATEN EINLESEN                                                          #
#   D. DATEN AUSWERTEN 1                                                       #
#       - Zuschauerzahlen, Bewertung + Co                                      #
#       - Charaktere: Anz gespr. Worte nach Episode, Geschlecht etc            #
#       - Orte: nach Anz gespr. Worte, Episode, Geschlecht etc                 #
#       - Autor*innen: Autor*innenschaft je Episode nach Geschlecht            #
#   E.  DATEN AUSWERTEN 2                                                      #
#       - Sprechverhalten (TFIDF): spez. Worte nach Ort, Charakter, Episoden   #
#       - Top 10 pos/neg (AFINN): Orte, Charaktere, Episoden                   #
#       - Emotionen (NRC): Orte, Charaktere, Episoden                          #
#       - Netzwerk:                                                            #
#           + Beziehung über Sprechakt: directed, undirected -> gephi-Output   #
#           + Modularitätscluster                                              #
#           + Sprechverhalten: nach Modularitätscluster                        #
#           + Emotionen: nach Modularitätscluster                              #
#       - Wortkonjunktur im Episodenverlauf                                    #
#                                                                              #
################################################################################

################################################################################
#   A. INIT                                                                    #
################################################################################

# Packages laden
library(magrittr)
library(dplyr)
library(ggplot2)
library(stringr)
library(igraph)
library(tm)
library(tidytext)
library(tidyr)
library(ggthemes)
library(RColorBrewer)
library(gridExtra)
library(tidygraph)
library(ggraph)

# Funktionen laden 
setwd("C:/Users/sambo/Dropbox/R/R Scripts")
source("format/to_class.r")
source("corpus/clean_corpus.R")

# Working Directory
setwd("C:/Users/sambo/Dropbox/R/R Projekte/kaggle/simpsons")

################################################################################
#   B. FUNKTIONEN                                                              #
################################################################################

# Plot: Bar-Chart Top n tfidf Worte je character/location/etc
plot_tfidf <- function(label_df=character.df,      
                       col="character_id",
                       label_col="normalized_name",
                       name_filter="homer simpson",   
                       top=10,
                       thema="Akteur*innen",
                       title_label=paste0("Sprachgebrauch nach ",thema,"\n Top ",top," Worte (TFIDF)"),
                       y_label="Term Frequency Inverse Document Frequency",
                       x_label=""){        
    words <- script.df %>%
        dplyr::select(one_of(col),normalized_text) %>%
        tidytext::unnest_tokens(word,normalized_text) %>%
        dplyr::anti_join(stop_words %>%
                             dplyr::bind_rows(data_frame(word=c("im","dont","youre","ill","ive","hes",
                                                                "didnt","id","youve","youll"),
                                                         lexicon="usr"))) %>%
        dplyr::count_(vars=c(paste(col),"word"),sort=T) %>%
        dplyr::ungroup()
    total_words <- words %>%
        dplyr::group_by_(col) %>%
        dplyr::summarize(total=sum(n))
    words <- dplyr::left_join(words,total_words)
    p <- words %>%
        tidytext::bind_tf_idf_(term_col="word",document_col=col,n_col="n") %>%
        dplyr::select(-total) %>%
        dplyr::arrange(desc(tf_idf)) %>%
        dplyr::left_join(label_df %>%
                             dplyr::select_("id",label_col) %>%
                             dplyr::mutate(id=as.numeric(id)),
                         by=setNames("id",col)) %>%
        dplyr::filter(grepl(name_filter,.[[label_col]])) %>%
        dplyr::group_by_(col) %>%
        dplyr::top_n(top,tf_idf) %>%
        dplyr::mutate_(label=label_col) %>%
        dplyr::ungroup() %>%
        dplyr::arrange_(col,"tf_idf") %>%
        dplyr::mutate(order=row_number())
    p %>% 
        ggplot2::ggplot(aes(y=tf_idf,x=order)) +
        ggplot2::geom_bar(stat="identity",fill="#FFD90F") +
        ggplot2::scale_x_continuous(breaks=p$order,
                                    labels=gsub("(.{19})", "\\1\n", p$word),
                                    expand=c(0,0)) +
        ggplot2::facet_wrap(~label,scales="free") +
        ggplot2::coord_flip() +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(title_label) +
        ggplot2::labs(x=x_label,y=y_label) +
        ggplot2::theme(plot.title=element_text(color="#666666",face="bold",size=18,hjust=0.5),
                       axis.text=element_text(color="#666666"),
                       axis.title=element_text(color="#666666"))
}


# Plot: Bar-Chart Top n tf Worte je character/location/etc
plot_tf <- function(label_df=character.df,      
                    col="character_id",          
                    name_filter="homer simpson",   
                    top=5,
                    title_label=paste0("Sprachgebrauch nach Akteur*innen\n Top ",top," Worte (TF)"),
                    y_label="Term Frequency",
                    x_label=""){        
    words <- script.df %>%
        dplyr::select(one_of(col),normalized_text) %>%
        tidytext::unnest_tokens(word,normalized_text) %>%
        dplyr::anti_join(stop_words %>%
                             dplyr::bind_rows(data_frame(word=c("im","dont","youre","ill","ive","hes",
                                                                "didnt","id","youve","youll"),
                                                         lexicon="usr"))) %>%
        dplyr::count_(vars=c(paste(col),"word"),sort=T) %>%
        dplyr::ungroup()
    total_words <- words %>%
        dplyr::group_by_(col) %>%
        dplyr::summarize(total=sum(n))
    words <- dplyr::left_join(words,total_words)
    p <- words %>%
        dplyr::select(-total) %>%
        dplyr::arrange(desc(n)) %>%
        dplyr::left_join(label_df %>%
                             dplyr::select(id,normalized_name) %>%
                             dplyr::mutate(id=as.numeric(id)),
                         by=setNames("id",col)) %>%
        dplyr::filter(grepl(name_filter,.$normalized_name)) %>%
        dplyr::group_by_(col) %>%
        dplyr::top_n(top,n) %>%
        dplyr::ungroup() %>%
        dplyr::arrange_(col,"n") %>%
        dplyr::mutate(order=row_number())
    p %>% 
        ggplot2::ggplot(aes(y=n,x=order)) +
        ggplot2::geom_bar(stat="identity",fill="#FFD90F") +
        ggplot2::scale_x_continuous(breaks=p$order,
                                    labels=gsub("(.{19})", "\\1\n", p$word),
                                    expand=c(0,0)) +
        ggplot2::facet_wrap(~normalized_name,scales="free") +
        ggplot2::coord_flip() +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(title_label) +
        ggplot2::labs(x=x_label,y=y_label) +
        ggplot2::theme(plot.title=element_text(color="#666666",face="bold",size=18,hjust=0.5),
                       axis.text=element_text(color="#666666"),
                       axis.title=element_text(color="#666666"))
}

# Plot Sentiment: Top-AFINN-Score
plot_senti_afinn <- function(label_df=character.df,      
                             col="character_id",          
                             name_filter="homer simpson",   
                             top=10,
                             w="wt",
                             label_col="normalized_name",
                             title_label=paste0("Sentiment-Analyse\n Top ",top," pos/neg")){  
    # wt = afinn-score je wort * häufigkeit für gesamte col 
    # wt_sc = normalisiertes wt
    x_label <- dplyr::case_when(col=="character_id" ~ "Charaktere",
                                col=="location_id" ~ "Orte",
                                col=="episode_id" ~ "Episoden")
    y_label <- dplyr::case_when(w=="wt" ~ "Afinn-Score",
                                w=="wt_sc" ~ "normalisierter Afinn-Score",
                                w=="n" ~ "Häufigkeit")
    words <- script.df %>%
        dplyr::select(one_of(col),normalized_text) %>%
        tidytext::unnest_tokens(word,normalized_text) %>%
        dplyr::anti_join(stop_words %>%
                             dplyr::bind_rows(data_frame(word=c("im","dont","youre","ill","ive","hes",
                                                                "didnt","id","youve","youll"),
                                                         lexicon="usr"))) %>%
        dplyr::count_(vars=c(paste(col),"word"),sort=T) %>%
        dplyr::ungroup()
    words %>%
        dplyr::left_join(label_df %>%
                             dplyr::select_("id",label_col) %>%
                             dplyr::mutate(id=as.numeric(id)),
                         by=setNames("id",col)) %>%
        dplyr::filter(grepl(name_filter,.[[label_col]])) %>%
        dplyr::inner_join(get_sentiments("afinn"),by=c("word"="word")) %>%
        dplyr::group_by_(col) %>%
        dplyr::summarise(wt=sum(score*n),n=sum(n)) %>%
        dplyr::mutate(wt_sc=as.numeric(scale(wt))) %>%
        dplyr::left_join(label_df %>% 
                             dplyr::select_("id",label_col) %>%
                             dplyr::mutate(id=as.numeric(id)),
                         by=setNames("id",col)) %>%
        dplyr::filter(grepl(name_filter, .[[label_col]])) %>%
        dplyr::mutate(grp=ifelse(wt>0,"pos","neg")) %>%
        dplyr::mutate_(label=label_col) %>%
        dplyr::group_by(grp) %>%
        dplyr::mutate_(top_var=w) %>%
        dplyr::top_n(top,abs(top_var)) %>%
        ggplot2::ggplot(aes(x=reorder(label,-top_var),y=top_var)) + 
        ggplot2::geom_col(fill="#FFD90F") +
        ggplot2::coord_flip() +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(title_label) +
        ggplot2::labs(x=x_label,y=y_label) +
        ggplot2::theme(plot.title=element_text(color="#666666",face="bold",size=18,hjust=0.5),
                       axis.text=element_text(color="#666666"),
                       axis.title=element_text(color="#666666"))
}

# Plot Sentiment: Top-Bing-Score
plot_senti_bing <- function(label_df=character.df,      
                            col="character_id",          
                            name_filter=".",   
                            top=10,
                            w="wt",
                            label_col="normalized_name",
                            title_label=paste0("Sentiment-Analyse\n Top ",top," pos/neg")){  
    x_label <- dplyr::case_when(col=="character_id" ~ "Charaktere",
                                col=="location_id" ~ "Orte",
                                col=="episode_id" ~ "Episoden")
    y_label <- dplyr::case_when(w=="wt" ~ "Bing-Score",
                                w=="wt_sc" ~ "normalisierter Bing-Score",
                                w=="n" ~ "Häufigkeit")
    words <- script.df %>%
        dplyr::select(one_of(col),normalized_text) %>%
        tidytext::unnest_tokens(word,normalized_text) %>%
        dplyr::anti_join(stop_words %>%
                             dplyr::bind_rows(data_frame(word=c("im","dont","youre","ill","ive","hes",
                                                                "didnt","id","youve","youll"),
                                                         lexicon="usr"))) %>%
        dplyr::count_(vars=c(paste(col),"word"),sort=T) %>%
        dplyr::ungroup()
    words %>%
        dplyr::left_join(label_df %>%
                             dplyr::select_("id",label_col) %>%
                             dplyr::mutate(id=as.numeric(id)),
                         by=setNames("id",col)) %>%
        dplyr::filter(grepl(name_filter,.[[label_col]])) %>%
        dplyr::inner_join(tidytext::get_sentiments("bing"),by=c("word"="word")) %>%
        dplyr::group_by_(col,"sentiment") %>%
        dplyr::summarise(n=sum(n)) %>%
        dplyr::ungroup() %>%
        tidyr::spread(sentiment,n,fill=0) %>%
        dplyr::mutate(wt=positive-negative) %>%
        dplyr::mutate(wt_sc=scale(wt)) %>%
        dplyr::left_join(label_df %>% 
                             dplyr::select_("id",label_col) %>%
                             dplyr::mutate(id=as.numeric(id)),
                         by=setNames("id",col)) %>%
        dplyr::filter(grepl(name_filter, .[[label_col]])) %>%
        dplyr::mutate_(top_var=w) %>%
        dplyr::mutate(grp=ifelse(top_var>0,"pos","neg")) %>%
        mutate(top_var2=ifelse(top_var>0,top_var,top_var*-1)) %>% # abs(top_var) führt bei wt_sc zu fehler
        dplyr::group_by(grp) %>%
        dplyr::top_n(top,top_var2) %>%
        dplyr::mutate_(label=label_col) %>%
        ggplot2::ggplot(aes(x=reorder(label,-top_var),y=top_var)) + 
        ggplot2::geom_col(fill="#FFD90F") +
        ggplot2::coord_flip() +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(title_label) +
        ggplot2::labs(x=x_label,y=y_label) +
        ggplot2::theme(plot.title=element_text(color="#666666",face="bold",size=18,hjust=0.5),
                       axis.text=element_text(color="#666666"),
                       axis.title=element_text(color="#666666"))
}

# Plot Sentiment: NRC
plot_senti_nrc <- function(label_df=character.df,      
                           col="character_id",          
                           name_filter=".", 
                           emo_filter=".",
                           min_words=1000,
                           top=10,
                           w="pc",
                           label_col="normalized_name",
                           title_label=paste0("Sentiment-Analyse\n Top ",top," pos/neg")){  
    # wt = afinn-score je wort * häufigkeit für gesamte col 
    # wt_sc = normalisiertes wt
    x_label <- dplyr::case_when(col=="character_id" ~ "Charaktere",
                                col=="location_id" ~ "Orte",
                                col=="episode_id" ~ "Episoden")
    y_label <- dplyr::case_when(w=="pc" ~ "Anteil der Emotion am gesamten Sperchakt",
                                w=="pc_sc" ~ "normalisierter Anteil der Emotion am gesamten Sperchakt",
                                w=="n" ~ "absolute Häufigkeit der Emotion",
                                w=="n_sc" ~ "normalisierte, absolute Häufigkeit der Emotion")
    colour_pal <- c("#FFD90F","#9c5b01","#2359F1","#83C33F","#DA6901",
                    "#A2232C","#70D1FF","#D1B270","#4D5357","#ff81c1")
    words <- script.df %>%
        dplyr::select(one_of(col),normalized_text) %>%
        tidytext::unnest_tokens(word,normalized_text) %>%
        dplyr::anti_join(stop_words %>%
                             dplyr::bind_rows(data_frame(word=c("im","dont","youre","ill","ive","hes",
                                                                "didnt","id","youve","youll"),
                                                         lexicon="usr"))) %>%
        dplyr::count_(vars=c(paste(col),"word"),sort=T) %>%
        dplyr::ungroup()
    p <- words %>%
        dplyr::left_join(label_df %>%
                             dplyr::select_("id",label_col) %>%
                             dplyr::mutate(id=as.numeric(id)),
                         by=setNames("id",col)) %>%
        dplyr::filter(grepl(name_filter,.[[label_col]])) %>%
        dplyr::group_by_(col) %>%
        dplyr::mutate(words_total=sum(n)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(words_total>=min_words) %>%
        dplyr::inner_join(tidytext::get_sentiments("nrc"),by=c("word"="word")) %>%
        dplyr::filter(grepl(emo_filter,.$sentiment)) %>%
        dplyr::group_by_(col,"sentiment") %>%
        dplyr::summarise(n=sum(n)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by_(col) %>%
        dplyr::mutate(pc=n/sum(n)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(grepl(emo_filter,.$sentiment)) %>%
        dplyr::mutate(n_sc=as.numeric(scale(n)),pc_sc=as.numeric(scale(pc))) %>%
        dplyr::group_by(sentiment) %>%
        dplyr::mutate_(top_var=w) %>%
        dplyr::top_n(top,top_var) %>%
        dplyr::left_join(label_df %>% 
                             dplyr::select_("id",label_col) %>%
                             dplyr::mutate(id=as.numeric(id)),
                         by=setNames("id",col)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(grepl(name_filter, .[[label_col]])) %>%
        dplyr::arrange(sentiment,top_var) %>%
        dplyr::mutate(order=row_number())
    p %>%
        ggplot2::ggplot(aes(x=order,y=top_var,fill=sentiment)) +
        ggplot2::geom_bar(stat="identity",position="stack") + 
        ggplot2::scale_x_continuous(breaks=p$order,
                                    labels=gsub("(.{19})", "\\1\n", p[[label_col]]),
                                    expand=c(0,0)) +
        ggplot2::scale_fill_manual(values=colour_pal[1:length(unique(p$sentiment))]) +
        ggplot2::facet_wrap(~sentiment,scales="free") + 
        ggplot2::coord_flip() +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(title_label) +
        ggplot2::labs(x=x_label,y=y_label) +
        ggplot2::guides(fill=FALSE) +
        ggplot2::theme(plot.title=element_text(color="#666666",face="bold",size=18,hjust=0.5),
                       axis.text=element_text(color="#666666"),
                       axis.title=element_text(color="#666666"))
}

################################################################################
#   C. DATEN EINLESEN                                                          #
################################################################################

character.df <- read.csv("data/simpsons_characters.csv",stringsAsFactors=F) %>%
    dplyr::as_data_frame()
location.df <- read.csv("data/simpsons_locations.csv",stringsAsFactors=F) %>% 
    dplyr::as_data_frame()
writer.df <- read.csv("data/simpsons_writers.csv",stringsAsFactors=F) %>%
    dplyr::as_data_frame()

#w_gender.df <- data_frame(writer=unique(writer.df$writer)) %>%
#    arrange(writer) %>%
#    mutate(gender=c("m"))

# Episoden einlesen
# dmmy <- readLines("data/simpsons_episodes.csv")[-1]    # header löschen
# match.list <- c("^[0-9]*?",                       # id
#                 ".*?",                            # title
#                 "\\d{4}-\\d{2}-\\d{2}",           # original_air_date
#                 ".*?",                            # production_code
#                 "[0-9]*?",                        # season
#                 "[0-9]*?",                        # number_in_season
#                 "[0-9]*?",                        # number_in_series
#                 ".*?",                            # us_viewers_in_millions
#                 "[0-9]*?",                        # views
#                 ".*?",                            # imdb_rating
#                 ".*?",                            # imdb_votes
#                 ".*?",                            # img_url
#                 ".*$")                            # video_url
# 
# # data.frame spaltenweise befüllen/Zeile splitten
# match <- 1:length(dmmy)
# for (i in 1:length(match.list)){
#     start <- paste(match.list[1:(i-1)],collapse=",")
#     end <- paste(match.list[(i+1):length(match.list)],collapse=",")
#     
#     start <- ifelse(i==1,"",paste0("(?:",start,",)")) # non-capturing group
#     cap <- paste0("(",match.list[i],")")               # capturing group
#     end <- ifelse(i==length(match.list),"",            # non-capturing group
#                   paste0("(?:,",end,")"))   
#     
#     match <- cbind(match,str_match(dmmy,paste0(start,cap,end))[,2])
# }
# episodes.df <- dplyr::as_data_frame(match) %>%
#     magrittr::set_colnames(c("row","id","title","original_air_date","production_code",
#                    "season","number_in_season","number_in_series",
#                    "us_viewers_in_millions","views","imdb_rating",
#                    "imdb_votes","img_url","video_url"))

# Script einlesen
# hohe Qualität: 
#   row, id, episode_id, number, raw_text, timestamp_in_ms, speaking_line,
#   character_id, location_id
# mittlere Qualität
#   normalized_text, word_count
# schlechte Qualität
#   raw_character_text, raw_location_text, spoken_words

# dmmy <- readLines("data/simpsons_script_lines.csv")[-1] # header löschen
# match.list <- c("^[0-9]*?",                        # id
#                 "[0-9]*?",                         # episode_id
#                 "[0-9]*?",                         # number
#                 ".*?",                             # raw_text
#                 "[0-9]*?",                         # timestamp_in_ms
#                 "\\w*",                            # speaking_line
#                 "[0-9]*?",                         # character_id
#                 "[0-9]*?",                         # location_id
#                 ".*?",                             # raw_character_text
#                 ".*?",                             # raw_location_text
#                 ".*",                              # spoken_words
#                 ".*?",                             # normalized_text
#                 "[0-9]*$")                         # word_count
# 
# match <- 1:length(dmmy)
# for (i in 1:length(match.list)){
#     start <- paste(match.list[1:(i-1)],collapse=",")
#     end <- paste(match.list[(i+1):length(match.list)],collapse=",")
#     
#     start <- ifelse(i==1,"",paste0("(?:",start,",)")) # non-capturing group
#     cap <- paste0("(",match.list[i],")")               # capturing group
#     end <- ifelse(i==length(match.list),"",            # non-capturing group
#                   paste0("(?:,",end,")"))   
#     
#     match <- cbind(match,str_match(dmmy,paste0(start,cap,end))[,2])
# }
# script.df <- dplyr::as_data_frame(match) %>%
#     magrittr::set_colnames(c("row","id","episode_id","number","raw_text",
#                              "timestamp_in_ms","speaking_line","character_id",
#                              "location_id","raw_character_text","raw_location_text",
#                              "spoken_words","normalized_text","word_count"))

# Formate anpassen
# cols <- colnames(episodes.df)
# episodes.df <- dplyr::as_data_frame(episodes.df) %>%
#     mutate_each_(funs(as.numeric),cols[c(1,2,6:12)]) %>%
#     mutate_each_(funs(as.Date),cols[4])
# 
# cols <- colnames(script.df)
# script.df <- dplyr::as_data_frame(script.df) %>%
#     mutate_each_(funs(as.numeric),cols[c(1:4,6,8,9)]) %>%
#     mutate_each_(funs(as.logical),cols[7]) %>% 
#     select(row:location_id,normalized_text) %>%
#     mutate(word_count=str_count(normalized_text,"\\S+"))

#save(episodes.df,file="data/episodesdf.RData")
load(file="data/episodesdf.RData")
#save(script.df,file="data/scriptdf.RData")
load(file="data/scriptdf.RData")

################################################################################
#   D. DATEN AUSWERTEN - SIMPSONS PART I                                       #
################################################################################
#   Farben
#   gelb: FFD90F
#   blau: 1791FF
#   braun: D1B270
#   grün: D1FF87

# D.0. Zuschauerzahlen, allg. Zuschauerzahlen, Bewertung 
episodes.df %>%
    select(id,title,date=original_air_date,viewers=us_viewers_in_millions,
           views,rating=imdb_rating) %>%
    mutate(val=NA) %>%
    filter(is.na(id)) %>%
    bind_rows(episodes.df %>%
                  select(id,title,date=original_air_date,us_viewers_in_millions) %>%
                  mutate(val=as.numeric(us_viewers_in_millions), group=1) %>%
                  mutate(val=scale(val))) %>%
    bind_rows(episodes.df %>%
                  select(id,title,date=original_air_date,imdb_rating) %>%
                  mutate(val=as.numeric(imdb_rating), group=2) %>%
                  mutate(val=scale(val))) %>%
    select(id,title,date,val,group) %>%
    ggplot(aes(x=as.Date(date),y=val)) +
    geom_line(colour="#666666") +
    geom_smooth(colour="#FFD90F",fill="#1791FF") +
    facet_wrap(~factor(group,labels=c("US Viewers in Millions",
                                      "IMDB Rating")),
               scales="free",nrow=2,ncol=1) +
    theme_bw() +
    ggtitle("Zuschauer*innen und Rating im Verlauf") +
    labs(x="", y="standardisierte Werte") +
    guides(fill=guide_legend(title="")) +
    theme(plot.title=element_text(color="#666666",
                                  face="bold",
                                  size=18,
                                  hjust=0.5),
          axis.title=element_text(color="#666666"),
          axis.text=element_text(color="#666666"),
          legend.text=element_text(color="#666666"),
          legend.title=element_text(color="#666666"),
          legend.position="right")


x <- episodes.df %>%
    select(id,viewers=us_viewers_in_millions,views,rating=imdb_rating) %>%
    filter(!is.na(id))

summary(lm(data=x,viewers~rating))


# season
episodes.df %>%
    select(id,season,date=original_air_date,viewers=us_viewers_in_millions) %>%
    mutate(val=as.numeric(viewers),season=as.numeric(season)) %>%
    group_by(season) %>%
    summarise(val=sum(val,na.rm=T),date=max(date)) %>%
    mutate(val=scale(val)) %>%
    ggplot(aes(x=season,y=val)) +
    geom_line(colour="#666666") +
    geom_smooth(colour="#FFD90F",fill="#1791FF") +
    theme_bw() +
    ggtitle("Zuschauer*innen und Rating im Verlauf") +
    labs(x="", y="standardisierte Werte") +
    scale_x_continuous(breaks=seq(1,28,2)) +
    guides(fill=guide_legend(title="")) +
    theme(plot.title=element_text(color="#666666",
                                  face="bold",
                                  size=18,
                                  hjust=0.5),
          axis.title=element_text(color="#666666"),
          axis.text=element_text(color="#666666"),
          legend.text=element_text(color="#666666"),
          legend.title=element_text(color="#666666"),
          legend.position="right")

# schlechteste/beste Folgen

episodes.df %>%
    select(id,title,date=original_air_date,viewers=us_viewers_in_millions,
           views,rating=imdb_rating) %>%
    mutate(val=NA) %>%
    filter(is.na(id)) %>%
    bind_rows(episodes.df %>%
                  select(id,title,date=original_air_date,us_viewers_in_millions) %>%
                  mutate(val=as.numeric(us_viewers_in_millions), group=1) %>%
                  mutate(val=scale(val))) %>%
    bind_rows(episodes.df %>%
                  select(id,title,date=original_air_date,imdb_rating) %>%
                  mutate(val=as.numeric(imdb_rating), group=2) %>%
                  mutate(val=scale(val))) %>%
    select(id,title,date,val,group) %>%
    filter(group==2) %>%
    top_n(5,-val) %>%
    arrange(val)
    
# D.1. Charaktere nach Anzahl gesprochener Worte 
char_filter <- c("homer simpson","marge simpson","lisa simpson","bart simpson")
words.char <- script.df %>%
    select(character_id,episode_id,word_count) %>%
    group_by(character_id) %>%
    mutate(word_ct=sum(word_count),
           ep_ct=length(unique(episode_id)),
           word_ep_ct=sum(word_count)/length(unique(episode_id)),
           act_ct=n(),
           act_ep_ct=n()/length(unique(episode_id))) %>%
    ungroup() %>%
    select(-episode_id,-word_count) %>%
    filter(!duplicated(character_id) & !is.na(character_id)) %>%
    arrange(-word_ct) %>%
    left_join(character.df %>%
                  select(id,normalized_name,gender),
              by=c("character_id"="id")) %>%
    filter(gender!="")

# Plot: Anz. Worte alle Charaktere
# erste Frau (Nebenrollen): Edna Krabappel-Flanders (Platz 15)
words.char %>%
    #filter(!(normalized_name %in% char_filter)) %>%
    top_n(10,word_ct) %>%
    ggplot(aes(x=reorder(normalized_name,word_ct),y=word_ct)) +
    geom_col(aes(fill=factor(gender,labels=c("weiblich","männlich")))) +
    #facet_grid(factor(gender,levels=c("m","f"))~.,scales="free") +
    scale_fill_manual(values=c("#1791FF","#FFD90F")) +
    coord_flip() +
    ggtitle("Anzahl gesprochener Worte\nTop 10 Charaktere") +
    scale_y_continuous(limits=c(0,max(words.char$word_ct+25000)),
                       breaks=seq(0,max(words.char$word_ct)+25000,25000),
                       labels=format(seq(0,max(words.char$word_ct)+25000,25000),
                                     big.mark=".", small.mark=".",decimal.mark=",",
                                     nsmall=0)) +
    theme_bw() +
    guides(fill=guide_legend(title="")) +
    theme(plot.title=element_text(color="#666666",
                                  face="bold",
                                  size=18,
                                  hjust=0.5),
          axis.title=element_blank(),
          axis.text=element_text(color="#666666"),
          legend.text=element_text(color="#666666"),
          legend.title=element_text(color="#666666"),
          legend.position="right")

# Plot: Simspons-Sperchakte
words.char %>%
    mutate(flag=ifelse(normalized_name %in% char_filter,1,2)) %>%
    group_by(flag,gender) %>%
    summarize(word_ct=sum(word_ct)) %>%
    ungroup() %>%
    mutate(word_pc=word_ct/sum(word_ct)) %>%
    bind_rows(words.char %>% 
                  group_by(gender) %>%
                  summarize(word_ct=sum(word_ct)) %>%
                  mutate(flag=0,word_pc=word_ct/sum(word_ct))) %>%
    ggplot(aes(x=flag,y=word_pc,fill=gender)) +
    geom_bar(stat="identity") +
    scale_fill_manual(labels=c("weiblich","männlich"),values=c("#1791FF","#FFD90F")) +
    scale_x_continuous(breaks=0:2,labels=c("Gesamt","Simpsons","Andere")) +
    scale_y_continuous(limits=c(0,1),
                       breaks=seq(0,1,0.1),
                       labels=paste0(seq(0,100,10),"%")) +
    theme_bw() +
    ggtitle("Anteil gesprochener Worte") +
    guides(fill=guide_legend(title="")) +
    theme(plot.title=element_text(color="#666666",
                                  face="bold",
                                  size=18,
                                  hjust=0.5),
          axis.title=element_blank(),
          axis.text=element_text(color="#666666"),
          legend.text=element_text(color="#666666"),
          legend.title=element_text(color="#666666"),
          legend.position="left")
    

# Plot: Anz. Worte x Geschlecht Nebencharaktere
words.char <- words.char %>%
    filter(!(normalized_name %in% char_filter))

words.char %>%
    group_by(gender) %>% 
    top_n(10,word_ct) %>%
    ungroup() %>%
    ggplot(aes(x=reorder(normalized_name,word_ct),y=word_ct)) +
    geom_col(aes(fill=gender)) +
    facet_grid(factor(gender,levels=c("m","f"))~.,scales="free") +
    scale_fill_manual(values=c("#1791FF","#FFD90F")) +
    coord_flip() +
    ggtitle("Gesprochene Worte nach Geschlecht\nNebenrollen") +
    scale_y_continuous(limits=c(0,max(words.char$word_ct)+5000),
                       breaks=seq(0,max(words.char$word_ct)+5000,5000),
                       labels=format(seq(0,max(words.char$word_ct)+5000,5000),
                                     big.mark=".", small.mark=".",decimal.mark=",",
                                     nsmall=0)) +
    theme_bw() +
    guides(fill=F) +
    theme(plot.title=element_text(color="#666666",
                                  face="bold",
                                  size=18,
                                  hjust=0.5),
          axis.title=element_blank(),
          axis.text=element_text(color="#666666"))

# D.2. Anzahl Worte x Anzahl Episoden x Geschlecht
words.char <- script.df %>%
    select(character_id,episode_id,word_count) %>%
    group_by(character_id) %>%
    mutate(word_ct=sum(word_count,na.rm=T),
           ep_count=length(unique(episode_id)),
           word_ep_ct=sum(word_count,na.rm=T)/length(unique(episode_id)),
           act_ct=n(),
           act_ep_ct=n()/length(unique(episode_id))) %>%
    ungroup() %>%
    mutate(ep_max=length(unique(episode_id))) %>%
    left_join(character.df %>%
                  select(id,normalized_name,gender),
              by=c("character_id"="id")) %>%
    filter(gender!="" & !is.na(character_id) & !(normalized_name %in% char_filter))  %>%
    select(-episode_id,-word_count) %>%
    unique() %>%
    top_n(50,word_ct) 

# Plot: abw Sendungen x Abw Worte/Sendung x Anz Worte
words.char %>%
    #filter(normalized_name!="herb" & normalized_name!="sideshow bob") %>%
    mutate(ep_sc=scale(ep_count),
           word_sc=scale(log10(word_ep_ct))) %>% 
    ggplot(aes(x=ep_sc,y=word_sc,size=word_ct)) +
    geom_point(aes(colour=gender)) +
    geom_hline(aes(yintercept=0),linetype="dashed",alpha=0.3) + 
    geom_vline(aes(xintercept=0),linetype="dashed",alpha=0.3) +
    scale_colour_manual(name="Geschlecht",labels=c("weiblich","männlich"),
                        values=c("#1791FF","#FFD90F")) +
    scale_size(name="Wortanzahl",breaks=c(10000,20000,30000),labels=c("10.000","20.000","30.000")) +
    theme_bw() + 
    theme(legend.position="top") +
    ggtitle("Feld relevanter Akteur*innen\nTop 50 Nebenrollen\n") +
    labs(x="Abw. Episoden Ø",y="Abw. Ø Worte pro Episode") +
    # scale_y_continuous(limits=c(-1,4),
    #                    breaks=seq(-1,4,1),
    #                    labels=seq(-1,4,1)) +
    # scale_x_continuous(limits=c(-2,3),
    #                   breaks=seq(-2,3,1),
    #                   labels=seq(-2,3,1)) +
    geom_text(aes(label=normalized_name),
              show.legend=F,
              hjust=0, 
              vjust=1.5,
              size=2,
              colour="#666666") +
    theme(plot.title=element_text(color="#666666",face="bold",size=18,hjust=0.5),
          axis.title=element_text(color="#666666"),
          axis.text=element_text(color="#666666"),
          legend.text=element_text(color="#666666"),
          legend.title=element_text(color="#666666"),
          legend.position="right") +
    coord_flip()

# D.3. Worte x Geschlecht und Charaktere x Geschlecht 
gender <- script.df %>%
    select(character_id,word_count) %>%
    left_join(character.df %>%
                  select(id,gender),
              by=c("character_id"="id")) %>%
    filter(!is.na(gender) & gender != "") %>%
    group_by(gender) %>%
    mutate(word_count=sum(word_count),char_count=length(unique(character_id))) %>%
    ungroup() %>%
    select(-character_id) %>%
    unique() %>%
    mutate(word_count=word_count/sum(word_count),char_count=char_count/sum(char_count))

gender2 <- script.df %>%
    select(character_id,word_count) %>%
    left_join(character.df %>%
                  select(id,gender,normalized_name),
              by=c("character_id"="id")) %>%
    filter(!is.na(gender) & gender != "") %>%
    filter(!(normalized_name %in% char_filter)) %>%
    group_by(gender) %>%
    mutate(word_count=sum(word_count),char_count=length(unique(character_id))) %>%
    ungroup() %>%
    select(-character_id,-normalized_name) %>%
    unique() %>%
    mutate(word_count=word_count/sum(word_count),char_count=char_count/sum(char_count))

# Plot: Geschlecht x Worte und Charaktere
ggplot(gender) +
    geom_bar(aes(x=1,y=word_count),position="stack",stat="identity",width=0.9,alpha=0) +
    geom_bar(data=gender,aes(x=2,y=char_count,fill=factor(gender,labels=c("weiblich","männlich"))),
                             position="stack",stat="identity",width=0.9) +
    geom_text(aes(label = "Charaktere x\nGeschlecht",x=2,y=0.5), color="#666666",size = 3) +
    geom_bar(data=gender,aes(x=3,y=word_count,fill=factor(gender,labels=c("weiblich","männlich"))),
             position="stack",stat="identity", width=0.9) +
    geom_text(aes(label = "Worte x Geschlecht\n(alle Charaktere)",x=3,y=0.5),color="#666666",size = 3) +
    geom_bar(data=gender2, aes(x=4,y=word_count,fill=factor(gender,labels=c("weiblich","männlich"))),
             position="stack",stat="identity") +
    geom_text(aes(label = "Worte x Geschlecht\n(Nebencharaktere)",x=4,y=0.5),color="#666666",size = 3) +
    # geom_bar(data=gender2, aes(x=6,y=char_count,fill=factor(gender,labels=c("weiblich","männlich"))),
    #          position="stack",stat="identity") +
    scale_fill_manual(values=c("#1791FF","#FFD90F")) + 
    scale_y_continuous(labels=c("0","25 %","50 %","75 %","100 %")) +
    theme(panel.grid=element_blank(),axis.title=element_blank(), legend.position="right",
          axis.ticks=element_blank(), axis.text.y = element_blank()) +
    coord_polar("y") +
    theme_bw() + 
    guides(fill=guide_legend(title="")) +
    ggtitle("Charaktere und gesprochene Worte nach Geschlecht") +
    theme(plot.title=element_text(color="#666666",face="bold",size=18,hjust=0.5),
          legend.title=element_text(color="#666666"),
          legend.text=element_text(color="#666666"),
          axis.text=element_text(color="#666666"),
          axis.text.x=element_text(color="#666666"),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          legend.position="right")

# D.4. Charaktere je Episode nach Geschlecht
gender <- script.df %>%
    select(character_id,episode_id) %>%
    left_join(character.df %>%
                  select(id,gender),
              by=c("character_id"="id")) %>%
    filter(!is.na(gender) & gender != "") %>%
    group_by(episode_id,gender) %>%
    mutate(g_ct=length(unique(character_id))) %>%
    ungroup() %>%
    select(-character_id) %>%
    unique() %>%
    group_by(episode_id) %>%
    mutate(g_pc=g_ct/sum(g_ct)) %>%
    ungroup()

# Plot: Charaktere je Episode (f)
gender %>%
    filter(gender=="f") %>%
    ggplot(aes(episode_id,g_pc,fill=gender)) +
    geom_bar(stat="identity",position="stack") +
    geom_smooth(fill="#FFD90F") +
    #facet_grid(.~gender) +
    scale_fill_manual(values=c("#1791FF")) +
    scale_y_continuous(breaks=seq(0,1,0.1),labels=seq(0,100,10)) +
    scale_x_continuous(breaks=seq(1,max(gender$episode_id),20)) +
    ggtitle("Anteil weiblicher Charaktere je Episode\nalle Charaktere") +
    labs(x="Episode",y="Anteil") +
    theme_bw() + 
    guides(fill=F) +
    theme(plot.title=element_text(color="#666666",face="bold",size=18,hjust=0.5),
          axis.text=element_text(color="#666666"),
          axis.title=element_text(color="#666666"))

x <- gender %>%
    filter(gender=="f")

summary(lm(data=x,g_pc~episode_id))
lm(data=x,g_pc~episode_id)

# Episoden mit hohem Anteil weiblicher Charaktere
gender %>%
    left_join(episodes.df %>%
                  select(id,title,original_air_date,imdb_rating),
              by=c("episode_id"="id")) %>%
    filter(gender=="f") %>%
    arrange(-g_pc) %>%
    top_n(10,-g_pc)


# location nach genderanteil
loc <- script.df %>%
    select(character_id,location_id) %>%
    unique() %>%
    left_join(character.df %>%
                  select(id,gender),
              by=c("character_id"="id")) %>%
    left_join(location.df %>%
                  select(id,normalized_name),
              by=c("location_id"="id")) %>%
    filter(!is.na(character_id) & gender!="" & !is.na(location_id)) %>%
    group_by(location_id,gender) %>%
    mutate(g_ct=n()) %>% 
    ungroup() %>% 
    select(-character_id) %>%
    group_by(location_id) %>%
    mutate(ct=n(),g_pc=g_ct/n()) %>%
    ungroup() %>%
    unique() %>%
    group_by(gender) %>%
    mutate(g_diff=as.numeric(scale(g_pc))) %>%
    mutate(flag=ifelse(g_diff>0,1,0)) %>%
    ungroup() %>%
    filter(gender=="f" & ct>10) 

loc %>%
    group_by(gender,flag) %>%
    top_n(5,abs(g_diff)) %>%
    ungroup() %>%
    ggplot(aes(x=reorder(normalized_name,g_diff),y=g_diff,
               fill=factor(flag,labels=c("männlich","weiblich")))) +
    geom_bar(stat="identity",position="stack") +
    scale_fill_manual(values=c("#FFD90F","#1791FF")) +
    geom_text(aes(label=round(g_diff,2)),hjust=0.5,size=3.5,color="#666666") +
    #facet_grid(gender~.,scales="free") +
    coord_flip() +
    theme_bw() +
    ggtitle("Orte des Geschlechts\nalle Charaktere") +
    labs(x="",y="Abweichung vom Durchschnitt") +
    theme(plot.title=element_text(color="#666666",face="bold",size=18,hjust=0.5),
          axis.text=element_text(color="#666666"),
          axis.title=element_text(color="#666666")) +
    guides(fill=guide_legend(title=""))

# D.5. Anteil weibl. Charaktere nach Autorinnenateil je Episode

epg.df <- episodes.df %>%
    left_join(writer.df,by=c("production_code"="episode_code")) %>%
    filter(!is.na(gender)) %>%
    group_by(id,title,original_air_date,gender) %>%
    summarise(wg_ct=n()) %>%
    ungroup() %>%
    group_by(id) %>%
    mutate(wg_pc=wg_ct/sum(wg_ct),w_sum=sum(wg_ct)) %>%
    ungroup() %>%
    filter(gender=="f")

words.char <- script.df %>%
    left_join(character.df %>%
                  select(id,gender),
              by=c("character_id"="id")) %>%
    filter(!is.na(gender) & gender!="") %>%
    group_by(episode_id,gender) %>%
    summarise(cg_ct=n()) %>%
    ungroup() %>%
    group_by(episode_id) %>%
    mutate(cg_pc=cg_ct/sum(cg_ct),c_sum=sum(cg_ct)) %>%
    ungroup() %>%
    filter(gender=="f")

n <- data_frame(id=unique(words.char$episode_id)) %>%
    left_join(epg.df %>%
                  select(id,wg_pc,w_sum),
              by=c("id"="id")) %>%
    left_join(words.char %>%
                  select(episode_id,cg_pc,c_sum),
              by=c("id"="episode_id")) %>%
    mutate(wg_pc=ifelse(is.na(wg_pc),0,wg_pc),
           w_sum=ifelse(is.na(w_sum),0,w_sum),
           f_flag=ifelse(wg_pc>0,1,0)) 

# D.6. Orte nach Wortanteil

script.df %>%
    group_by(location_id) %>%
    summarise(wc=sum(word_count)) %>%
    left_join(location.df %>%
                  select(id,normalized_name) %>%
                  mutate(id=as.numeric(id)),
              by=c("location_id"="id")) %>%
    filter(wc!=0)  %>%
    #filter(!grepl("simpson",normalized_name)) %>%
    top_n(20,wc) %>%
    ggplot(aes(y=wc,x=reorder(normalized_name,wc))) +
    geom_col() +
    coord_flip()

# loc x gen correspondence-analysis
library(reshape2)

loc <- script.df %>%
    select(character_id,location_id) %>%
    unique() %>%
    left_join(character.df %>%
                  select(id,gender),
              by=c("character_id"="id")) %>%
    left_join(location.df %>%
                  select(id,normalized_name),
              by=c("location_id"="id")) %>%
    filter(!is.na(character_id) & gender!="" & !is.na(location_id)) %>%
    group_by(location_id,gender) %>%
    mutate(g_ct=n()) %>%
    ungroup() %>%
    group_by(location_id) %>%
    mutate(ct=n()) %>%
    ungroup() %>%
    select(-character_id) %>%
    unique() %>%
    top_n(100,ct) %>%
    select(-ct,-location_id) %>%
    dcast(normalized_name~gender)
loc$f[is.na(loc$f)] <- 0
loc$m[is.na(loc$m)] <- 0

loc %>%
    mutate(f1=f/(f+m),m1=m/(f+m)) %>%
    mutate(f=f1,m=m1) %>%
    top_n(10,f) %>%
    #mutate(n=m+f) %>%
    #top_n(100,n) %>%
    #top_n(-50,m) %>%
    mutate(avg_f=sum(f)/n(),avg_m=sum(m)/n(),avg_col=(f+m)/2,avg=(sum(f)+sum(m))/(2*n())) %>%
    mutate(exp_f=avg_f*avg_col/avg,exp_m=avg_m*avg_col/avg) %>%
    mutate(res_f=f-exp_f,res_m=m-exp_m) %>%
    ggplot(aes(x=res_f,y=res_m)) + 
    geom_point() +
    geom_hline(yintercept = 0, colour="grey") + 
    geom_vline(xintercept = 0, colour="grey") +
    geom_text(aes(label=normalized_name), size=3, colour="#666666")

################################################################################
#   E. DATEN AUSWERTEN - SIMPSONS PART II                                      #
################################################################################

# Ausgewählte Orte
filter <- paste("^duff brewery$","^duff brewery grounds$","^oval office$",
                "^krustylu studios$","^aztec theater$","^spaceship$",sep="|")
plot_tfidf(label_df=location.df,
           col="location_id",
           name_filter=filter,
           top = 10,
           thema = "Orten")

# Bedtimestories
filter <- "bedroom"
filter <- paste("^marge bedroom$","^homer bedroom$","^simpson master bedroom$",
                "^bedroom$","^master bedroom$","^patty and selma bedroom$",
                "^milhouse bedroom$","^flanders bedroom$","^flanders master bedroom$",
                "^rod and todd bedroom$","^lovejoy bedroom$","manjula bedroom",
                "^lenny bedroom$","^wolfcastle bedroom$",sep="|")
plot_tfidf(label_df=location.df,
           col="location_id",
           name_filter=filter,
           top = 10,
           thema = "Orten")

# Sprachlich-nationale Stereotypen
filter <- paste("^african city$","^china$","^chinatown street$", 
                "^eighteenth-century austrian palace$","^great wall of china$",
                "^italy$","^kremlin$","^paris$","^rome$","^tokyo$",
                "^vienna opera house$",sep="|")
plot_tfidf(label_df=location.df,
           col="location_id",
           name_filter=filter,
           top = 10,
           thema = "Orten")

# Prisonbreak:
# recht viele Gefängnisse  ^^
plot_tfidf(label_df=location.df,
           col="location_id",
           name_filter="prison",
           top = 10,
           thema = "Orten")

# Die Simpsons im Sprachvergleich
filter <- paste("^bart simpson$","^homer simpson$", "^maggie simpson$", 
                "^lisa simpson$", "^sideshow bob$","^c montgomery burns$",
                "^selma bouvier$","^patty bouvier$","^barney gumble$",
                "^marge simpson$",sep="|")
plot_tfidf(label_df=character.df,
           col="character_id",
           name_filter=filter,
           top = 10)

# Top-Pos/Neg-Folgen im Sprachgebrauch
filter <- paste("^Lisa the Greek$","^Itchy & Scratchy Land$","Mad, Mad",sep="|")
plot_tfidf(label_df=episodes.df,
           col="episode_id",
           name_filter=filter,
           label_col="title",
           top = 10,
           thema="Episoden")

# Haloween-Folgen?
plot_tfidf(label_df=episodes.df,
           col="episode_id",
           name_filter="^Treehouse of Horror",
           label_col="title",
           top = 10)

# pos/neg orte top 10
plot_senti_afinn(label_df=location.df,
                 col="location_id",
                 name_filter=".",
                 top=10,
                 w="wt_sc",
                 label_col="normalized_name")

# pos/neg char top 10
plot_senti_afinn(label_df=character.df,
                 col="character_id",
                 name_filter=".",
                 top=10,
                 w="wt_sc",
                 label_col="normalized_name")

# pos/neg episodes top 10
plot_senti_afinn(label_df=episodes.df,
                col="episode_id",
                name_filter=".",
                top=10,
                w="wt_sc",
                label_col="title")

# plot nrc
emo <- paste("anger","anticipation","disgust","fear","joy","sadness",
         "surprise","trust",sep="|")
plot_senti_nrc(label_df=episodes.df,
               col="episode_id",
               name_filter=".",
               min_words=5,
               top=5,
               w="pc",
               label_col="title",
               emo_filter=emo)

filter <- paste("^bart simpson$","^homer simpson$", "^maggie simpson$", "^marge simpson$",
                "^lisa simpson$", "^grmpa simpson$","^mona simpson$", "^sideshow bob$",
                "^c montgomery burns$","^selma bouvier$","^patty bouvier$",
                "^moe szyslak$","^barney gumble$","^ned flanders$",sep="|")
emo <- paste("anger","anticipation","disgust","fear","joy","sadness",
             "surprise","trust",sep="|")
plot_senti_nrc(label_df=character.df,
               col="character_id",
               name_filter=filter,
               min_words=1000,
               top=20,
               w="pc",
               label_col="normalized_name",
               emo_filter=emo,
               title_label="Sentiment-Analyse\nausgewählte Charaktere")

# Orte des Schreckens und der Wut
plot_senti_nrc(label_df=location.df,
               col="location_id",
               name_filter=".",
               min_words=1000,
               top=5,
               w="pc",
               label_col="normalized_name",
               emo_filter="fear|anger|disgust|negative|sadness")

# Orte der Freude und co
plot_senti_nrc(label_df=location.df,
               col="location_id",
               name_filter=".",
               min_words=1000,
               top=5,
               w="pc",
               label_col="normalized_name",
               emo_filter="joy|positive|surprise|trust")

# Emotionen nach Geografie
filter <- paste("^african city$","^china$","^chinatown street$", 
                "^eighteenth-century austrian palace$","^great wall of china$",
                "^italy$","^kremlin$","^paris$","^rome$","^tokyo$",
                "^vienna opera house$",sep="|")
plot_senti_nrc(label_df=location.df,
               col="location_id",
               name_filter=filter,
               min_words=10,
               top=20,
               w="pc",
               label_col="normalized_name",
               emo_filter=".")

################################################################################
#   E. Netzwerk erstellen                                                      #
################################################################################

# E.1. Netzwerk anhand von Sprechaktinteraktion bestimmen

# Netzwerk von Personen am gleichen Ort
# ms.thld <- 90000 # max ms-Diff für Interaktion
# out <- script.df %>%
#     select(id,episode_id,timestamp_in_ms,character_id,location_id) %>%
#     filter(!is.na(character_id) & !is.na(location_id)) %>%
#     mutate(grp=paste(episode_id,location_id,sep="_"),grp_out=NA) %>%
#     arrange(episode_id, location_id, timestamp_in_ms)
# 
# for (i in 1:nrow(out)){
#     if (i!=1){
#         # Zeile 2 und alle weiteren: wenn Location oder Ort abweichen oder
#         # Zeitunterschied zwischen Sprechakten zu groß, wird der Zeile eine neue
#         # Gruppe zugewiesen
#         if (out$grp[i]!=out$grp[i-1] | (out$timestamp_in_ms[i]-out$timestamp_in_ms[i-1]>ms.thld)){
#             grp <- out$timestamp_in_ms[i]
#         }
#     } else { # Zeile 1
#         grp <- out$timestamp_in_ms[i]
#     }
#     # innerhalb der 'grp_out' sind Akteur*innen miteinander verbunden
#     out$grp_out[i] <- paste(out$grp[i],grp,sep="_") 
# }
# save(out,file="netzwerk/out90000.RData")
load(file="netzwerk/out90000.RData")

# Netzwerk erstellen
# directed mit loops
# 15.919 Gruppen bei 140.382 Zeilen
# 
# source <- character()
# target <- character()
# for (split in unique(out$grp_out)){
#     s.dmmy <- rep(out$character_id[out$grp_out==split],
#                   length(out$character_id[out$grp_out==split]))
#     t.dmmy <- sort(s.dmmy)
#     source <- append(source,s.dmmy)
#     target <- append(target,t.dmmy)
# }
# net_dl <- data_frame(source=source,target=target)
# save(net_dl,file="net_dl90000.RData")
load(file="netzwerk/net_dl90000.RData")

# Loops löschen
net_d <- net_dl %>% 
    filter(source!=target)

# undirected + loops
#id: Beziehung anhand von net_dl unabhängig von ihrer Richtung
# id <- sapply(1:nrow(net_dl),function(f) paste(sort(c(net_dl$source[f],net_dl$target[f])),collapse="_"))
# net_ul <- net_dl %>%
#     mutate(id=id) %>%
#     group_by(id) %>%
#     mutate(wt=n()) %>% # weight: Anzahl der Verbindungen (wird doppelt gezählt, nur manche loops nicht)
#     ungroup() %>%
#     filter(!duplicated(id))
# save(net_ul,file="netzwerk/net_ul90000.RData")
load(file="netzwerk/net_ul90000.RData")
# loops löschen
net_u <- net_ul %>%
    filter(source!=target)

# E.2. Netzwerk, Modularitätsklassen und Co in [R] 

g <- net_u %>%
    as_tbl_graph(directed=F) %>%
    mutate(degree=centrality_degree(weights=wt)) %>%
    mutate(betw=centrality_betweenness(weights=wt)) %>%
    mutate(close=centrality_closeness(weights=wt)) %>%
    mutate(eigen=centrality_eigen(weights=wt)) %>%
    mutate(pagerank=centrality_pagerank(weights=wt)) %>%
    #filter(degree>100) %>%
    mutate(mod_class=as.factor(group_louvain(weights=wt))) %>%
    group_by(mod_class) %>%
    mutate(mod_size=n()) %>%
    ungroup()

# Plot: Netzwerk in R
g %>%
    filter(mod_size >= sort(unique(mod_size),decreasing=T)[5]) %>% # top-5 Cluster
    top_n(30,pagerank) %>%
    activate(nodes) %>%
    left_join(character.df %>%
                  select(id,normalized_name) %>%
                  mutate(id=as.character(id)),
              by=c("name"="id")) %>%
    ggraph(layout="lgl") + 
    geom_edge_link(edge_alpha=.3) + 
    geom_node_point(aes(size=log(pagerank),colour=mod_class)) +
    geom_node_text(aes(label=normalized_name),
                   colour="black",
                   alpha=0.8,
                   vjust=0.2) +
    theme_graph()

# Gephi: output
nodes <- g %>%
    #filter(mod_size >= sort(unique(mod_size),decreasing=T)[5]) %>% # top-5 Cluster
    as_tibble() %>%
    left_join(character.df %>%
                  select(id,normalized_name) %>%
                  mutate(id=as.character(id)),
              by=c("name"="id")) %>%
    mutate(id=as.integer(name),
           label=normalized_name) %>%
    select(id,mod_class,label,degree,pagerank, betw, close, eigen) %>%
    mutate(row_id=row_number())
edges <- g %>%
    #filter(mod_size >= sort(unique(mod_size),decreasing=T)[5]) %>% # top-5 Cluster
    activate(edges) %>%
    as_tibble() %>% 
    left_join(nodes %>%
                  mutate(source=id) %>%
                  select(source,row_id),
              by=c("from"="row_id")) %>%
    left_join(nodes %>%
                  mutate(target=id) %>%
                  select(target,row_id),
              by=c("to"="row_id")) %>%
    mutate(type="undirected",
           weight=wt,
           log_weigth=log(wt)) %>%
    select(source,target,id,type,weight,log_weigth)

write.table(edges,file="edge_ges.csv",sep=";",row.names=F)
write.table(nodes,file="node_ges.csv",sep=";",row.names=F)

# intensivste beziehungen: Top_15
bez <- edges %>% 
    filter(source==1|target==1) %>% 
    arrange(-weight) %>%
    left_join(character.df %>% mutate(from_lab=normalized_name) %>%select(id,from_lab),
              by=c("source"="id")) %>%
    left_join(character.df %>% mutate(to_lab=normalized_name) %>%select(id,to_lab),
              by=c("target"="id")) %>%
    top_n(10,weight)

# top 10 bez verschiedener akteuere aneinander ketten
plot_topbez <- function(char_id,char_label,top=10) {
    out <- data_frame()
    colour_pal <- c("#FFD90F","#9c5b01","#2359F1","#83C33F","#DA6901",
                    "#A2232C","#70D1FF","#D1B270","#4D5357","#ff81c1")
    for (i in char_id) {
        dmmy <- edges %>% 
            filter(source==i|target==i) %>% 
            arrange(-weight) %>%
            #left_join(character.df %>% mutate(from_lab=normalized_name) %>%select(id,from_lab),
            #          by=c("source"="id")) %>%
            #left_join(character.df %>% mutate(to_lab=normalized_name) %>%select(id,to_lab),
            #          by=c("target"="id")) %>%
            mutate(weight_pc=weight/sum(weight)) %>%
            top_n(top,weight) %>%
            mutate(class=i)
        out <- out %>% 
            bind_rows(dmmy)
    }
    out <- out %>%
        group_by(class) %>%
        arrange(-weight) %>%
        mutate(order=row_number()) %>% 
        ungroup()
    out %>%
        ggplot(aes(x=order,y=weight_pc,group=as.factor(class),colour=as.factor(class))) +
        geom_line() + 
        facet_wrap(~class,scale="free") +
        scale_colour_manual(labels=char_label,values=colour_pal[1:length(char_label)]) +
        scale_x_continuous(breaks=round(seq(from=1,to=max(out$order),by=(max(out$order)-1)/10)),
                           labels=round(seq(from=1,to=max(out$order),by=(max(out$order)-1)/10))) +
        ggplot2::theme_bw() +
        labs(x="",y="Anteil") +
        ggplot2::ggtitle(paste0("Top ",top," Beziehungen ausgewählter Charaktere")) +
        guides(colour=guide_legend(title="")) +
        ggplot2::theme(plot.title=element_text(color="#666666",face="bold",size=18,hjust=0.5),
                       axis.text=element_text(color="#666666"),
                       axis.title=element_text(color="#666666"))
    
    
}

bez %>%
    mutate(order=row_number()) %>%
    ggplot(aes(x=order,y=weight)) +
    geom_line()

# Top 5 Mod-Class-Character
top_modchar <- g %>% 
    filter(mod_size >= sort(unique(mod_size),decreasing=T)[5]) %>%
    group_by(mod_class) %>%
    top_n(5,degree) %>%
    ungroup() %>% 
    as_tibble() %>%
    left_join(character.df %>%
                  select(id,normalized_name) %>%
                  mutate(id=as.character(id)),
              by=c("name"="id")) %>%
    group_by(mod_class) %>%
    dplyr::summarise(mod_char=paste(normalized_name,collapse=",    ")) %>%
    dplyr::mutate(mod_char=paste0(mod_class,": ",mod_char))
                     
# ###########
# 
# # E.2. Netzwerk, Modularitätsklassen und Co in [R] 
# 
# # graph erzeugen
# g <- graph_from_data_frame(net_u %>%
#                                select(source,target,wt),
#                            directed=F)
# # components berechnen
# components(g,mode="weak")
# component_distribution(g,
#                        cumulative=T, # rel. Häufigkeit
#                        mul.size=F)   # rel. Häufigkeit * Clustergröße
# count_components(g,mode="weak")
# 
# # components mit zugehörigen nodes
# groups(components(g))
# 
# 
# # Filter: Nodes mit min-Degree=100
# node_filter <-names(which(graph.strength(g,weight=E(g)$wt)>100))
# g2 <- induced_subgraph(g, node_filter)
# 
# # Modularitätscluster festlegen/zuordnen
# cl <- cluster_louvain(g2,weights=E(g2)$wt) %>%
#     membership()
# g2.louv <- make_clusters(g2,membership=cl)
# # Node-/Edgelist mit Attributen erzeugen (Modularity-Class und Weight)
# nodes <- dplyr::data_frame(id=names(membership(g2.louv)),
#                     mod_class=as.numeric(membership(g2.louv))) %>%
#     group_by(mod_class) %>%
#     mutate(grp_size=n()) %>%
#     ungroup()
# edges <- as_data_frame(g2)
# 
# top_modclass <- nodes %>%
#     select(mod_class,grp_size) %>%
#     filter(!duplicated(mod_class)) %>%
#     top_n(5,grp_size)
# 
# # zentralste Charaktere in ModClass
# top_modchar <- script.df %>%
#     dplyr::group_by(character_id) %>%
#     dplyr::summarize(words=sum(word_count)) %>%
#     dplyr::left_join(nodes %>% 
#                         mutate(id=as.numeric(id)),
#                     by=c("character_id"="id")) %>%
#     dplyr::group_by(mod_class) %>%
#     dplyr::top_n(5,words) %>%
#     dplyr::left_join(character.df %>%
#                          select(id,normalized_name),
#                      by=c("character_id"="id")) %>%
#     dplyr::mutate(top_modchar=paste(normalized_name,collapse=", ")) %>%
#     dplyr::select(mod_class,top_modchar) %>%
#     unique()
# 
# # Node-/Edgelist für Gephi-Ausgabe
# node.lst <- nodes %>%
#     #filter(mod_class %in% top_modclass$mod_class) %>%
#     left_join(character.df %>%
#                   select(id,normalized_name,gender) %>%
#                   mutate(id=as.character(id)),
#               by=c("id"="id"))
#     
# edge.lst <- edges %>%
#      #filter(from %in% node.lst$id &
#       #         to %in% node.lst$id) %>% 
#     select(from,to,wt) %>%
#     set_colnames(c("source","target","weight"))
# 
# write.table(edge.lst,file="edge.csv",sep=";",row.names=F)
# write.table(node.lst,file="node.csv",sep=";",row.names=F)
# 

# E.3 Sprechverhalten/Sentiment nach Netzwerkgruppen

col <- "mod_class"
label_df <- top_modchar
top <- 20
name_filter <- "^27$|^3$|^1$|^2$|^9$"

# TFIDF nach Netzwerkcluster
words <- script.df %>%
    dplyr::left_join(nodes,by=c("character_id"="id")) %>%
    dplyr::select(one_of(col),normalized_text) %>%
    dplyr::mutate_(col=col) %>%
    dplyr::filter(!is.na(col)) %>%
    tidytext::unnest_tokens(word,normalized_text) %>%
    dplyr::anti_join(stop_words %>%
                         dplyr::bind_rows(data_frame(word=c("im","dont","youre","ill","ive","hes",
                                                            "didnt","id","youve","youll"),
                                                     lexicon="usr"))) %>%
    dplyr::count_(vars=c(paste(col),"word"),sort=T) %>%
    dplyr::ungroup()
total_words <- words %>%
    dplyr::group_by_(col) %>%
    dplyr::summarize(total=sum(n))
words <- dplyr::left_join(words,total_words)
p <- words %>%
    tidytext::bind_tf_idf_(term_col="word",document_col=col,n_col="n") %>%
    dplyr::select(-total) %>%
    dplyr::arrange(desc(tf_idf)) %>%
    dplyr::left_join(label_df, by="mod_class") %>%
    dplyr::filter(grepl(name_filter,.$mod_class)) %>%
    dplyr::group_by_(col) %>%
    dplyr::top_n(top,tf_idf) %>%
    dplyr::ungroup() %>%
    dplyr::arrange_(col,"tf_idf") %>%
    dplyr::mutate(order=row_number())
p %>% 
    ggplot2::ggplot(aes(y=tf_idf,x=order,color=gsub(",", "\\1\n", p$mod_char))) +
    ggplot2::geom_bar(stat="identity",fill="#FFD90F") +
    ggplot2::scale_color_manual(values=rep("#FFD90F",5)) +
    ggplot2::scale_x_continuous(breaks=p$order,
                                labels=gsub("(.{19})", "\\1\n", p$word),
                                expand=c(0,0)) +
    ggplot2::facet_wrap(~mod_class,scales="free") +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::ggtitle("Sprachgebrauch nach Modularitätscluster\nTop 10 Worte (TFIDF)") +
    ggplot2::guides(color=guide_legend(title="Top 5 Charaktere der Cluster",
                                       title.position="top",
                                       position="bottom", 
                                       keywidth=0,
                                       keyheight=0,
                                       override.aes = list(alpha = 0,
                                                           colour="white"))) +
    ggplot2::theme(plot.title=element_text(color="#666666",face="bold",size=18,hjust=0.5),
                   axis.text=element_text(color="#666666"),
                   axis.title=element_text(color="#666666"),
                   legend.position=c(0.8, 0.2))


# Emotionen nach Netzwerkclustern

# Afinn
col <- "mod_class"
label_df <- top_modchar
top <- 10
name_filter <- "."
label_col <- "mod_class"
w <- "wt_sc"

# wt = afinn-score je wort * häufigkeit für gesamte col 
# wt_sc = normalisiertes wt
words <- script.df %>%
    dplyr::left_join(nodes,by=c("character_id"="id")) %>%
    dplyr::select(one_of(col),normalized_text) %>%
    dplyr::mutate_(col=col) %>%
    dplyr::filter(!is.na(col)) %>%
    tidytext::unnest_tokens(word,normalized_text) %>%
    dplyr::anti_join(stop_words %>%
                         dplyr::bind_rows(data_frame(word=c("im","dont","youre","ill","ive","hes",
                                              "didnt","id","youve","youll"),
                                       lexicon="usr"))) %>%
    count_(vars=c(paste(col),"word"),sort=T) %>%
    ungroup()
p <- words %>%
    dplyr::filter(grepl(name_filter,.[[label_col]])) %>%
    dplyr::inner_join(get_sentiments("afinn"),by=c("word"="word")) %>%
    dplyr::group_by_(col) %>%
    dplyr::summarise(wt=sum(score*n),n=sum(n)) %>%
    dplyr::mutate(wt_sc=as.numeric(scale(wt))) %>%
    dplyr::filter(grepl(name_filter, .[[label_col]])) %>%
    dplyr::mutate(grp=ifelse(wt>0,"pos","neg")) %>%
    dplyr::mutate_(label=label_col) %>%
    dplyr::group_by(grp) %>%
    dplyr::mutate_(top_var=w) %>%
    dplyr::top_n(top,abs(top_var)) %>%
    dplyr::left_join(top_modchar,by="mod_class") 
p %>%
    ggplot(aes(x=reorder(label,-top_var),y=top_var,color=gsub(",", "\\1\n", p$mod_char))) + 
    ggplot2::geom_col(fill="#FFD90F") +
    ggplot2::scale_color_manual(values=rep("#FFD90F",5)) +
    coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::labs(x="Cluster",y="normalisierte Afinn-Score") +
    ggplot2::ggtitle("Sentiment-Analyse\nTop 5 Cluster nach pos/neg konnotierten Worte") +
    ggplot2::guides(color=guide_legend(title="Top 5 Charaktere der Cluster",
                                       title.position="top",
                                       position="bottom",
                                       keywidth=0,
                                       keyheight=0,
                                       override.aes = list(alpha = 0,
                                                           colour="white"))) +
    ggplot2::theme(plot.title=element_text(color="#666666",face="bold",size=18,hjust=0.5),
                   axis.text=element_text(color="#666666"),
                   axis.title=element_text(color="#666666"),
                   legend.position=c(0.8, 0.6))

# NRC

min_words <- 0
emo_filter <- "."
w <- "pc"
top<-5
name_filter <- "^27$|^3$|^1$|^2$|^9$"
label_col <- "mod_class"

words <- script.df %>%
    dplyr::left_join(nodes,by=c("character_id"="id")) %>%
    dplyr::select(one_of(col),normalized_text) %>%
    dplyr::mutate_(col=col) %>%
    dplyr::filter(!is.na(col)) %>%
    select(one_of(col),normalized_text) %>%
    unnest_tokens(word,normalized_text) %>%
    anti_join(stop_words %>%
                  bind_rows(data_frame(word=c("im","dont","youre","ill","ive","hes",
                                              "didnt","id","youve","youll"),
                                       lexicon="usr"))) %>%
    count_(vars=c(paste(col),"word"),sort=T) %>%
    ungroup()
p <- words %>%
    filter(grepl(name_filter,.[[label_col]])) %>%
    group_by_(col) %>%
    mutate(words_total=sum(n)) %>%
    ungroup() %>%
    filter(words_total>=min_words) %>%
    inner_join(get_sentiments("nrc"),by=c("word"="word")) %>%
    group_by_(col,"sentiment") %>%
    summarise(n=sum(n)) %>%
    ungroup() %>%
    group_by_(col) %>%
    mutate(pc=n/sum(n)) %>%
    ungroup() %>%
    group_by(sentiment) %>%
    mutate(n_sc=as.numeric(scale(n)),pc_sc=as.numeric(scale(pc))) %>%
    ungroup() %>%
    filter(grepl(emo_filter,.$sentiment)) %>%
    group_by(sentiment) %>%
    mutate_(top_var=w) %>%
    top_n(top,top_var) %>%
    left_join(top_modchar,
              by="mod_class") %>%
    ungroup() %>%
    filter(grepl(name_filter, .[[label_col]])) %>%
    arrange(sentiment,top_var) %>%
    mutate(order=row_number())
p %>%
    ggplot(aes(x=order,y=top_var,fill=sentiment,color=gsub(",", "\\1\n", p$mod_char))) +
    geom_bar(stat="identity",position="stack") + 
    ggplot2::scale_color_manual(values=rep("grey",5)) +
    ggplot2::scale_fill_discrete(guide=F) +
    scale_x_continuous(breaks=p$order,
                       labels=substr(p[[label_col]],1,20),
                       expand=c(0,0)) +
    facet_wrap(~sentiment,scales="free") + 
    coord_flip() +
    ggplot2::theme_bw() +
    #ggplot2::labs(x="Cluster",y="normalisierte Afinn-Score") +
    ggplot2::ggtitle("Sentiment-Analyse\nWortanteil nach Emotionen") +
    ggplot2::guides(color=guide_legend(title="Top 5 Charaktere der Cluster",
                                       title.position="top",
                                       position="bottom",
                                       keywidth=0,
                                       keyheight=0,
                                       override.aes = list(alpha = 0,
                                                           colour="white"),
                                       nrow=2)) +
    ggplot2::theme(plot.title=element_text(color="#666666",face="bold",size=18,hjust=0.5),
                   axis.text=element_text(color="#666666"),
                   axis.title=element_text(color="#666666"),
                   legend.position=c(0.75, 0.15))

# Pagerank + Co im Episodenschnitt
out
source <- character()
target <- character()
ep <- character()
for (split in unique(out$grp_out)){
    s.dmmy <- rep(out$character_id[out$grp_out==split],
                  length(out$character_id[out$grp_out==split]))
    t.dmmy <- sort(s.dmmy)
    e.dmmy <- rep(str_match(split,"^\\d*")[1],length(s.dmmy))
    source <- append(source,s.dmmy)
    target <- append(target,t.dmmy)
    ep <- append(ep,e.dmmy)
}
net_dl <- data_frame(source=source,target=target,ep=ep)
# Loops löschen
net_d <- net_dl %>% 
    filter(source!=target)

# undirected + loops
#id: Beziehung anhand von net_dl unabhängig von ihrer Richtung
id <- sapply(1:nrow(net_dl),function(f) paste(sort(c(net_dl$ep[f], net_dl$source[f],net_dl$target[f])),collapse="_"))
net_ul <- net_dl %>%
    mutate(id=id) %>%
    group_by(id) %>%
    mutate(wt=n()) %>% # weight: Anzahl der Verbindungen (wird doppelt gezählt, nur manche loops nicht)
    ungroup() %>%
    filter(!duplicated(id))
# loops löschen
net_u <- net_ul %>%
    filter(source!=target)
save(net_u,file="net_u_ep.RData")

# E.2. Netzwerk, Modularitätsklassen und Co in [R] 
n.ep <- tibble()
for (f in unique(net_u$ep)){
    g <- net_u %>%
        filter(ep==f) %>%
        as_tbl_graph(directed=F) %>%
        mutate(degree=centrality_degree(weights=wt)) %>%
        mutate(betw=centrality_betweenness(weights=wt)) %>%
        mutate(close=centrality_closeness(weights=wt)) %>%
        mutate(eigen=centrality_eigen(weights=wt)) %>%
        mutate(pagerank=centrality_pagerank(weights=wt)) %>%
        #filter(degree>100) %>%
        mutate(mod_class=as.factor(group_louvain(weights=wt))) %>%
        group_by(mod_class) %>%
        mutate(mod_size=n()) %>%
        ungroup()
    n <- g %>%
        as_tibble() %>%
        mutate(ep=f)
    n.ep <- n.ep %>%
        bind_rows(n)
}
out <- n.ep %>%
    group_by(name) %>%
    summarize(pr=mean(pagerank)) %>%
    arrange(-pr) %>%
    left_join(character.df %>% select(id,normalized_name) %>% mutate(id=as.character(id)),
              by=c("name"="id"))
 
out2 <- n.ep %>%
    group_by(name) %>%
    summarize(pr=mean(pagerank),ep_n=n()) %>%
    mutate(pr_sc=log(as.numeric(scale(pr))),ep_sc=log(as.numeric(scale(ep_n)))) %>%
    left_join(character.df %>% select(id,normalized_name) %>% mutate(id=as.character(id)),
              by=c("name"="id"))


out2 %>%
    ggplot(aes(x=ep_sc,y=pr_sc)) +
    geom_point() +
    geom_hline(aes(yintercept=0),linetype="dashed",alpha=0.3) + 
    geom_vline(aes(xintercept=0),linetype="dashed",alpha=0.3) +
    #scale_colour_manual(name="Geschlecht",labels=c("weiblich","männlich"),
    #                    values=c("#1791FF","#FFD90F")) +
    #scale_size(name="Wortanzahl",breaks=c(10000,20000,30000),labels=c("10.000","20.000","30.000")) +
    theme_bw() + 
    # scale_y_continuous(limits=c(-1,4),
    #                    breaks=seq(-1,4,1),
    #                    labels=seq(-1,4,1)) +
    # scale_x_continuous(limits=c(-2,3),
    #                   breaks=seq(-2,3,1),
    #                   labels=seq(-2,3,1)) +
    geom_text(aes(label=normalized_name),
              show.legend=F,
              hjust=0, 
              vjust=1.5,
              size=2,
              colour="#666666") +
    theme(plot.title=element_text(color="#666666",face="bold",size=18,hjust=0.5),
          axis.title=element_text(color="#666666"),
          axis.text=element_text(color="#666666"),
          legend.text=element_text(color="#666666"),
          legend.title=element_text(color="#666666"),
          legend.position="right") +
    coord_flip()




                

# Vorkommen von Worten im Zeitverlauf
corpus2 <- script.df %>%
    group_by(episode_id) %>%
    mutate(text=paste(normalized_text,collapse=" ")) %>%
    select(episode_id,text) %>%
    filter(!duplicated(episode_id)) %>%
    ungroup() %>%
    select(text) %>%
    DataframeSource() %>%
    Corpus()

corp.tdy <- corpus2 %>% 
    tidy() %>%
    left_join(episodes.df %>%
                  select(id,original_air_date) %>%
                  mutate(id=as.character(id)),
              by=c("id"="id")) %>%
    unnest_tokens(word,text) %>%
    anti_join(stop_words,by=c("word"="word"))
# Wörter pro Datum/anteilig
corp.tdy <- corp.tdy %>%
    select(id,word) %>%
    count(id, word) %>%
    ungroup() %>%
    tidyr::complete(id, word, fill=list(n=0)) %>%
    group_by(id) %>%
    mutate(id_total=sum(n), percent=n/id_total) %>%
    ungroup() %>%
    group_by(word) %>%
    filter(sum(n) > 500)

model <- corp.tdy %>%
    filter(word %in% c("bart","homer","lisa","marge")) %>%
    do(tidy(glm(cbind(n,id_total-n)~id,.,family="binomial"))) %>%
    ungroup() %>%
    #filter(term=="original_air_date") %>%
    arrange(desc(abs(estimate)))

model %>% 
    mutate(adjusted.p.value=p.adjust(p.value)) %>%
    ggplot(aes(estimate, adjusted.p.value)) + 
    geom_line() +
    scale_y_log10() +
    #geom_text(aes(label=word),vjust=1,hjust=1,check_overlap=T) +
    xlab("Est change over time") +
    ylab("Adjust p-value") +
    facet_wrap(~word,scale="free")

model %>% 
    #filter(word %in% c("hell","dad","cool","springfield","school","worry")) %>%
    filter(word %in% c("bart","marge","homer","lisa","maggie","burns","moe","barney")) %>%
    #top_n(20, abs(estimate)) %>%
    inner_join(corp.tdy) %>%
    ggplot(aes(id,percent)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~word,scales="free") +
    scale_y_continuous()+
    ylab("Freq of word in speech")


# Wortnetzwerk einer Episode - Worte nach Geschlecht einfärben
# Mad, mad marge episode 247
ms.thld <- 90000
out <- script.df %>%
    #filter(episode_id==247) %>%
    select(character_id, episode_id, location_id, timestamp_in_ms, normalized_text) %>%
    left_join(character.df %>%
                  select(id,normalized_name,gender),
              by=c("character_id"="id")) %>%
    filter(!is.na(character_id) & !is.na(location_id)) %>%
          mutate(grp=paste(episode_id,location_id,sep="_"),grp_out=NA) %>%
          arrange(episode_id, location_id, timestamp_in_ms)
for (i in 1:nrow(out)){
    if (i!=1){
        # Zeile 2 und alle weiteren: wenn Location oder Ort abweichen oder
        # Zeitunterschied zwischen Sprechakten zu groß, wird der Zeile eine neue
        # Gruppe zugewiesen
        if (out$grp[i]!=out$grp[i-1] | (out$timestamp_in_ms[i]-out$timestamp_in_ms[i-1]>ms.thld)){
            grp <- out$timestamp_in_ms[i]
        }
    } else { # Zeile 1
        grp <- out$timestamp_in_ms[i]
    }
    # innerhalb der 'grp_out' sind Akteur*innen miteinander verbunden
    out$grp_out[i] <- paste(out$grp[i],grp,sep="_")
}
save(out,file="netzwerk/out_word.RData")
load(file="netzwerk/out_loc247.RData")

words <- out %>%
    dplyr::select(gender,grp_out,normalized_text) %>%
    tidytext::unnest_tokens(word,normalized_text) %>%
    dplyr::anti_join(stop_words %>%
                         dplyr::bind_rows(data_frame(word=c("im","dont","youre","ill","ive","hes",
                                                            "didnt","id","youve","youll"),
                                                     lexicon="usr"))) %>%
    dplyr::group_by(word) %>%
    dplyr::mutate(m_pc = ifelse(gender=="m",1,0) %>% sum() %>% divide_by(n())) %>% # anteil von männern verwendeter worte
    dplyr::ungroup() %>%
    dplyr::group_by(word) %>%
    dplyr::mutate(n=n()) %>%
    #dplyr::distinct(word,.keep_all=T) %>%
    dplyr::ungroup() %>%
    dplyr::select(-gender)

    
# Netzwerk erstellen
# directed mit loops

# Edgelist erstellen 
# Innerhalb einer Gruppe bilden drei aufeinanderfolgende Worte eine Beziehung
# Target zu NA (am Ende einer Liste) werden gelöscht

edges <- data_frame()
for (split in unique(words$grp_out)){
    dmmy <- words %>%
        filter(grp_out==split) %>%
        mutate(source=word,
               t1=lead(word,n=1),
               t2=lead(word,n=2),
               t3=lead(word,n=3),
               t4=lead(word,n=4),
               t5=lead(word,n=5))
    edge.dmmy <- dmmy %>%
        mutate(target=t1) %>%
        select(source,target) %>%
        bind_rows(dmmy %>%
                      mutate(target=t2) %>%
                      select(source,target)) %>%
        bind_rows(dmmy %>%
                      mutate(target=t3) %>%
                      select(source,target)) %>%
        bind_rows(dmmy %>%
                      mutate(target=t4) %>%
                      select(source,target)) %>%
        bind_rows(dmmy %>%
                      mutate(target=t5) %>%
                      select(source,target))
    edges <- edges %>%
        bind_rows(edge.dmmy)
}


edges <- edges %>%
    filter(!is.na(target)) %>%
    filter(source!=target)
nodes <- words %>%
    group_by(word) %>%
    distinct(word,.keep_all=T) %>%
    ungroup() %>%
    mutate(id=word,
           label=word,
           weight=n) %>%
    select(id,label,weight,m_pc)

write.table(edges,file="edge_word.csv",sep=";",row.names=F)
write.table(nodes,file="node_word.csv",sep=";",row.names=F)


