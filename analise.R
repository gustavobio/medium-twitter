library(dplyr)
library(twitteR)
library(purrr)
library(ggplot2)
library(tidyr)
library(stringr)
library(rvest)
library(lubridate)
library(tidytext)
library(silgelib)

bolsonaro_tweets <- userTimeline("jairbolsonaro", n = 3200, includeRts = TRUE)
marina_tweets <- userTimeline("silva_marina", n = 3200, includeRts = TRUE)
ciro_tweets <- userTimeline("cirogomes", n = 3200, includeRts = TRUE)
lula_tweets <- userTimeline("LulapeloBrasil", n = 3200, includeRts = TRUE)
alckmin_tweets <- userTimeline("geraldoalckmin", n = 3200, includeRts = TRUE)

bolsonaro_tweets_df <- tbl_df(map_df(bolsonaro_tweets, as.data.frame))
marina_tweets_df <- tbl_df(map_df(marina_tweets, as.data.frame))
ciro_tweets_df <- tbl_df(map_df(ciro_tweets, as.data.frame))
lula_tweets_df <- tbl_df(map_df(lula_tweets, as.data.frame))
alckmin_tweets_df <- tbl_df(map_df(alckmin_tweets, as.data.frame))

bolsonaro_stats <- as.data.frame(getUser("jairbolsonaro"))
marina_stats <- as.data.frame(getUser("silva_marina"))
ciro_stats <- as.data.frame(getUser("cirogomes"))
lula_stats <- as.data.frame(getUser("LulapeloBrasil"))
alckmin_stats <- as.data.frame(getUser("geraldoalckmin"))

pre_stats <- bind_rows(bolsonaro_stats, marina_stats, ciro_stats, lula_stats, alckmin_stats)

presidenciaveis <- bind_rows(bolsonaro_tweets_df, marina_tweets_df, ciro_tweets_df, lula_tweets_df, alckmin_tweets_df)

presidenciaveis <- presidenciaveis %>%
  mutate(presidenciavel = case_when(
    screenName == "jairbolsonaro" ~ "Bolsonaro",
    screenName == "silva_marina" ~ "Marina",
    screenName == "cirogomes" ~ "Ciro",
    screenName == "geraldoalckmin" ~ "Alckmin", 
    screenName == "LulapeloBrasil" ~ "Lula"
  )) %>%
  mutate(fonte = str_extract(statusSource, "(?<=\\>).+(?=\\<)")) %>%
  mutate(fonte = case_when(
    fonte == "Twitter for iPhone" ~ "iPhone",
    fonte == "Twitter for Android" ~ "Android",
    fonte == "Twitter Web Client" ~ "Web Client",
    TRUE ~ "Outros"
  )) %>%
  mutate(horario = hour(created)) %>%
  mutate(dia_semana = wday(created, label = T, locale = "pt_BR.UTF-8", week_start = 1)) %>%
  mutate(RT = ifelse(str_detect(text, "^RT"), "Retweet", "Próprio"))

p_tweets <- pre_stats %>%
  mutate(name = reorder(name, statusesCount)) %>%
  ggplot(aes(x = name, y = statusesCount)) + 
  labs(x = NULL, y = NULL, title = "Número de tweets") +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = scales::format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  coord_flip() + 
  theme_plex(base_size = 14)

ggsave(p_tweets, file = "p_tweets.png", width = 8, height = 6, dpi = 300)

p_seguidores <- pre_stats %>%
  mutate(name = reorder(name, followersCount)) %>%
  ggplot(aes(x = name, y = followersCount)) + 
  labs(x = NULL, y = NULL, title = "Número de seguidores no Twitter") +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = scales::format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  coord_flip() + 
  theme_plex(base_size = 14)

ggsave(p_seguidores, file = "p_seguidores.png", width = 8, height = 6, dpi = 300)

p_tweet_retweet <- presidenciaveis %>%
  group_by(presidenciavel, RT) %>%
  summarise(n = n()) %>%
  mutate(RT_pct = n/sum(n)) %>%
  ggplot(aes(x = RT, y = RT_pct)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Tipo de tweet", y = NULL, title = "Tipos de mensagens") +  
  facet_wrap(~presidenciavel, ncol = 4) + 
  scale_y_continuous(labels = scales::percent_format()) +
  theme_plex(base_size = 14)
  
ggsave(p_tweet_retweet, file = "p_tweet_retweet.png", width = 9, height = 5, dpi = 300)
  
p_retweets <- presidenciaveis %>%
  filter(RT == "Próprio") %>%
  group_by(presidenciavel) %>%
  summarise(retweets = mean(retweetCount)) %>%
  mutate(presidenciavel = reorder(presidenciavel, retweets)) %>%
  ggplot(aes(x = presidenciavel, y = retweets)) + 
  labs(x = NULL, y = NULL, title = "Número médio de retweets por postagem") +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = scales::format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  coord_flip() + 
  theme_plex(base_size = 14)

ggsave(p_retweets, file = "p_retweets.png", width = 8, height = 6, dpi = 300)

p_retweet_horario <- presidenciaveis %>%
  filter(!grepl("^RT", text)) %>%
  mutate(horario = hour(created)) %>%
  ggplot(aes(x = horario, y = retweetCount)) +
  labs(x = "Hora do dia", y = "log10(número de retweets)") +
  geom_point(alpha = 0.3) + 
  geom_smooth() + 
  facet_wrap(~presidenciavel, scales = "free", ncol = 2) +
  scale_y_log10() + 
  theme_plex()

ggsave(p_retweet_horario, file = "p_retweet_horario.png", width = 8, height = 8, dpi = 300)

p_retweets_week <- presidenciaveis %>%
  filter(RT == "Próprio") %>%
  group_by(date = floor_date(created, "week"), presidenciavel) %>%
  summarise(retweets = mean(retweetCount)) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = retweets, group = presidenciavel, colour = presidenciavel)) +
  geom_line() + 
  labs(x = "Data", y = "Número médio de retweets", title = "Média de compartilhamentos por semana", subtitle = "Nos últimos 3200 tweets de cada candidato") + 
  scale_y_continuous(labels = scales::format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  guides(colour = guide_legend(title = "Presidenciável")) + 
  theme_plex(base_size = 14)

ggsave(p_retweets_week, file = "p_retweets_week.png", width = 12, height = 4, dpi = 300)

p_retweets_week_log10 <- presidenciaveis %>%
  filter(!grepl("^RT", text)) %>%
  group_by(date = floor_date(created, "week"), presidenciavel) %>%
  summarise(retweets = mean(retweetCount)) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = retweets, group = presidenciavel, colour = presidenciavel)) +
  geom_line() + 
  geom_smooth() + 
  labs(x = "Data", y = "log10(média de compartilhamentos)", title = "Média de compartilhamentos por semana", subtitle = "Nos últimos 3200 tweets de cada candidato") + 
  scale_y_log10(labels = scales::format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  guides(colour = guide_legend(title = "Presidenciável")) + 
  theme_plex(base_size = 14)

ggsave(p_retweets_week_log10, file = "p_retweets_week_log10.png", width = 12, height = 4, dpi = 300)

p_tweets_all <- presidenciaveis %>%
  filter(RT == "Próprio") %>%
  ggplot(aes(x = created, y = retweetCount)) +
  geom_line(size = 0.1) + 
  labs(x = "Data", y = "Número de retweets", title = "Número de compartilhamentos em cada postagem", subtitle = "Nos últimos 3200 tweets de cada candidato") + 
  scale_y_continuous(labels = scales::format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  guides(colour = guide_legend(title = "Presidenciável")) + 
  theme_plex(base_size = 14) + 
  facet_wrap(~presidenciavel, scales = "free", ncol = 2)

ggsave(p_tweets_all, file = "p_tweets_all.png", width = 14, height = 8, dpi = 300)

presidenciaveis_limpo <- presidenciaveis %>%
  filter(!grepl("^RT", text)) %>%
  mutate(text = str_replace(text, "(http).*", "")) %>%
  group_by(presidenciavel) %>%
  arrange(desc(retweetCount))

tokens_top_100 <- presidenciaveis_limpo %>%
  filter(row_number() %in% 1:300) %>%
  ungroup() %>%
  unnest_tokens(palavra, text) %>%
  anti_join(stop_words_br) %>%
  filter(nchar(palavra) > 1) %>%
  group_by(presidenciavel) %>%
  count(palavra, sort = TRUE) %>%
  arrange(presidenciavel, desc(n)) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(palavra = reorder_within(palavra, n, presidenciavel))

tokens_resto <- presidenciaveis_limpo %>%
  filter(!row_number() %in% 1:500) %>%
  ungroup() %>%
  unnest_tokens(palavra, text) %>%
  anti_join(stop_words_br) %>%
  filter(nchar(palavra) > 1) %>%
  group_by(presidenciavel) %>%
  count(palavra, sort = TRUE) %>%
  arrange(presidenciavel, desc(n)) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(palavra = reorder_within(palavra, n, presidenciavel))

p_palavras <- tokens_top_100 %>%
  ggplot(aes(x = palavra, y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  facet_wrap(~presidenciavel, scales = "free_y") + 
  theme_plex(base_size = 14) + 
  scale_x_reordered() + 
  labs(x = "Palavra", y = "Número de usos", title = "Palavras usadas nos tweets mais compartilhados")

ggsave(p_palavras, file = "p_palavras.png", width = 12, height = 12, dpi = 300)

p_tweets_meio <- presidenciaveis %>%
  group_by(data = floor_date(created, "week"), presidenciavel, fonte) %>%
  summarise(tweets = n()) %>%
  ungroup() %>%
  ggplot(aes(x = data, y = tweets)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 0.8) + 
  labs(x = "Data", y = "log10(número total de tweets)", title = "Quais os dispositivos usados por cada candidato?", subtitle = "Total por semana por dispositivo") + 
  guides(colour = guide_legend(title = "Presidenciável")) + 
  scale_y_log10() + 
  facet_grid(fonte ~ presidenciavel, scale = "free_x") + 
  theme_bw() + 
  theme(strip.text = element_text(family = "IBMPlexSans", size = 12),
        axis.text = element_text(family = "IBMPlexSans", size = 12),
        title = element_text(family = "IBMPlexSans-Bold", size = 12))

ggsave(p_tweets_meio, file = "p_tweets_meio.png", width = 10, height = 7, dpi = 300)

stop_words_br <- structure(list(palavra = c("vamos", "obrigado", "agora", "pra", "hoje", "paulo", "são",  "dia", "boa", "bom", "todos", "tudo", "sendo", "novo", "nova", "não", "porque", "bem", "aqui", "vai", "neste", "neste", "deste", "desta",  "de", "a", "o", "que", "e", "do", 
                                            "da", "em", "um", "para", "é", "com", "uma", "os", "no", 
                                            "se", "na", "por", "mais", "as", "dos", "como", "mas", "foi", 
                                            "ao", "ele", "das", "tem", "à", "seu", "sua", "ou", "ser", "quando", 
                                            "muito", "há", "nos", "já", "está", "eu", "também", "só", 
                                            "pelo", "pela", "até", "isso", "ela", "entre", "era", "depois", 
                                            "sem", "mesmo", "aos", "ter", "seus", "quem", "nas", "me", "esse", 
                                            "eles", "estão", "você", "tinha", "foram", "essa", "num", "nem", 
                                            "suas", "meu", "às", "minha", "têm", "numa", "pelos", "elas", 
                                            "havia", "seja", "qual", "será", "nós", "tenho", "lhe", "deles", 
                                            "essas", "esses", "pelas", "este", "fosse", "dele", "tu", "te", 
                                            "vocês", "vos", "lhes", "meus", "minhas", "teu", "tua", "teus", 
                                            "tuas", "nosso", "nossa", "nossos", "nossas", "dela", "delas", 
                                            "esta", "estes", "estas", "aquele", "aquela", "aqueles", "aquelas", 
                                            "isto", "aquilo", "estou", "está", "estamos", "estão", "estive", 
                                            "esteve", "estivemos", "estiveram", "estava", "estávamos", "estavam", 
                                            "estivera", "estivéramos", "esteja", "estejamos", "estejam", 
                                            "estivesse", "estivéssemos", "estivessem", "estiver", "estivermos", 
                                            "estiverem", "hei", "há", "havemos", "hão", "houve", "houvemos", 
                                            "houveram", "houvera", "houvéramos", "haja", "hajamos", "hajam", 
                                            "houvesse", "houvéssemos", "houvessem", "houver", "houvermos", 
                                            "houverem", "houverei", "houverá", "houveremos", "houverão", 
                                            "houveria", "houveríamos", "houveriam", "sou", "somos", "são", 
                                            "era", "éramos", "eram", "fui", "foi", "fomos", "foram", "fora", 
                                            "fôramos", "seja", "sejamos", "sejam", "fosse", "fôssemos", 
                                            "fossem", "for", "formos", "forem", "serei", "será", "seremos", 
                                            "serão", "seria", "seríamos", "seriam", "tenho", "tem", "temos", 
                                            "tém", "tinha", "tínhamos", "tinham", "tive", "teve", "tivemos", 
                                            "tiveram", "tivera", "tivéramos", "tenha", "tenhamos", "tenham", 
                                            "tivesse", "tivéssemos", "tivessem", "tiver", "tivermos", "tiverem", 
                                            "terei", "terá", "teremos", "terão", "teria", "teríamos", 
                                            "teriam")), .Names = "palavra", row.names = c(NA, -220L), class = "data.frame")
