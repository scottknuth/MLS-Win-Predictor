#Prepare Webpages for scraping
URL <- 'https://fbref.com/en/comps/22/history/Major-League-Soccer-Seasons'
WS <- read_html(URL)
FBRefURLs <- WS %>% html_nodes("th a") %>% html_attr("href") %>% as.character()
print(FBRefURLs3)
FBRefURLs2 <- tail(FBRefURLs,24)
FBRefURLs3 <- head(FBRefURLs2, 7)
FBRefURLs3 <- paste0("http://www.fbref.com",FBRefURLs3)
#Prepare Catcher
Catcher1 <- data.frame(Team=character(),Team_URL=character(),WCTeam=character(),WCTeam_URL=character())

for (i in FBRefURLs3) {
  WS1 <- read_html(i) 
  ECTeam <- WS1 %>% html_nodes("#results27981Eastern-Conference_overall .left a") %>% html_text() %>% as.character()
  ECTeam_URL <- WS1 %>% html_nodes("#results27981Eastern-Conference_overall .left a") %>% html_attr("href") %>% as.character()
  WCTeam <- WS1 %>% html_nodes("#results27981Western-Conference_overall .left a") %>% html_text() %>% as.character()
  WCTeam_URL <- WS1 %>% html_nodes("#results27981Western-Conference_overall .left a") %>% html_attr("href") %>% as.character()
  temp <- data.frame(ECTeam,ECTeam_URL,WCTeam,WCTeam_URL)
  Catcher1 <- rbind(Catcher1,temp)
  cat("Working")
  }
#Combine East and West to two columns on DF(just did it in Excel lol)
Catcher1 <- Catcher1 %>%
  select(`ECTeam`, `ECTeam_URL`) %>% 
  bind_rows(
    Catcher1 %>% 
      transmute(`ECTeam` = `WCTeam`, `ECTeam_URL` = `WCTeam_URL`)) 
#Rename Columns
Catcher1 <- Catcher1 %>%
  rename("Team" = `ECTeam`, "Team_URL" = `ECTeam_URL`)
#Add Beginning of URL
Catcher1$Team_URL <- paste0("http://www.fbref.com",Catcher1$Team_URL)

#Catcher2--scraping match report links

Catcher2 <- data.frame(Team=character(),Match_Type = character(), Match_Report_URL=character())

for (i in Catcher1$Team_URL) {
  WS2 <- read_html(i)
  Team <- WS2 %>% html_nodes("#all_stats_standard_2798 h2 span") %>% html_text() %>% as.character()
  Match_Type <- WS2 %>% html_nodes(".left:nth-child(4) a") %>% html_text() %>% as.character()
  Match_Report_URL <- WS2 %>% html_nodes("#matchlogs_all .group_start a") %>% html_attr("href") %>% as.character()
  temp2 <- data.frame(Team,Match_Type,Match_Report_URL)
  Catcher2 <- rbind(Catcher2,temp2)
  cat("Working")
}
#Filter only regular season matchers
Catcher2 <- Catcher2 %>%
  filter(`Match_Type` == "Regular Season")

#Catcher3--match report forms to get us starting XIs,home/away,result
Catcher2$Match_Report_URL <- paste0("http://www.fbref.com",Catcher2$Match_Report_URL)

Catcher3 <- data.frame(Team2 = character(), HStarting_XI = character(), 
                       AStarting_XI = character())
for (i in Catcher2$Match_Report_URL) {
  WS3 <- read_html(i)
  Team2 <- WS3 %>% html_nodes("h1") %>% html_text() %>% as.character()
  HStarting_XI <- WS3 %>% html_nodes("#a a") %>% extract(1:11) %>% html_attr("href") %>% as.character() %>% {if(length(.) == 0) NA else .}
  AStarting_XI <- WS3 %>% html_nodes("#b a") %>% extract(1:11) %>% html_attr("href") %>% as.character() %>% {if(length(.) == 0) NA else .}
  temp3 <- data.frame(Team2, HStarting_XI, AStarting_XI)
  Catcher3 <- rbind(Catcher3,temp3)
  cat("Working")
}
#Combine Home and Away Team Columns
Catcher3 <- Catcher3 %>%
  select(`HStarting_XI`, `AStarting_XI`, `Team2`) %>% 
  bind_rows(
    Catcher3 %>% 
      mutate(`HStarting_XI` = `AStarting_XI`)) 
#Rename and Remove
Catcher3 <- Catcher3 %>%
  rename(Starting_11 = `HStarting_XI`)

Catcher3 <- Catcher3 %>%
  select(`Starting_11`, `Team2`)
#Paste frontend of URL to Catcher3
Catcher3$Starting_11 <- paste0("http://www.fbref.com",Catcher3$Starting_11)

#Gets rid of duplicates and double checks counts
MatchIDDF <- head(MatchIDDF, 8976)

tibblecount <- MatchIDDF %>%
  summarise(count = n())
#Proper Filter Match IDDF
#Filter 
MatchIDDF <- MatchIDDF %>%
  arrange(`count`) %>%
  arrange(desc(`Team2`))

#Paste frontend of URL to MatchID
MatchIDDF$Starting_11 <- paste0("http://www.fbref.com", MatchIDDF$Starting_11)

#Catcher4 Set Up




