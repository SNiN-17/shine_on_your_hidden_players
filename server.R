library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(reshape2)
# githubから必要なデータを取得
# 打撃成績から
# 2012年以後
batUrl <- "https://raw.githubusercontent.com/SNiN-17/farm_stats/master/NPB_batting_since2012.csv"
bat <- read_csv(batUrl, 
                locale=locale(encoding="CP932"))
names(bat)[4] <- "Team"
bat$ID <- paste(bat$Player_name, bat$Team) 
# 16-17
# 前処理済み
batUrl2 <- "https://raw.githubusercontent.com/SNiN-17/shine_on_your_hidden_players/master/career_bat.csv"
career_bat <- read_csv(batUrl2, 
                       locale=locale(encoding="CP932"))
batUrl3 <- "https://raw.githubusercontent.com/SNiN-17/shine_on_your_hidden_players/master/farmBat100.csv"
farmBat100 <- read_csv(batUrl3, 
                       locale=locale(encoding="CP932"))
batUrl4 <- "https://raw.githubusercontent.com/SNiN-17/shine_on_your_hidden_players/master/farmBat200.csv"
farmBat200 <- read_csv(batUrl4, 
                       locale=locale(encoding="CP932"))
# 投手成績
# 12年以後
pitUrl <- "https://raw.githubusercontent.com/SNiN-17/farm_stats/master/NPB_pitching_since2012.csv"
pit <- read_csv(pitUrl, 
                locale=locale(encoding="CP932"))
names(pit)[4] <- "Team"
pit$ID <- paste(pit$Player_name, pit$Team) 
# 16-17, 前処理済み
pitUrl2 <- "https://raw.githubusercontent.com/SNiN-17/shine_on_your_hidden_players/master/career_pit.csv"
career_pit <- read_csv(pitUrl2, 
                       locale=locale(encoding="CP932"))
pitUrl3 <- "https://raw.githubusercontent.com/SNiN-17/shine_on_your_hidden_players/master/farmPit100.csv"
farmPit100 <- read_csv(pitUrl3, 
                       locale=locale(encoding="CP932"))
pitUrl4 <- "https://raw.githubusercontent.com/SNiN-17/shine_on_your_hidden_players/master/farmPit50.csv"
farmPit50 <- read_csv(pitUrl4, 
                       locale=locale(encoding="CP932"))
names(farmPit50)[5] <- "TBF"
names(farmPit100)[5] <- "TBF"


function(input, output) {
  
  # Hitterについて ----
  initialInput <- reactive({
    switch(input$dataset,
           "100 PA以上" = farmBat100,
           "200 PA以上" = farmBat200) 
  })
  
  # チーム名の処理
  datasetInput <- reactive({
    team <- input$team
    if(team == "All"){
      initialInput()
    }else{
      initialInput() %>% filter(Team == team)
    }
  })
  
  # データにインプットされた値でフィルターをかける
  secondInput <- reactive({
    datasetInput() %>% 
      filter(wOBA >= input$slider1[1])  %>% 
      filter(wOBA <= input$slider1[2]) %>% 
      filter(BBpct >= input$slider2[1])  %>% 
      filter(BBpct <= input$slider2[2])%>% 
      filter(Kpct >= input$slider3[1])  %>% 
      filter(Kpct <= input$slider3[2]) %>% 
      filter(Diff >= input$slider4[1])  %>% 
      filter(Diff <= input$slider4[2]) %>% 
      filter(HRpct>= input$slider5[1])  %>% 
      filter(HRpct <= input$slider5[2])  %>% 
      filter(TTO>= input$slider6[1])  %>% 
      filter(TTO <= input$slider6[2]) 
    
  })
  
  # テーブル1で表示するために形を整える
  thirdInput <- reactive({
    secondInput() %>%
      select(-c(3, 12)) %>%
      arrange(desc(Diff))
    
  })
  
  # 12年以後の各年度成績を形を整える
  fourthInput <- reactive({
    df <- secondInput() 
    selectedIDs <- df$ID
    bat2 <- bat %>% filter(ID %in% selectedIDs) %>%
      select(-c(16:18)) %>%
      arrange(Player_name)
  })
  
  
  # テーブル1
  output$view <- DT::renderDataTable({
    head(thirdInput(), input$obs)
  }, options = list(paging = FALSE)) 
  # テーブル2
  output$view2 <- DT::renderDataTable({
    head(fourthInput(), input$obs) %>%
      arrange(Level)
  }, options = list(paging = FALSE)) 
  
  # プロット
  # 選んでいるデータセット全体を箱ひげ図と、plotで描画
  # その中の選択している選手を赤で上書きでplot
  output$plot1 <- renderPlot({
    df <- datasetInput() %>% select(-c(3, 12))
    df <- melt(df,
               id.vars=c("Player_name", "Team"), # 残す列のidentityを指定する
               variable.name = "Stats_type",# 列名からつくる列の名前
               value.name = "Value") # value列の名前
    df2 <- secondInput() %>% 
      select(-c(3, 12))
    df2 <- melt(df2,
                id.vars=c("Player_name", "Team"), # 残す列のidentityを指定する
                variable.name = "Stats_type",# 列名からつくる列の名前
                value.name = "Value") # value列の名前
    gg <- ggplot() +
      geom_boxplot(data = df, aes( x = Team, y = Value),
                   outlier.shape = NA)+
      geom_point(data = df, aes( x = Team, y = Value), size = 0.8) + 
      geom_point(data = df2, aes(x = Team, y = Value), size = 1, color ="red") + 
      theme_gray(base_family = "HiraKakuPro-W3") +
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 15),
            strip.text = element_text(size=14)) +
      facet_wrap(~Stats_type, scales = "free", ncol = 2) +
      labs(caption = "Source: NPB official", 
           x = "Team", y = "Value") 
    gg
  }, res = 72, width = 600 ,height = 1000)
  
  # 参考文献等
  output$refs <- renderUI({
    str1 <- "[1] NPB公式サイト http://npb.jp"
    str2 <- "[2] wOBA係数@deltagraphs http://1point02.jp/op/gnav/glossary/discription/dis_bs_woba.html"
    str3 <- "[3] Shiny全般について ほくそえむ様によるチュートリアル翻訳版目次 http://d.hatena.ne.jp/hoxo_m/20151222/p1"
    str4 <- "[4] Rを使った野球統計全般 Marchi and Albert, Analyzing Baseball Data with R (2013; CRC press)."
    str5 <- "[5] DER, wOBAの計算に関する補足 https://sleepnowinthenumbers.blogspot.jp/2017/09/blog-post_23.html"
    str6 <- "[6] 中の人 https://twitter.com/sleep_in_nmbrs"
    HTML(paste(str1, str2, str3, str4, str5, str6, sep = '<br/>'))
  })
  
  
  # 投手側 ----
  # 基本的に野手でやっていることと同様
  initialInput2 <- reactive({
    switch(input$dataset2,
           "50 IP以上" = farmPit50,
           "100 IP以上" = farmPit100) 
  })
  
  # チーム名の処理
  datasetInput2 <- reactive({
    team2 <- input$team2
    if(team2 == "All"){
      initialInput2()
    }else{
      initialInput2() %>% filter(Team == team2)
    }
  })
  
  # データにインプットされた値でフィルターをかける
  secondInput2 <- reactive({
    datasetInput2() %>% 
      filter(FIP >= input$slider1_2[1])  %>% 
      filter(FIP <= input$slider1_2[2]) %>% 
      filter(BBpct >= input$slider2_2[1])  %>% 
      filter(BBpct <= input$slider2_2[2])%>% 
      filter(Kpct >= input$slider3_2[1])  %>% 
      filter(Kpct <= input$slider3_2[2]) %>% 
      filter(Diff >= input$slider4_2[1])  %>% 
      filter(Diff <= input$slider4_2[2]) %>% 
      filter(K_BB >= input$slider5_2[1])  %>% 
      filter(K_BB <= input$slider5_2[2]) 
    
  })
  
  # table3 (16以後の通算) 用に整形
  thirdInput2 <- reactive({
    secondInput2() %>%
      select(-c(3, 13)) %>%
      arrange(desc(Diff))
    
  })
  
  # table4 (12年以後の各年度) 用に整形
  fourthInput2 <- reactive({
    df <- secondInput2() 
    selectedIDs <- df$ID
    pit2 <- pit %>% filter(ID %in% selectedIDs) %>%
      select(-c(17,18)) %>%
      arrange(Player_name)
  })
  
  
  # テーブル3 (投手の1)
  output$view3 <- DT::renderDataTable({
    head(thirdInput2(), input$obs)
  }, options = list(paging = FALSE)) 
  # テーブル4 (投手の2)
  output$view4 <- DT::renderDataTable({
    head(fourthInput2(), input$obs2) %>%
      arrange(Level)
  }, options = list(paging = FALSE)) 
  
  # プロット
  output$plot2 <- renderPlot({
    df <- datasetInput2() %>% select(-c(3, 13))
    df <- melt(df,
               id.vars=c("Player_name", "Team"), # 残す列のidentityを指定する
               variable.name = "Stats_type",# 列名からつくる列の名前
               value.name = "Value") # value列の名前
    df2 <- secondInput2() %>% 
      select(-c(3, 13))
    df2 <- melt(df2,
                id.vars=c("Player_name", "Team"), # 残す列のidentityを指定する
                variable.name = "Stats_type",# 列名からつくる列の名前
                value.name = "Value") # value列の名前
    gg <- ggplot() +
      geom_boxplot(data = df, aes( x = Team, y = Value),
                   outlier.shape = NA)+
      geom_point(data = df, aes( x = Team, y = Value), size = 0.8) + 
      geom_point(data = df2, aes(x = Team, y = Value), size = 1, color ="red") + 
      theme_gray(base_family = "HiraKakuPro-W3") +
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 15),
            strip.text = element_text(size=14)) +
      facet_wrap(~Stats_type, scales = "free", ncol = 2) +
      labs(caption = "Source: NPB official",
           x = "Team", y = "Value") 
    gg
  }, res = 72, width = 600 ,height = 1000)
  
  # 参考文献等
  output$refs2 <- renderUI({
    str1 <- "[1] NPB公式サイト http://npb.jp"
    str2 <- "[2] FIPの計算@fangraphs http://www.fangraphs.com/library/pitching/fip/"
    str3 <- "[3] Shiny全般について ほくそえむ様によるチュートリアル翻訳版目次 http://d.hatena.ne.jp/hoxo_m/20151222/p1"
    str4 <- "[4] Rを使った野球統計全般 Marchi and Albert, Analyzing Baseball Data with R (2013; CRC press)."
    str5 <- "[5] DER, wOBAの計算に関する補足 https://sleepnowinthenumbers.blogspot.jp/2017/09/blog-post_23.html"
    str6 <- "[6] 中の人 https://twitter.com/sleep_in_nmbrs"
    HTML(paste(str1, str2, str3, str4, str5, str6, sep = '<br/>'))
  })
}