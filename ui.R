# NPBの一軍と二軍の個人成績を表示するshiny app
library(shiny)
shinyUI(navbarPage("不遇な選手を探せ! with Shiny",
                   tabPanel("Hitter",
                            sidebarLayout(
                              position = ("left"),
                              fluid = TRUE,
                              
                              # Sidebar panel for inputs ----
                              sidebarPanel(
                                p("データはNPB公式から. 17/9/17時点での更新分. 
                                  右端のDiffは16年以後に関して、二軍でのPAから一軍でのPAを引いた数値."),
                                
                                # Input: Selector for choosing dataset ----
                                #スライドバーでサンプルの閾値を設定できるようにする 
                                p("下のスライダーでDiffの打席数閾値や, 成績の範囲を変更したり, チームを指定して, 最近の二軍成績の割に一軍でのPAが少ない選手を選択できます."),
                                p("初期タブ画面は該当する選手の2016年以後の二軍通算成績. 真ん中のタブで16-17データのプロットを、右のタブでこれらの選手の2012年度以後の、各年度での一軍と二軍の成績を確認できます."),
                                # Input: Selector for choosing dataset ----
                                selectInput(inputId = "dataset",
                                            label = "対象データセット (2016-17の二軍での最低打席数):",
                                            choices = c("200 PA以上", "100 PA以上")),
                                
                                sliderInput("slider1", "wOBA:",
                                            min = 0.2, max = 0.5, value = c(0.34, 0.5)),
                                sliderInput("slider2", "BBpct:",
                                            min = 0, max = 50, value = c(10, 50)),
                                sliderInput("slider3", "Kpct:",
                                            min = 0, max = 50, value = c(0, 15)),
                                sliderInput("slider5", "HRpct:",
                                            min = 0, max = 12.5, value = c(0, 12.5)),
                                sliderInput("slider4", "2軍でのPA - 1軍でのPA (16, 17年):",
                                            min = -1000, max = 1000, value = c(0, 1000)),
                                sliderInput("slider6", "TTO (いわゆるアダム・ダン率):",
                                            min = 0, max = 100, value = c(0, 100)),
                                selectInput(inputId = "team",
                                            label = "調べたいチーム:",
                                            choices = c("All", "Bs", "C", "D", "DB", "E", "F", "G", "H", "L", "M", "S", "T")),
                                
                                # Input: Numeric entry for number of obs to view ----
                                numericInput(inputId = "obs",
                                             label = "最大表示数:",
                                             value = 5000),
                                
                                helpText("成績は名前+チームごとに合算している. これは主に, 同一の登録名の別人の選手の成績を合計することを回避するため (外国人選手にありがち).
                                         このため移籍すると別人扱いになる. またNPB公式での登録名が変更された場合も、別人として扱っている (漢字の変更も別人, 例えば, 宮崎　敏郎 → 宮﨑　敏郎)."),
                                helpText("wOBAスケールは合わせていない簡易版 (Deltagraphs HPで示されている係数を利用した; Ref参照). "),
                                
                                width = 4
                                ),
                              # 画面に表示する
                              mainPanel(
                                # タブをつくって、そこにプロットと参考文献を配置する
                                tabsetPanel(type = "tabs",
                                            tabPanel("2016以後通算ファーム成績", DT::dataTableOutput("view")),
                                            tabPanel("プロット", plotOutput("plot1")),
                                            tabPanel("対象選手の各年度, 各レベル成績", DT::dataTableOutput("view2")),
                                            tabPanel("Ref", htmlOutput("refs"))
                                )
                              )
                              )
                            ),
                   tabPanel("Pitcher",
                            sidebarLayout(
                              position = ("left"),
                              fluid = TRUE,
                              
                              # 投手側のNavigation barの内容
                              sidebarPanel(
                                p("データはNPB公式から. 17/9/17時点での更新分. 
                                  右端のDiffは16年以後に関して、二軍でのIPから一軍でのIPを引いた数値."),
                                
                                # Input: Selector for choosing dataset ----
                                #スライドバーでサンプルの閾値を設定できるようにする 
                                p("下のスライダーでDiffのIP数閾値や, 成績の範囲を変更したり, チームを指定して, 最近の二軍成績の割に一軍でのIPが少ない選手を選択できます."),
                                p("初期タブ画面は該当する選手の2016年以後の二軍通算成績. 真ん中のタブで16-17データのプロットを、右のタブでこれらの選手の2012年度以後の、各年度での一軍と二軍の成績を確認できます."),
                                # Input: Selector for choosing dataset ----
                                selectInput(inputId = "dataset2",
                                            label = "対象データセット (2016-17の二軍での最低IP数):",
                                            choices = c("50 IP以上", "100 IP以上")),
                                

                                
                                sliderInput("slider1_2", "FIP:",
                                            min = 1.5, max = 5.5, value = c(0, 3.3)),
                                sliderInput("slider2_2", "BBpct:",
                                            min = 0, max = 50, value = c(0, 10)),
                                sliderInput("slider3_2", "Kpct:",
                                            min = 0, max = 50, value = c(20, 50)),
                                sliderInput("slider5_2", "K-BB:",
                                            min = 0, max = 30, value = c(15, 30)),
                                sliderInput("slider4_2", "2軍でのIP - 1軍でのIP (16, 17年):",
                                            min = -400, max = 400, value = c(000, 400)),
                                selectInput(inputId = "team2",
                                            label = "調べたいチーム:",
                                            choices = c("All", "Bs", "C", "D", "DB", "E", "F", "G", "H", "L", "M", "S", "T")),
                                # Input: Numeric entry for number of obs to view ----
                                numericInput(inputId = "obs2",
                                             label = "最大表示数:",
                                             value = 5000),
                                
                                br(),
                                
                                
                                helpText("成績は名前+チームごとに合算している. これは主に, 同一の登録名の別人の選手の成績を合計することを回避するため (外国人選手にありがち).
                                         このため移籍すると別人扱いになる. またNPB公式での登録名が変更された場合も、別人として扱っている (漢字の変更も別人, 例えば, 宮崎　敏郎 → 宮﨑　敏郎)."),
                                helpText("IPやDiffの数字がおかしいのは丸めてから足したり引いたりしているため. 大目に見てやってください."),
                                helpText("FIPはFangraphs方式で定数を計算して算出. 各年度, 1, 2軍ごとに定数を計算して使用している. "),
                                helpText("DERは失策を考慮に入れていない簡易版. 正確ではないので注意. チームDERの影響も排除していない."),
                                
                                width = 4
                                ),
                              # 画面に表示する
                              mainPanel(
                                # タブをつくって、そこにプロットと参考文献を配置する
                                tabsetPanel(type = "tabs",
                                            tabPanel("2016以後通算ファーム成績", DT::dataTableOutput("view3")),
                                            tabPanel("プロット", plotOutput("plot2")),
                                            tabPanel("対象選手の各年度, 各レベル成績", DT::dataTableOutput("view4")),
                                            tabPanel("Ref", htmlOutput("refs2"))
                                )
                              )
                              )
                            )
))
