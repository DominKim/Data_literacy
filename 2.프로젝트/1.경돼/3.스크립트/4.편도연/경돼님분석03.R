library(data.table)
library(GGally)
library(lubridate)
library(tidyverse)
library(ggpmisc)
library(recipes)
library(gridExtra)

df_raw <- fread("c:/R/Competition/preprocessing_data.csv",header=T,encoding = 'UTF-8')
df <- df_raw
df %>% dim
##데이터 전처리
#1)경돼님이 댓글기능 막으셔서 좋아요 및 싫어요 NA처리 되어서 제거 후 진행.
df <- df %>% 
    drop_na()
#2)날짜 및 텍스트 전처리
df <- df %>% 
    mutate(published_at = ymd(published_at)) %>% 
    mutate(year = year(published_at),
           month = month(published_at),
           wday = wday(published_at,label = T)) %>% 
    select(-comments,-tags,-description) %>% 
    mutate(interest_rate = (likes+dislikes+comment_count)/view_count,
           total_interest = (likes+dislikes+comment_count)) #**변수추가**
#변수추가 이유 : likes, dislikes, comment_ct와 view_count의 상관관계가 유의하다.
#따라서 세 변수를 하나로 통합하여 interest라는 변수로 처리하였다.
#간략히 상관관계 확인하기
df %>% 
    select_if(is.numeric) %>%
    select(-published_time,-published_year,-published_month,-published_day) %>% 
    ggpairs()
#상관관계 결과 : view_ct와 유의미한 변수는 duration, interest
#주의할 점 : month_upload_ct는 이후 분석을 통해서 중요하다고 생각되나 해당 상관관계비교에선 유의미하지 않다고 나옴. month별로 그룹핑 된 값이기 떄문이라고 판단되어 이후 month별로 그룹핑 후 다시 상관관계 확인 요망.(결론 : 유의미한 변수임.)

#분석 진행 방향 : view_ct와 매우 유의한 interest 관련 변수들로 이상치 직접 판단 및 영상 특징 추출/정리
#HOW? 조회수가 높은 영상들을 interest_rate를 기준으로 알고리즘 등의 외적 요인의 의한 이상치인지 아니면 경돼님에 의한 TOP 영상인지 판단한다.
#외적 요인의 의한 높은 조회수 영상들을 이상치로 제거하고 조회수 TOP영상들의 특징을 조사해본다. 추출된 특징들로 앞으로 경돼님이 영상을 만드는데 참고할 수 있도록 제시함.
# + 추가적으로 낮은 조회수 + 낮은 interest의 영상들의 특징도 추출하여 앞으로 지양해야할 특징들을 제시한다.

#**분석 전, 월별로 그룹핑하여 upload_ct 변수 다시 확인해보기
#월별로 전처리 위한 작업
month_upload_ct <- df %>% 
    select(year,month,month_upload_count) %>% 
    group_by(year,month) %>% 
    summarise(mt_upload_ct = mean(month_upload_count)) %>% 
    arrange(year,month) %>% 
    select(mt_upload_ct)
a <- month_upload_ct[,2]
a

df3 <- df %>% 
    select(year,month,view_count,likes,dislikes,comment_count) %>% 
    group_by(year,month) %>% 
    summarise(mean_month_view = mean(view_count),
              mt_total_interest = sum(likes+dislikes+comment_count),
              mt_interest_rate = mean((likes+dislikes+comment_count)/view_count),
              total_view = sum(view_count)) %>%  
    arrange(year,month) %>% 
    bind_cols(a) %>% 
    mutate(date = paste(year,month) %>% ym)
df3 %>% head
df3 %>% ggpairs()
#분석 결과 - upload_ct는 중요한 변수이다.
#1)view_ct & upload_count 상관관계 유의미함.
cor.test(df3$mean_month_view,df3$mt_upload_ct) #0.4291062 /  p-value = 0.0007765
#2)total_interest,month_upload_count 상관관계 유의미함
cor.test(df3$mt_total_interest,df3$mt_upload_ct) #0.7014436 / p-value = 8.632e-10
#3)view_ct & total_interest 상관관계 유의미함.
cor.test(df3$mean_month_view,df3$mt_total_interest) #0.7575452 /p-value = 5.863e-12
#4)total_interest & interest_rate 상관관계 유의미함.
cor.test(df3$mt_total_interest,df3$mt_interest_rate)#-0.4810885 / p-value = 0.000132
#5)interest_rate  & view_ct 상관관계 유의미함.
cor.test(df3$mt_interest_rate,df3$mean_month_view)#-0.5614843  / p-value = 4.531e-06

#연도별 평균 view / interest_rate 구하기
df %>% 
    select(year,month,view_count,likes,dislikes,comment_count) %>%
    filter(year != 2016) %>% 
    group_by(year) %>% 
    summarise(yr_mean_view = mean(view_count),
              yr_interest_rate = mean((likes+dislikes+comment_count)/view_count)) %>%  
    arrange(year) %>% 
    ggplot(aes(year,yr_mean_view))+
    geom_bar(stat = 'identity')

df %>% 
    select(year,month,view_count,likes,dislikes,comment_count) %>%
    filter(year != 2016) %>% 
    group_by(year) %>% 
    summarise(yr_mean_view = mean(view_count),
              yr_interest_rate = mean((likes+dislikes+comment_count)/view_count),
              yr_total_interest = sum(likes+dislikes+comment_count)) %>% 
    arrange(year) %>% 
    ggplot(aes(year,yr_interest_rate))+
    geom_bar(stat = 'identity')
#   year yr_mean_view yr_interest_rate
#  2017       15155.           0.0282
#  2018       16365.           0.0258
#  2019        8860.           0.0281
#  2020        6709            0.0371
#  2021        3249.           0.0408

#연도별 데이터 시각화
df3 %>% 
    mutate(date = paste(year,month) %>% ym) %>%
    filter(year != 2016) %>% 
    select(date,everything()) %>% 
    ggplot(aes(date,mean_month_view))+
    geom_line()+
    scale_x_date(date_labels = "%b/%Y")+
    stat_peaks(colour = "red") + #극대점 빨간점으로 표시
    stat_peaks(geom = "text", colour = "red", #극대점 날짜("%Y-%m") 표시 
               vjust = -0.5, x.label.fmt = "%Y-%m") +
    stat_valleys(colour = "blue") +  #극소점 파란점으로 표시
    stat_valleys(geom = "text", colour = "blue", angle = 45,
                 vjust = 1.5, hjust = 1,  x.label.fmt = "%Y-%m")
#이상치로 판단되는 극대점 
#17 - 06 / 08 / 10 / 12
#18 - 01 / 04 / 08 / 10
#19 - 01 / 10 / 12
#20 - 07
df333 <- df %>% 
    select(title,interest_rate,view_count,year,month,published_day)
g1 <- df333 %>% 
    select(-title) %>% 
    group_by(year,month) %>% 
    summarise(total_view = sum(view_count),
              mean_view = sum(view_count)/n()) %>% 
    filter(year != 2016) %>% 
    mutate(date = paste(year,month) %>% ym) %>%
    ggplot(aes(date,total_view))+
    geom_bar(stat = 'identity')
g2 <- df333 %>% 
    select(-title) %>% 
    group_by(year,month) %>% 
    summarise(total_view = sum(view_count),
              mean_view = sum(view_count)/n()) %>% 
    filter(year != 2016) %>% 
    mutate(date = paste(year,month) %>% ym) %>%
    ggplot(aes(date,mean_view))+
    geom_bar(stat = 'identity')    

g3 <- df333 %>% 
    select(-title) %>% 
    group_by(year) %>% 
    summarise(mean_view = sum(view_count)/n()) %>% 
    filter(year != 2016) %>% 
    ggplot(aes(year,mean_view))+
    geom_bar(stat = 'identity')  

g4 <- df3 %>% 
    select(year,month,mt_interest_rate) %>% 
    filter(year != 2016) %>%
    group_by(year,month) %>% 
    summarise(mean11 = mean(mt_interest_rate)) %>% 
    mutate(date = paste(year,month) %>% ym) %>% 
    ggplot(aes(date,mean11))+
    geom_bar(stat = 'identity')

grid.arrange(g1,g2,g3,g4)

#===============================================================================
#1)2017년 쪼개보기
delete_var1 <- c('유도(Judo)하는 사람이 주짓수(Jiu-jitsu) 대회 나갔을 때','경돼 주짓수 대회 참가 vlog (long edited version)','여자 운동 동기부여(Workout Motivation for Women)','[경돼 유도]생활 체육과 엘리트 체육의 차이???!','추석 연휴, 그녀가 돌아왔다(Full version)','여자가 유도를 배우면?','[경돼 유도] 자유연습 기술 편집(judo randori)','그녀가 돌아왔다(티저 영상)','#1 헬스장에서 훈수두는 사람 대처법','일본어학과 소녀와의 운동','근육은 어떻게 성장할까? │한글 자막','유도 프로선수와 아마추어의 차이... 넘사벽?!')
#위에서 확인했던 연도별 이상치가 해당 연도에서 진짜 이상치가 많은지(top 조회수 영상이 많은지)먼저 확인
df3 %>%
    filter(year == 2017) %>% 
    arrange(mt_interest_rate) %>% 
    select(month, total_view, mean_month_view,mt_interest_rate) %>% 
    arrange(desc(total_view))
df %>%
    select(title,year,month,interest_rate,view_count) %>% 
    filter(year == 2017 & month == 8) %>% 
    arrange(desc(view_count))
#경돼님 주짓수 영상 알고리즘 영향 존재. > 이상치로 판단하고 제거.
df %>%
    select(title,year,month,interest_rate,view_count) %>% 
    filter(year == 2017 & month == 6) %>% 
    arrange(desc(view_count))
#설킴님 영상 이상치로 판단. 
df %>%
    select(title,year,month,interest_rate,view_count) %>% 
    filter(year == 2017 & month == 10) %>% 
    arrange(desc(view_count))
#1:5 이상치 처리 why? 엘리트 체육, 그녀, 유도(알고리즘/유행 영향이 있는듯)
df %>%
    select(title,year,month,interest_rate,view_count) %>% 
    filter(year == 2017 & month == 12) %>% 
    arrange(desc(view_count))
#1:2 제거 

#2018년 쪼개보기 - 01 / 04 / 06 /08 / 10
delete_var2 <- c('#2 언더아머 논란 종결, 직접 매장에 찾아가다!','다시 돌아온 그녀, 세번째 이야기','다시 돌아온 그녀(무편집본)','이제 언더아머 직원도 3대 500을 안다!│다시 찾은 언더아머','경돼 X 왕기춘, 월클의 벽에 도전하다!!(오늘 나는 깃털이었다.)','거폭도 인정한 유도 국가대표 상비군의 위엄... 생활체육의 한계!?','주짓수 가서 이런 기술 쓰면 안됩니다...','펑티모의 고양이송... 가장 현실적인 리액션(Reaction)')
df3 %>%
    filter(year == 2018) %>% 
    arrange(mt_interest_rate) %>% 
    select(month, total_view, mean_month_view,mt_interest_rate) %>% 
    arrange(desc(total_view))
#위 시계열 그래프에서 확인했듯이 탑 5와 이상치가 유사함 진행 ㄱㄱ
df %>%
    select(title,year,month,interest_rate,view_count) %>% 
    filter(year == 2018 & month == 1) %>% 
    arrange(desc(view_count))
#3영상 제거 why?언더아머로 인한 알고리즘, 그녀2,3
df %>%
    select(title,year,month,interest_rate,view_count) %>% 
    filter(year == 2018 & month == 4) %>% 
    arrange(desc(view_count))
#언더아머영상 1만 삭제, 2:4 뽀종, 키다리형 합방 영상이지만 경돼님에 대한 관심도 많음.
#해당 시기 해당 유튜버들은 5만 정도밖에 되지않음.(대기업 x, 서로의 영향 크게 X)
df %>%
    select(title,year,month,interest_rate,view_count) %>% 
    filter(year == 2018 & month == 6) %>% 
    arrange(desc(view_count))
#데드리프트의 2가지 셋업? 헬스 꿀팁 챙겨가자!(feat. 관장님) 조회수 81735 이상치에 해당하나 해당 영상을 확인하면 특별한 알고리즘 유입 없고 경돼님의 운동영상 및 꿀팁 영상임. 이는 외부적인 요인에 의한 것이 아니라고 판단하여 이상치 처리 X
df %>%
    select(title,year,month,interest_rate,view_count) %>% 
    filter(year == 2018 & month == 8) %>% 
    arrange(desc(view_count))
#1:3 삭제, 왕기춘, 유도 국가대표 상비군, 주짓수(알고리즘 유입, 어그로 o by 댓글 싸움)
df %>%
    select(title,year,month,interest_rate,view_count) %>% 
    filter(year == 2018 & month == 10) %>% 
    arrange(desc(view_count))
#펑티모의 고양이송... 가장 현실적인 리액션(Reaction) 삭제, 유행에 의한 알고리즘

#2019년 쪼개보기 - 01 / 05 / 12 + 08 / 04
delete_var3 <- c('내추럴, 스테로이더?','#1운동뺏기│3대 7000 뽀종에게 배워보는 등 운동 2019','회사다니면서 55일 동안 다이어트(-9kg 감량)')
df3 %>%
    filter(year == c(2019)) %>% 
    arrange(mt_interest_rate) %>% 
    select(month, total_view, mean_month_view,mt_interest_rate) %>% 
    arrange(desc(total_view))
#8월 이상치 추측, 4월 추가 분석 진행
df %>%
    select(title,year,month,interest_rate,view_count) %>% 
    filter(year == 2019 & month == 1) %>% 
    arrange(desc(view_count))
#내추럴, 스테로이더? 약투 이슈 당시 문제, 이상치로 판단 제거 
df %>%
    select(title,year,month,interest_rate,view_count) %>% 
    filter(year == 2019 & month == 4) %>% 
    arrange(desc(view_count))
#이상치 X 외적요소 X 
df %>%
    select(title,year,month,interest_rate,view_count) %>% 
    filter(year == 2019 & month == 5) %>% 
    arrange(desc(view_count))
##1운동뺏기│3대 7000 뽀종에게 배워보는 등 운동 2019 제목 어그로로 인한 외적 유입
df %>%
    select(title,year,month,interest_rate,view_count) %>% 
    filter(year == 2019 & month == 8) %>% 
    arrange(desc(view_count))
#이상치 X 외적 요인 X 조회수 가장 많은 short영상 존재. 현재의 short영상과의 개념은 다르지만 조회수 top
df %>%
    select(title,year,month,interest_rate,view_count) %>% 
    filter(year == 2019 & month == 12) %>% 
    arrange(desc(view_count))
#직장인도 유도대회에 입상할 수 있을까?! - 이상치인지 아닌지 판단하기 어렵다.. 유명인이 안나오는 경돼님 유도 영상도 전부다 이상치 처리해야하나?

#20 - 07
df3 %>%
    filter(year == c(2020)) %>% 
    arrange(mt_interest_rate) %>% 
    select(month, total_view, mean_month_view,mt_interest_rate) %>% 
    arrange(desc(total_view))
df %>%
    select(title,year,month,interest_rate,view_count) %>% 
    filter(year == 2020 & month == 7) %>% 
    arrange(desc(view_count))
#회사다니면서 55일 동안 다이어트(-9kg 감량) - 흥미도도 매우 낮은 편이며 알고리즘의 영향을 받은듯 싶다. 이상치 처리

#쪼개보기 끝
#=================================================================================
#이상치 제거 하기
delete_var <- c('유도(Judo)하는 사람이 주짓수(Jiu-jitsu) 대회 나갔을 때','경돼 주짓수 대회 참가 vlog (long edited version)','여자 운동 동기부여(Workout Motivation for Women)','[경돼 유도]생활 체육과 엘리트 체육의 차이???!','추석 연휴, 그녀가 돌아왔다(Full version)','여자가 유도를 배우면?','[경돼 유도] 자유연습 기술 편집(judo randori)','그녀가 돌아왔다(티저 영상)','#1 헬스장에서 훈수두는 사람 대처법','일본어학과 소녀와의 운동','근육은 어떻게 성장할까? │한글 자막','유도 프로선수와 아마추어의 차이... 넘사벽?!','#2 언더아머 논란 종결, 직접 매장에 찾아가다!','다시 돌아온 그녀, 세번째 이야기','다시 돌아온 그녀(무편집본)','이제 언더아머 직원도 3대 500을 안다!│다시 찾은 언더아머','경돼 X 왕기춘, 월클의 벽에 도전하다!!(오늘 나는 깃털이었다.)','거폭도 인정한 유도 국가대표 상비군의 위엄... 생활체육의 한계!?','주짓수 가서 이런 기술 쓰면 안됩니다...','펑티모의 고양이송... 가장 현실적인 리액션(Reaction)','내추럴, 스테로이더?','#1운동뺏기│3대 7000 뽀종에게 배워보는 등 운동 2019','회사다니면서 55일 동안 다이어트(-9kg 감량)')
#length(delete_var) #23개
#597 -23 =575 이상치 제거 잘됌.
df22 <- df %>% 
    select(title,interest_rate,view_count,year,month,published_day) %>% 
    filter(!(title %in% delete_var))

g11 <- df22 %>% 
    select(-title) %>% 
    group_by(year,month) %>% 
    summarise(total_view = sum(view_count),
              mean_view = sum(view_count)/n()) %>% 
    filter(year != 2016) %>% 
    mutate(date = paste(year,month) %>% ym) %>%
    ggplot(aes(date,total_view))+
    geom_bar(stat = 'identity')
g22 <- df22 %>% 
    select(-title) %>% 
    group_by(year,month) %>% 
    summarise(total_view = sum(view_count),
              mean_view = sum(view_count)/n()) %>% 
    filter(year != 2016) %>% 
    mutate(date = paste(year,month) %>% ym) %>%
    ggplot(aes(date,mean_view))+
    geom_bar(stat = 'identity')    

g33 <- df22 %>% 
    select(-title) %>% 
    group_by(year) %>% 
    summarise(mean_view = sum(view_count)/n()) %>% 
    filter(year != 2016) %>% 
    ggplot(aes(year,mean_view))+
    geom_bar(stat = 'identity')  

g44 <- df %>% 
    select(title,year,month,view_count,likes,dislikes,comment_count) %>% 
    filter(!(title %in% delete_var)) %>% 
    group_by(year,month) %>% 
    summarise(mean_month_view = mean(view_count),
              mt_total_interest = sum(likes+dislikes+comment_count),
              mt_interest_rate = mean((likes+dislikes+comment_count)/view_count),
              total_view = sum(view_count)) %>%  
    arrange(year,month) %>% 
    bind_cols(a) %>% 
    mutate(date = paste(year,month) %>% ym)%>% 
    ggplot(aes(date,mt_interest_rate))+
    geom_bar(stat = 'identity')

grid.arrange(g11,g22,g33,g44)

df %>% 
    select(title,interest_rate,view_count,year,month,published_day,view_count,likes,dislikes,comment_count) %>% 
    filter(!(title %in% delete_var)) %>% 
    select(year,month,view_count,likes,dislikes,comment_count) %>%
    filter(year != 2016) %>% 
    group_by(year,month) %>% 
    summarise(mt_mean_view = mean(view_count),
              mt_total_view = sum(view_count),
              mt_interest_rate = mean((likes+dislikes+comment_count)/view_count),
              mt_total_interest = sum(likes+dislikes+comment_count),
              mt_total_upload = n()) %>% ggpairs() 

#인사이트!
#유의미한 변수 1 : interest_rate 높히기 위해서 기존 시청자들이 좋아하는 컨텐츠 위주로 업로드를 하기.
#기존 시청자들이 좋아하는 컨텐츠 : 운동영상 + 일상 10분정도. > 이번 분석에서는 높은 조회수들 및 interest_rate를 고려하여 이상치를 제거하고 이상치가 아닌 top 조회수 영상들의 영상 구성 방식, 내용을 확인하여 공통점을 수기로 작성하였다.
#이부분에 대한 수치화는 추후 추가 분석이 필요함.
#유의미한 변수 2 : 업로드 횟수 올리기


#===============================================================================

#조회수 낮고 흥미도도 낮은 영상들에서 이유 발견해서 지향점으로 제안하기
#유의미한 결과 없었음.
a1 <- df %>% 
    select(title,year,month,view_count,likes,dislikes,comment_count) %>% 
    mutate(interest_rate = (likes+dislikes+comment_count)/view_count,
           total_interest = (likes+dislikes+comment_count)) %>% 
    mutate(date = paste(year,month) %>% ym) %>% 
    filter(year == 2017) %>% 
    filter(view_count < 4000) %>% 
    select(title,year,month,view_count,total_interest,interest_rate) %>% 
    arrange(view_count)
a2 <- df %>% 
    select(title,year,month,view_count,likes,dislikes,comment_count) %>% 
    mutate(interest_rate = (likes+dislikes+comment_count)/view_count,
           total_interest = (likes+dislikes+comment_count)) %>% 
    mutate(date = paste(year,month) %>% ym) %>% 
    filter(year == 2018) %>% 
    filter(view_count < 5000) %>% 
    select(title,year,month,view_count,total_interest,interest_rate) %>% 
    arrange(view_count)

a3 <- df %>% 
    select(title,year,month,view_count,likes,dislikes,comment_count) %>% 
    mutate(interest_rate = (likes+dislikes+comment_count)/view_count,
           total_interest = (likes+dislikes+comment_count)) %>% 
    mutate(date = paste(year,month) %>% ym) %>% 
    filter(year == 2019) %>% 
    filter(view_count < 5000) %>% 
    select(title,year,month,view_count,total_interest,interest_rate) %>% 
    arrange(view_count)
a4 <- df %>% 
    select(title,year,month,view_count,likes,dislikes,comment_count) %>% 
    mutate(interest_rate = (likes+dislikes+comment_count)/view_count,
           total_interest = (likes+dislikes+comment_count)) %>% 
    mutate(date = paste(year,month) %>% ym) %>% 
    filter(year == 2020) %>% 
    filter(view_count < 4000) %>% 
    select(title,month,year,view_count,total_interest,interest_rate) %>% 
    arrange(view_count)


