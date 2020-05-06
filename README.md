# interest


1. 설치방법 (copy and paste)

install.packages("devtools") #github package 설치 시 필요  
library(devtools)
 
install_github("Melooona/interest")   
library(Interest)  
  
제약조건 :  dplyr, lubridate 패키지 필요  
날짜 포맷 : Date.  
UTF-8 encoding  


2. 사용방법  
2-1. 최저보증이율테이블 생성  
min_i_ftn(계약일, 변화구간 vector, 구간최저보증이율 vector)  

ex)  
계약일_1 <- as.Date(c('2014-12-31'),'%Y-%m-%d')  
min_calendar_1 <- min_i_ftn(계약일_1, c(5,10), c(0.05, 0.03))  #계약일_1 : 2014-12-31

          V1         V2      V3  
1 2014-12-31   2019-12-31   0.05  
2 2019-12-31   2029-12-31   0.03  
3 2029-12-31   9999-12-31   0.00  




i_cal_ftn_2(계약일, 시작일, 결산일, 이자율테이블 or 상수, 윤년 or 평년, 연복리 or 일복리, 최저보증이율 테이블(optional))



