# Interest (update : 2020-05-06)


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



2-2. 이자율계산 함수  
i_cal_ftn_2(계약일, 시작일, 결산일, 이자율테이블 or 상수, 윤년 or 평년, 연복리 or 일복리, 최저보증이율 테이블(optional))  
   계약일 : AY년도 구분 기준일.  
   시작일 ~ 결산일 :  이자율 부리기간  
   이자율테이블 or 상수 (0 <= 상수 <= 1) : 이자율테이블은 (n * 3) 테이블. (p1, p2, p3) = (이율시작일, 이율종료일, 공시이율)  
   윤년 or 평년 : 윤년=분모 366 반영. 평년 = 분모 365 Fix  
   연복리 or 일복리 : 연복리 = (1 + d / 365 * i) ,  일복리 = (1+i)^(d/365)  
  최저보증이율테이블 : Optional var. 사용 시 'min_i_ftn' 테이블 사용.

ex)
> I_TB_test  
   공시부리시작일 공시부리종료일 적용이율  
1      2014-12-31     2015-02-28   0.0350  
2      2015-02-28     2015-03-31   0.0340  
3      2015-03-31     2015-04-30   0.0330  
4      2015-04-30     2015-05-31   0.0320  
5      2015-05-31     2015-07-31   0.0310  
6      2015-07-31     2015-08-31   0.0315  
7      2015-08-31     2015-10-31   0.0300  
8      2015-10-31     2015-11-30   0.0290  
9      2015-11-30     2016-02-29   0.0280  
10     2016-02-29     2019-12-31   0.0275  


계약일_2 <- as.Date(c('2014-10-05'),'%Y-%m-%d')  
시작일_2 <- as.Date(c('2014-10-05'),'%Y-%m-%d')  
결산일 <- as.Date(c('2019-12-31'),'%Y-%m-%d')  

> i_cal_공시_최저x_평년_일복리 <- i_cal_ftn_2(계약일_2, 시작일_2, 결산일, I_TB_test, '평년', '일복리')   
[1] 1.158402258  

  


