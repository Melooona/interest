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
i_cal_ftn_2(계약일, 시작일, 결산일, 이자율테이블 or 상수, 윤년 or 평년, 연복리 or 일복리, 최저보증이율 테이블(optional))


