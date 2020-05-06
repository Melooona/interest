# 1. AYtb ftn : 결산일까지 AY연도 생성
#   계약일 기준 AY 달력 만들기 (x=계약일)
#   제약 : 결산일 변수설정
AYtb_ftn<- function(x){
  df <- data.frame()
  k <- 1
  while(x < 결산일){
    df[k,1] <- x
    df[k,2] <- x + years(1)
    df[k,3] <- df[k,2]-df[k,1]
    df[k,4] <- k-1
    x <- x + years(1)
    k <- k+1
  }
  df
}


# 2. 부리달력 함수 : AYtb, 이율테이블 바탕으로 부리달력 생성
#   (시작일, 종료일, AYtb, 이율테이블 or 상수)
calendar_ftn <- function(x, endday, y, z){
  if(length(z)==1){
    i <- min(which(y[,2] > x))
    j <- max(which(y[,2] < endday))
    df <- data.frame()
    df[1,1] <- x
    for(k in i:j){
      df[k-i+1,2] <- y[k,2]
      df[k-i+1,3] <- df[k-i+1,2] - df[k-i+1,1]
      df[k-i+1,3] <- df[k-i+1,2] - df[k-i+1,1]
      df[k-i+2,1] <- df[k-i+1,2]
    }
    df[j-i+2,2] <- endday
    df[j-i+2,3] <- df[j-i+2,2] - df[j-i+2,1]
  }
  else{
    k <- 1
    a <- 1
    b <- 1
    df <- data.frame()
    df[k,1] <- x
    while (df[k,1] < endday){
      if(z[a,2] < y[b,2]){
        df[k,2] <- z[a,2]
        a <- a+1}
      else{
        df[k,2] <- y[b,2]
        b <- b+1
      }
      df[k,3] <- df[k,2] - df[k,1]
      k <- k+1
      df[k,1] <- df[k-1,2]
    }
    df <- df[-length(df[,1]),]
  }
  df
}


# 3 최저보증테이블 함수 : 최저보증 테이블 생성
# (x,y,z) : (계약일, 벡터_최저보증구분점, 벡터_최저보증이율)
min_i_ftn <- function(x,y,z){
  df <- data.frame()
  k <- length(y)
  i <- 1
  while(i <= k){
    df[i,1] <- x
    df[i,2] <- x + years(y[i])
    df[i,3] <- z[i]
    
    x <- df[i,2]
    i <- i+1
  }
  df[i,1] <- df[i-1,2]
  df[i,2] <- '9999-12-31'
  df[i,3] <- 0
  df
}





# 4 최저보증 반영한 Calendar ftn
# 제약 : calendar ftn 존재해야 함 
# 이율테이블 대신 고정이율 사용할 때 최저보증이율은 없다고 가정하고 만든 함수임
# 따라서 상수이율 적용 시 최저보증이율은 적용되지 않음.

# (시작일, 종료일, AYtb, 이율테이블or상수, 최저보증테이블or null)

calendar_ftn_2 <- function(x,endday, y, z, w){
  if(missing(w)||length(z)==1){
    calendar <- calendar_ftn(x, endday, y, z)
  }
  else{
    temp <- calendar_ftn(x, endday, y, z)
    df <- data.frame()
    i <- 1
    a <- 1
    b <- 1
    df[i,1] <- temp[i,1]
    while(df[i,1] < endday){
      if(temp[a,2] < w[b,2]){
        df[i,2] <- temp[a,2]
        a <- a+1
      }
      else{
        df[i,2] <- w[b,2]
        b <- b+1
      }
      df[i,3] <- df[i,2] - df[i,1]
      #print(i)
      #print(df)
      i <- i+1
      df[i,1] <- df[i-1,2]
    }
    df <- df[-length(df[,1]),]
  }
}

# days 가 0 인 차이 생김. else 가 equal 부분도 포함하기 때문.
# 계산상 차이가 없기에 그냥 사용함



# 5 
# (최저보증반영calendar, Aytb, 이율테이블, 최저보증테이블 )
AY_i_ftn_2 <- function(x, y, z, w){
  df <- data.frame()
  # AY 일수, 부리해수 카운팅
  j <- length(x[,1])
  i <- 1
  a1 <- 1
  while (i <= j){
    if(x[i,2] <= y[a1,2]){
      df[i,1] <- y[a1,3]
      df[i,2] <- y[a1,4]
      a1 <- 1
      i <- i+1}
    else{
      a1 <- a1+1
      i <- i
    }
  }
  # 이자율 매칭
  if(length(z)==1){
    df[ ,3] <- z
  }
  else{
    i2 <- 1
    a2 <- 1
    while (i2 <= j){
      if(x[i2,2] <= z[a2,2]){
        df[i2, 3] <- z[a2,3]
        a2 <- 1
        i2 <- i2+1
      }
      else{
        a2 <- a2+1
        i2 <- i2
      }
    }
  }
  # 최저보증 이자율 매칭, 최저보증 없으면 위 산출값 출력
  if(missing(w)){
    df[,4] <- 0
  }
  else{
    i3 <- 1
    a3 <- 1
    while(i3 <= j){
      if(x[i3,2] <= w[a3,2]){
        df[i3,4] <- w[a3,3]
        a3 <- 1
        i3 <- i3+1
      }
      else{
        a3 <- a3 +1 
        i3 <- i3
      }
    }
  }
  # 최저보증이율 적용
  df$max <- apply(df[,3:4], 1, max)
  df
}



# 6

i_cal_ftn_2 <- function(w, x, endday,  z, c, d, e){
  
  y <- AYtb_ftn(w)
  
  temp_1 <- calendar_ftn_2(x, endday, y, z, e)
  colnames(temp_1) <- c('부리시작일', '부리종료일', '부리일수')
  temp_2 <- AY_i_ftn_2(temp_1, y, z, e)
  colnames(temp_2) <- c('AY일수', '경과년','공시이율','최저보증이율','연적용이율')
  temp <- cbind(temp_1, temp_2)
  temp$부리일수 <- as.numeric(temp$부리일수)
  temp$AY일수 <- as.numeric(temp$AY일수)
  
  ifelse(c == '평년', temp$AY일수 <- 365, temp$AY일수 <- temp$AY일수 )
  #print(temp)
  
  if(d=='연복리'){
    temp$일이자율 <- temp$연적용이율 * temp$부리일수 / temp$AY일수
    a <- temp %>% group_by(경과년) %>% summarise(sum(일이자율))
    a_2 <- a[,2]+1
    a_3 <-prod(a_2)
    #print(temp)
    print(a_3)
  }
  else{
    temp$일이자율 <- (temp$연적용이율+1)^(temp$부리일수/temp$AY일수)
    a_3 <- prod(temp$일이자율)
    print(a_3)
  }
}


