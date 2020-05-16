#Wilcoxon順位和の確率分布を求める

#求めたい順位和分布の全サンプルのサイズをm, 一方のグループのサンプルのサイズをnとする。
#今回は、m=10,n=5として確かめる。
m <- 10
n <- 5

#1から10までの順位を生成
data <- c(1:m)

#組み合わせを生成
a <- t(combn(1:m,n))

#差を入れておくベクトルを定義
difference <- c()

#考えられる順位和の差を求める
for (i in 1:choose(m, n)){
  group1 <- data[a[i,]]
  difference[i] <- sum(group1) - (sum(data)-sum(group1))
}

#探索しやすくするためにdifferenceをソートする
difference <- sort(difference)

#確率(割合)を保管するベクトル定義
prob <- c()

#ある差を取る種類の個数を記録する変数を定義
cnt <- 0

#差の種類が全体で何種類あるかの個数を変数で定義
number <- 1

#数える時に使う変数を定義
x <- min(difference)

#便宜と計算量削減のためdifferenceの長さを記録しておく変数を定義
len <- length(difference)


for (j in 1:len){
  if(x==difference[j]){
    cnt=cnt+1
  }
  else{
    prob[number]=cnt/len
    x=difference[j]
    cnt=1
    number=number+1
  }
  if (j==len){
    prob[number]=cnt/len
  }
}
prob
x=0:25
barplot(prob, names.arg=c(0:25), col=ifelse(x>=35-15,"red","grey"))