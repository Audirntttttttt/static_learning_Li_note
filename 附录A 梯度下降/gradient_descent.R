gradient_descent <- function(expression,target,treshold){
  #输入：
  #expression: expression()对象的数学表达式
  #target: 表达式所含的符号变量
  #treshold: 阈值
  #返回x*
  
  #封装提取变量
  env<-environment()
  
  x_final <- function(){
    xfi<-c()
    for(i in c(1:length(target))){
      xfi[i]<-get(target[i])
    }
    xfi
  }
  
  units<-length(target)
  #梯度函数，返回list，list的合集为梯度
  gradient_fx<-list()
  for(i in c(1:units)){
    gradient_fx[i]<- deriv(expression,target[i])
  }

  #初始化xk,取k=0
  set.seed(100)
  for(i in target){
    assign(i,sample(1:100,1),envir=env)
  }
  
  k<-0
  
  #梯度函数，返回list，list的合集为梯度
  
  gradient_fx<- deriv(expression,target)
  
  repeat{
    #复制一份xk
    x_ori<-x_final()
    #计算fx
    fx<-eval(expression,envir=env)
    #计算gk
    gk<-as.numeric(attributes(eval(gradient_fx,envir=env))[[1]])
    print(gk)
    #计算|gk|
    gk_mo<-sqrt(sum(gk^2))
    #如果梯度小于阈值，迭代结束
    if (gk_mo<treshold) {
      print(paste0("在第",k,"次迭代中，梯度小于阈值停止迭代"))
      return(x_ori)
    }
    #取反向梯度
    pk<-gk*(-1)
    
    #构建单向优化的函数
    fx_singl<-function(t){
      for(i in c(1:units)){
        assign(target[i],x_ori[i]+t*pk[i])
      }  
      Y<-eval(expression)
      Y
    }
    
    #计算此向梯度的最小步长，t>0
    fxk_t<-as.numeric(unlist(optimize(fx_singl,lower=0,upper=100)))
    for(i in c(1:units)){
      assign(target[i],x_ori[i]+fxk_t[1]*pk[i])
    }
    
    if((fxk_t[1]-fx)<treshold){
      print(paste0("在第",k,"次迭代中，迭代差小于阈值而结束"))
      return(x_final())
    }
    k<-k+1
    print(paste0("将开始第k次迭代"))
  }
}
