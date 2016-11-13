#---------------
#SLR

# casual
slr_cas<-lm(formula=casual~.,data=train_cas_data)
summary(slr_cas)
slr_cas.pred=predict(slr_cas,test)
#slr_cas.rmse = sqrt(mean((test$casual - slr_cas.pred)^2))
#slr_cas.rmse

# registered
slr_reg<-lm(formula=registered~.,data=train_reg_data)
summary(slr_reg)

# count
slr_cnt<-lm(formula=cnt~.,data=train_cnt_data)
summary(slr_cnt)



#--------------
#LASSO
library(glmnet)

# casual ##############################################
#X and Y
cas_x = model.matrix(casual~.-1,data=train_cas_data)
cas_y = train_cas_data$casual

#model, plot, and coefficients
cas.lasso=cv.glmnet(cas_x,cas_y,type.measure="mse")
plot(cas.lasso)
coef(cas.lasso)

#ideal lambda value
cas_lam=cas.lasso$lambda.min
cas_lam
best_cas.lasso=glmnet(cas_x,cas_y,alpha=1)
predict(best_cas.lasso,s=cas_lam,type="coefficients")

#error
sqrt(cas.lasso$cvm[cas.lasso$lambda == cas.lasso$lambda.1se])



# registered ##########################################
#X and Y
reg_x = model.matrix(registered~.-1,data=train_reg_data)
reg_y = train_reg_data$registered

#model, plot, and coefficients
reg.lasso=cv.glmnet(reg_x,reg_y,type.measure="mse")
plot(reg.lasso)
coef(reg.lasso)

#ideal lambda value
reg_lam=reg.lasso$lambda.min
reg_lam
best_reg.lasso=glmnet(reg_x,reg_y,alpha=1)
predict(best_reg.lasso,s=reg_lam,type="coefficients")

#error
sqrt(reg.lasso$cvm[reg.lasso$lambda == reg.lasso$lambda.1se])



# count ##############################################
#X and Y
cnt_x = model.matrix(cnt~.-1,data=train_cnt_data)
cnt_y = train_cnt_data$cnt

#model, plot, and coefficients
cnt.lasso=cv.glmnet(cnt_x,cnt_y,type.measure="mse")
plot(cnt.lasso)
coef(cnt.lasso)

#ideal lambda value
cnt_lam=cnt.lasso$lambda.min
cnt_lam
best_cnt.lasso=glmnet(cnt_x,cnt_y,alpha=1)
predict(best_cnt.lasso,s=cnt_lam,type="coefficients")

#error
sqrt(cnt.lasso$cvm[cnt.lasso$lambda == cnt.lasso$lambda.1se])
