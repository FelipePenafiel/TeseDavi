# Pacote fornece funções para ler dados em planilhas do Excel 
library(readxl)

# Ler arquivo 
dados <- read_excel("C:/Pessoal/Mestrado/Regressao/correlacao/prova2.xlsx")

# Visualização dados
print(dados, n=29)
summary(dados)

# Diagrama de dispersão
pairs(dados)

# Stepwise
Y <- dados$`Captura(nº)`
X1 <- dados$`Temperatura (ºC)`
X2 <- dados$`Precipitação (mm)`
cor(dados)

#Entrando com o Modelo Nulo (Y = B0) e com o Modelo Completo (Y = B0+B1X1+B2X2)
MN = lm(Y ~ 1, data=dados)
MC = lm(Y ~ X1 + X2, data=dados)

# Método: Passo a passo - "Stepwise", utilizando o critério AIC (k=2)
step(MC, direction="both", k=2, trace=1)
step(MN, scope=list(lower=MN,upper=MC), data = mult, direction="both", k=2, trace=1)

# Regressão linear com modelo selecionado
mod <- lm(Y~X1+X2)
anova(mod)
summary(mod)

# resíduos ordinários
res_ord=residuals(mod);res_ord

# resíduos estudentizados internamente
res_pad=rstandard(mod);res_pad

# rs(i) - Resíduo estudentizado externamente
res_stud=rstudent(mod);res_stud

# matriz X e vetor Y
X=model.matrix(mod);X
Y=as.matrix(dados$`Captura(nº)`);Y

# Pontos inconsistentes - rs(i) grande
res_stud
n=length(Y)
p=ncol(X)
alpha=0.05/(2*n)
t_alpha=qt(alpha,n-p-1,lower.tail=F);t_alpha

# Pontos de alavanca
H=X%*%solve(t(X)%*%X)%*%t(X)
hii=diag(H);hii
n=length(Y)
p=ncol(X)
hbarra=p/n
pad_hii=(3*p)/n;pad_hii

# Pontos influentes
infl <- influence.measures(mod);infl
summary(infl)

# Normalidade resíduos
par(mfrow=c(2,2))
plot(mod)
residuos <- resid(mod)
shapiro <- shapiro.test(residuos)
print (shapiro)

# Homocedasticidade (Breusch-Pagan)
library(lmtest)
bptest(mod)





