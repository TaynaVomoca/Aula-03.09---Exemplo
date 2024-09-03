##################################
#Pacotes
##################################
install.packages("kmed")
library(kmed)
library(dplyr)
install.packages("sjPlot")
library(sjPlot)
library(ggplot2)

#importando dataset

dados <- heart

###########################
# 1 - preparação do dados
###########################
#selecionando colunas

dados_novos <- dados %>% select(age, sex, cp, thalach, class)

#renomeando colunas
dados_novos<- dados_novos %>% rename(idade = age, sexo = sex, dor_no_peito = cp,
                                     freq_maxima = thalach, doenca_cardiaca = class)

#Renomeando valores para a variavel sexo
dados_novos$sexo <- factor(dados_novos$sexo, levels = c(FALSE, TRUE),
                           labels = c("feminino", "masculino"))

#Renomeando valore para dor no peito
dados_novos$dor_no_peito <- factor(dados_novos$dor_no_peito, levels = 1:4, 
                                   labels = c("angina típica", "angina atípica", "dor não-anginal", "assintomatico"))

#Codificando par doente e não-doente
dados_novos$doenca_cardiaca <- ifelse(dados_novos$doenca_cardiaca == 0, 0, 1)

#Passando para fator
dados_novos$doenca_cardiaca <- factor(dados_novos$doenca_cardiaca, 
                                      levels = c(0,1), labels = c("sem doença", "com doença"))

###################################################################################
# 2 - Modelo 1 - Regresão logítica simples - Variavel independente quantitativa
###################################################################################

ggplot(dados_novos, aes(y = idade, fill = doenca_cardiaca)) + geom_boxplot()

dados_novos %>% group_by(doenca_cardiaca) %>% summarise(media = mean(idade))

modelo_1 <- glm(doenca_cardiaca ~ idade, data = dados_novos, family = "binomial")
summary(modelo_1)

#Quando o coeficiente é igual a zero, x e y são independentes
#Quando o coeficiente é > 0, a probabilidade de y = 1 (doente) aumenta com x 
#Quando o coeficiente é < 0, a probabilidade de y = 1 (doente) diminui com x

#quantificano a relação
exp(coef(modelo_1)["idade"])

#Um ano extra de vida aumenta a chance de desenvolver uma doença cardiaca por um fator de 1,05

#Analise do intercepto
exp(coef(modelo_1)[1])/(1 + exp(coef(modelo_1)[1]))

#Uma pessoa de 0 ano tem uma chance de desenvolver doença cardíaca de 0,04

#Predizendo a probabilidade de ter doença
novo_paciente <- data.frame(idade = 30)
predict(modelo_1, novo_paciente, type = "response")

#Uma pessoa de 30 anos tem chance de 18,78% de ter doença cardíaca

plot_model(modelo_1, type = "pred", terms = "idade")

###################################################################################
# 2 - Modelo 2 - Regresão logítica simples - Variavel independente qualitativa
###################################################################################

#analie grafica
ggplot(data = dados_novos, aes(x = sexo, fill = doenca_cardiaca)) + geom_bar()

#teste qui-quadrado para independencia
chisq.test(table(dados_novos$doenca_cardiaca, dados_novos$sexo))

# calor p <0.05. Podemo rejeitar a hipótese de independencia das variaveis e supor
# que elas sejam associadas com um nível de confiança de 95%.

modelo_2 <- glm(doenca_cardiaca ~ sexo, data = dados_novos, family = "binomial")
summary(modelo_2)

exp(coef(modelo_2)["sexomasculino"])

#Uma pesoa do sexo masculino tem o fator multiplicado por 3,574 de deenvolver doença cardíaca

plot_model(modelo_2, type = "pred", terms = "sexo")

###################################
# 3 - Modelo 3 - Várias variaveis
###################################

modelo_3 <- glm(doenca_cardiaca ~ ., data = dados_novos, family = "binomial")
summary(modelo_3)

