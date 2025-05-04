install.packages("mltools")
install.packages("stringi")
install.packages("stringr")
install.packages("shiny")
install.packages("shinythemes")
install.packages("shinydashboard")
install.packages("rtools")
install.packages("dplyr")
install.packages("htmltools")
install.packages("ggplot2")
install.packages("esquisse")
install.packages("broom")

library(readxl)
library(lubridate)
library(mltools)
library(caret)
library(stringi)
library(stringr)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse) 
library(dplyr)       
library(stats)
library(esquisse)
library(rmarkdown)
library(ggplot2)
library(knitr)
library(tinytex)
library(shiny)
library(Hmisc)
library(broom)


telecomData <- read.csv("D:/Universidade Europeia - Pós Graduação/Data Science for Business/5. Modelação e Visualização/WA_Fn-UseC_-Telco-Customer-Churn.csv")

# PARTE 1 -------------------------------
# 1.A. Identificar valores em falta e proceder à sua substituição. Justifique o 
#método de substituição que realizou.

head(telecomData)
str(telecomData)

is.na(telecomData)%>%
  sum()

valoresDuplicados <- duplicated(telecomData)|>
  sum()
valoresDuplicados

summary(telecomData)


# Numa abordagem inicial, optei por analisar a estrutura do dataframe importado,
#um dataframe que inclui dados demográficos sobre os clientes da empresa como 
#género, idade, estado civil e dependentes. Assim como, clientes que cancelaram 
#contrato com a empresa no último mês, os serviços que cada cliente adquiriu 
#(telefone, várias linhas, internet, segurança online, backup online, proteção 
#de dispositivos, apoio técnico, e serviços de streaming) e informação de conta 
#do cliente (nomeadamente, longevidade do cliente, tipo de contrato, método de pagamento, 
#fatura digital ou em papel, despesas mensais e despesas totais). 
#Na identificação de valores em falta, conseguimos perceber através do comando 
#sum(is.na(telecomData)) que o total de valores em falta são 11, e não há nenhum
#valor duplicado.

colSums(is.na(telecomData)) 
which(is.na(telecomData$TotalCharges))

NAtelecomData <- telecomData %>% filter(is.na(telecomData$TotalCharges))
NAtelecomData



# De seguida, é importante perceber onde estão localizados os valores em falta, 
#e analisar uma possível relação entre esses valores em falta e os restantes. Numa 
#primeira análise não foi possível encontrar um padrão entre eles, sendo a proporção de valores em 
#falta mínima, comparando com a quantidade de valores presentes (11 para 7032), entende-se
#então estarmos perante valores em falta completamente ao acaso. 
#Como estamos perante missing values MCAR, é possível utilizar qualquer um dos
#métodos referidos pela literatura: omissão de valores em falta, imputação de 
#valores médios ou medianos, regressão ou imputação múltipla, desde que a imputação
#ou eliminação não enviese a análise. 
#Para este caso, opta-se por imputar valores medianos uma vez que a média e a 
#mediana, neste caso ,têm valores bastante diferentes devido ao espetro de valores
#que a variável Total Charges assume. Com valores medianos, imputamos valores 
#que se observam mais vezes nesta variável sem enviesar resultados.


telecomDataClean <- telecomData %>%
  mutate_if(is.character, as.factor)

telecomDataClean$TotalCharges<-impute(telecomDataClean$TotalCharges, median)

is.na(telecomDataClean) %>%
  sum()

#------------------------------------------------------------------------------

#1.B. Identificar valores outliers no ficheiro de dados. Explique o seu 
#raciocínio.

summary(telecomDataClean)

#Com um rápido comando como o summary() obtemos de forma instantânea alguma 
#estatística descritiva das várias variáveis do dataframe, em particular para o
#escrutíneo de outliers, o mínimo e o máximo de cada uma. As variáveis que podem
#conter outliers são as variáveis numéricas como tenure, MonthlyCharges e Total
#Charges.

boxplot(telecomDataClean$tenure, horizontal = TRUE)
boxplot(telecomDataClean$MonthlyCharges, horizontal = TRUE)
boxplot(telecomDataClean$TotalCharges, horizontal = TRUE)

#A visualização gráfica, a partir de gráficos boxplot, histogramas ou scatterplot
# permite uma visualização rápida de como os dados se comportam na variável em
#questão. Neste caso em específico, conseguimos visualizar que na variável 
#TotalCharges, existem valores máximos muito distantes da mediana e de ambos os
#quartis.

hist(telecomDataClean$TotalCharges,
     main = "Custos Totais para o Cliente",
     xlab = "Custos Totais (Dólares)",
     ylab = "Frequência",
     col = "darkmagenta", border = "white")

#-------------------------------------------------------------------------------

#1.C. Criar uma nova variável que no seu entendimento possa ter poder preditivo 
#para prever churn. Explique o seu raciocínio.

#Número de reclamações efetuadas por cliente seriam dados interessantes de ter
#neste contexto, e com isso comparar dados geográficos de forma a perceber se 
#a densidade populacional de certa aréa se correlaciona com um número alto de
#queixas e consequentemente cancelamento de contrato.


################################################################################

#Parte 2: Gráficos Básicos, Mapas, e Personalização (20 pontos)

#Utilizando o conjunto de dados limpos da Parte 1, criar as seguintes visualizações:

#Uma visualização para uma variável contínua.

ggplot(data = telecomDataClean, aes( x = MonthlyCharges)) +
  geom_density(colour = "blue") + 
  labs(title = "Custos Mensais de Clientes", 
       x = "Custos Mensais (Dólares)", y="Densidade")

#Existem vários tipos de gráficos que podemos utilizar para representar uma 
#variável contínua, nomeadamente boxplot, scatterplot e gráficos de densidade.
#Optei por utilizar um gráfico de densidade para representar os custos mensais 
#dos clientes para visualizar a distribuição desta variável contínua. Os picos
#do gráfico de densidade ajudam a identificar onde é que os valores estão 
#concentrados ao longo do intervalo da variável contínua, ou seja, neste caso,
#mostra que a concentração dos valores mensais é maior por volta dos 20 dólares, 
#havendo uma diminuição de concentração de clientes com valores um pouco superiores, 
#voltando então a existir novamente uma maior concentração de clientes custos 
#mensais entre os 75 e os 100 dólares.


#-------------------------------------------------------------------------------
#Uma visualização para uma variável categórica.


ggplot(telecomDataClean, aes(x= PaymentMethod, y=1)) + 
  geom_bar(width=0.5, stat="identity", fill="darkred") + 
  labs(title="Número de Clientes por Método de Pagamento") +
  xlab("Método de Pagamento") +
  ylab("Número de Clientes")


#Para a visualização de uma variável categórica como o tipo de pagamento utilizado
#por cada cliente, optei por um gráfico de barras por ser uma visualização simples
#e eficaz para comunicar a distribuição da mesma. 
#Podemos visualizar que o método de pagamento mais utilizado é o Cheque Eletrónico,
#havendo depois pouca variação entre Transferência bancária, Cartão de Crédito e
#Cheque Postal.



#----------------------------------------------------------
#Uma visualização mostrando a relação entre duas variáveis contínuas.

ggplot(telecomDataClean, aes(x= tenure, y=TotalCharges)) + 
  geom_point(stat="identity", colour = "brown") + 
  geom_smooth(method=lm) +
  labs(title="Custos Totais para o Cliente por Tempo com Telecom") +
  xlab("Tempo de Continuidade (Meses)") +
  ylab("Custos Totais (Dólares")


#Estando perante duas variáveis contínuas, optei pela utilização de um gráfico
#Scatterplot, já que, mais uma vez, são uma ferramenta de visualização simples
#e eficaz da relação entre duas variáveis contínuas.
#Este gráfico dispersão representa a relação entre o tempo de continuidade nos
#serviços da empresa e os custos totais para o cliente. Como expectável, conseguimos
#ver uma correlação positiva entre o aumento de tempo do cliente na Telecom e  
#o aumento de custos para o mesmo. Ainda é possível visualizar um outlier que 
#se encontra na posição 0 meses e tem custos superiors a 1250 dólares. 

#--------------------------------------------------------------------------------
#Parte 3: Visualizações de dados com R (ggplot2) (20 pontos)

#Criar uma visualização ggplot2 à sua escolha utilizando três variáveis (pelo 
#menos uma categórica) do conjunto de dados limpos da Parte 
#Explique a sua escolha de variáveis, tipos de gráficos, e estética. 
#Descrever 2 ideias chave que pretende apresentar com a sua visualização.

set.seed(123)
trainIndex <- createDataPartition(telecomDataClean$Churn, p = .8, list = FALSE)
telecomDataCleanTrain <- telecomDataClean[trainIndex,]
telecomDataCleanTest <- telecomDataClean[-trainIndex,]

ggplot(telecomDataCleanTest, aes(x = tenure, y = MonthlyCharges)) +
  geom_point(colour = "orange") +
  facet_wrap(~ Contract) +
  geom_smooth(method=lm) +
  labs(title = "Comparação entre Custos Mensais, Tipo de contrato e longevidade na Telecom", 
       x = "Longevidade do Cliente na Telecom (Meses)", y="Custos Mensais (Dólares)")

#Optei por utilizar as variáveis custos mensais, tipo de contrato e longevidade
#do cliente na Telecom. Isto para perceber o impacto que os custos mensais e o
#tipo de contrato possam ter numa maior ou menor longevidade para um cliente da
#Telecom. Conseguimos perceber que os clientes que têm uma menor longevidade na
#Telecom optam por um contrato mensal, sendo o custo mensal variável. No contrato
#anual conseguimos já visualizar uma tendência a maior longevidade, assim como
#no contrato bianual que ainda é mais evidente com os dados concentrados mais à
#direita. Curiosamente o custos mensais não parecem ter impacto na longevidade
#do cliente na Empresa.

#-------------------------------------------------------------------------------
#Parte 4: Comparações de Dados, Gráficos Personalizados e Gráficos usando Temas 
#e Faceting (20 pontos)

#Utilizando o conjunto de dados limpos da Parte 1 e ggplot2, criar uma 
#visualização que compare os dados entre categorias ou grupos para a taxa de 
#churn. Utilizar faceting ou outras técnicas para realçar as comparações. Serão
#atribuídos pontos extra a quem apresentar visualizações interactivas. Explique 
#a sua escolha de tipos de gráficos e estética, e descreva como a visualização 
#ajuda a tornar a comparação clara.


telecomDataClean <- telecomDataClean %>%
  mutate(Churn=ifelse(Churn=="No", 0, 1))
telecomDataClean$Churn <- as.integer(telecomDataClean$Churn)

TotalCancelamentos <- sum(telecomDataClean$Churn)
TaxaChurn<-sum(telecomDataClean$Churn)/nrow(telecomDataClean)*100


# Desistência por Tipo de Contrato e Despesas mensais

#Grupo de despesas mensais (18-30, 30-50, 50-70, 70-90, 90-110, >110)
ChurnDespesasMensais <- telecomDataClean %>%
  group_by(telecomDataClean$MonthlyCharges, cut(MonthlyCharges, breaks=seq(20, 110, by=20))) %>%
  dplyr::summarize(TotalCount=n(), ConversionRate=sum(Churn)) %>%
  mutate(TaxaChurn)
  
ggplot(data=ChurnDespesasMensais, aes(x=`telecomDataClean$MonthlyCharges`, y=ConversionRate)) +
  geom_bar(width=0.5, stat="identity", fill="darkgreen") + 
  labs(title="Taxa de Churn por Custos Mensais", x= "Custos mensais (Dólares)", y = "Taxa de Churn")

#Grupo de Contratos (Month-to-Month, One Year, Two Year)
ChurnContratos <- telecomDataClean %>%
  group_by(telecomDataClean$Contract) %>%
  dplyr::summarize(TotalCount=n(), ConversionRate=sum(Churn)) %>%
  mutate(TaxaChurn)

ChurnContratosDespesasMensais <- telecomDataClean %>%
  group_by(telecomDataClean$MonthlyCharges, cut(MonthlyCharges, breaks=seq(20, 110, by=20)),
           telecomDataClean$Contract) %>%
  dplyr::summarize(TotalCount=n(), ConversionRate=sum(Churn)) %>%
  mutate(TaxaChurn)

ggplot(data=ChurnContratosDespesasMensais, aes(x=`telecomDataClean$Contract`, y=ConversionRate)) +
  geom_bar(width=0.5, stat="identity", fill="purple") + 
  labs(title="Taxa de Churn por Tipo de Contrato", x= "Tipos de Contratos", y = "Taxa de Churn")


ChurnContratosDespesasMensais <-rename(ChurnContratosDespesasMensais, CustosMensais = `telecomDataClean$MonthlyCharges`,
       NumeroCancelamentos = ConversionRate,
       TipoContrato = `telecomDataClean$Contract`)


install.packages("plotly")
library(plotly)

GraphInt <- ggplot(ChurnContratosDespesasMensais, 
                   aes(x=`CustosMensais`, y=`NumeroCancelamentos`, colour = `TipoContrato`)) +
  geom_point(size=2) +
  facet_wrap(~ TipoContrato) +
  labs(Main = "Número de Cancelamentos por Tipo de Contrato e Custos Mensais",
       x = "Custos Mensais (Dólares)",
       y = "Número de Cancelamentos",
       color = "Tipo de Contrato") +
  theme_bw()

ggplotly(GraphInt)

#Este gráfico de comparação de tipos de contrato e inclui o número de cancelamentos
#por cada tipo de contrato e prestação mensal. Com este tipo de visualização que
#possui três gráficos lado a lado, usando faceting, conseguimos rapidamente perceber
#que o contrato mensal é o tipo de contrato que apresenta maior incidência de 
#cancelamento de contrato com a Telecom. Passando do contrato mensal para o anual 
#conseguimos ver uma acentuada diminuição de cancelamentos, com os dados mais concentrados
#linha de um e zero cancelamentos.
#No contrato bianual existem ainda menos cancelamentos, estando a concentração
#destes mais evidente na área de custos mensais superiores a 92,45 dólares.
#Numa primeira abordagem todos os dados estavam no mesmo gráfico, dando de uma 
#forma interativa escolher isolar um tipo de contrato para o analisar melhor. No
#entanto, optei por utilizar faceting para separar os dados em três gráficos diferente,
#sendo assim possível ter um perceção imediata do comportamente dos clientes 
#relativamente ao cancelamento de serviços por tipo de contrato e por custos mensais.
#A forma interativa do gráfico continua a permitir um escrutínio maior dos dados.


#-------------------------------------------------------------------------------
#Parte 5: Melhores Práticas de Visualização de Dados (10 pontos)

#Critique uma das visualizações das Partes 2, 3 e 4, identificando áreas a 
#melhorar com base nas melhores práticas de visualização de dados. Forneça 3 
#sugestões específicas sobre como melhorar uma dessas visualizações.

#Relativamente à visualização da parte 4, poderia melhorar na comunicação escrita
#e visual das variáveis. A nível de comunicação escrita, seria ideal ver as variáveis,
#todas escritas numa língua e não misturar Português com Inglês para mostrar 
#uniformidade. Uma outra melhoria que podia ser feita para apresentação dos dados
#de cancelamento seria a omissão da apresentação dos clientes que não cancelaram
#o serviço de forma a não termos um overplotting, já que, estando a falar de 
#cancelamentos, o 0 significa que não cancelaram. Portanto, o objetivo é saber
#as características dos clientes que cancelaram. 
#Relativamesnte à parte visual, a nível de cores, apesar de as que
#foram usadas serem bem distintas umas das outras, seria interessante existir um
#tipo de gradiente no que toca ao aumentos de custos mensais. Ou seja, num custo
#mensal mais baixo um vermelho/verde/azul mais claro e à medida que o preço aumenta
#a intensidade da cor também aumenta. 

#-------------------------------------------------------------------------------
#Parte 6: Construção de dashboards em R (10 pontos)

#Conceber um esquema simples de dashboard para apresentar uma ou várias das 
#visualizações da Parte 2, Parte 3, e Parte 4. Tenha em atenção à disposição 
#geral, incluindo a colocação de cada visualização, bem como quaisquer elementos 
#adicionais do dashboard (por exemplo, filtros, elementos interactivos, anotações 
#de texto). Explique as suas escolhas de desenho e como contribuem para um painel 
#de instrumentos eficaz e de fácil utilização.

#A ideia deste dashboard é comunicar os cancelamentos que ocorreram no mês passado,
#e, como tal, apresantar alguns dados explorados que possam mostrar alguns dos
#potenciais motivos de desistência por parte dos clientes. 
#Gostaria de ter conseguido mudar as cores dos ícones que representam os cartões
#de valor, o dos clientes antigos para amarelo, cancelamentos para vermelho e
#atuais para verde. 
#O gráfico interativo permite verificar o comportamento dos clientes que desistiram
#dos serviços e o gráfico de tipos de contrato para promover junto da equipa de 
#vendas os contratos bi anuais, uma vez que a prevalência de cancelamentos nesse
#tipo de contrato é menor. 

## Shiny UI Editor

install.packages("remotes")
install.packages("shiny")

# Install using the remotes package

library(shinyuieditor)



library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)

ui <- grid_page(
  layout = c(
    "header        header              header        ",
    "ClientesMesPassados Cancelamentos ClientesAtuais",
    "plotly          plotly                plot        ",
    "plotly          plotly                plot        "
  ),
  row_sizes = c(
    "100px",
    "1.19fr",
    "0.81fr",
    "1fr"
  ),
  col_sizes = c(
    "390px",
    "0.95fr",
    "1.05fr"
  ),
  gap_size = "1rem",
  grid_card_text(
    area = "header",
    content = "Cancelamentos de Serviços - Telecom",
    alignment = "start",
    is_title = TRUE
  ),
  grid_card(
    area = "plotly",
    card_header("Interactive Plot"),
    card_body(
      plotlyOutput(
        outputId = "interactivePlot",
        width = "100%",
        height = "100%"
      )
    )
  ),
  grid_card(
    area = "ClientesMesPassados",
    card_body(
      value_box(
        title = "Clientes Mês Passado",
        showcase = bsicons::bs_icon("person-badge-fill"),
        value = nrow(telecomData)
      )
    )
  ),
  grid_card(
    area = "Cancelamentos",
    card_body(
      value_box(
        title = "Cancelamentos Último Mês",
        showcase = bsicons::bs_icon("x-square-fill"),
        value = TotalCancelamentos
      )
    )
  ),
  grid_card(
    area = "ClientesAtuais",
    value_box(
      title = "Clientes Atuais",
      showcase = bsicons::bs_icon("person-fill-check"),
      value = nrow(telecomData) - TotalCancelamentos,
      colour = "red"
    )
  ),
  grid_card(
    area = "plot",
    card_body(plotOutput(outputId = "plot"))
  )
)


server <- function(input, output) {
  
  output$plot <- renderPlot(
    ggplot(telecomDataCleanTest, aes(x = tenure, y = MonthlyCharges)) +
      geom_point(colour = "orange") +
      facet_wrap(~ Contract) +
      geom_smooth(method=lm) +
      labs(title = "Comparação entre Custos Mensais, Tipo de contrato e longevidade na Telecom", 
           x = "Longevidade do Cliente na Telecom (Meses)", y="Custos Mensais (Dólares)")
  )
  
  output$interactivePlot <- renderPlotly(GraphInt)
}

shinyApp(ui, server)