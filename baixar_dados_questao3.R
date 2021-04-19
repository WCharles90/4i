# limpar o cachê
rm(list = ls())

# configuração de exibição de decimais
options(digits = "2", scipen = 99) 

# Carregando os Pacotes Necessários
ip <- installed.packages()
for (i in c("tidyverse","readxl", "ggplot2", "stargazer","sidrar", "gridExtra", "knitr",
            "BETS")){
  if ( !(i %in% ip) ) { install.packages(i) }
  if (i=="Quandl") { library(i, character.only = T) ; Quandl.auth("WP2rt8HsRo3kjWsRkLY5") } 
  else library(i, character.only = T)
}
rm(list=c("ip","i"))


# 6442 	Índice e variação da receita nominal e do volume de serviços (2014 = 100)
PMS1 <- get_sidra(6442, 
                  #         variable = 1,
                  period = c("202001-202101"),
                  geo = "State"
) 

saveRDS(PMS1, "PMS1.rds")

# 6445 Índice e variação da receita nominal e do volume das atividades turísticas (2014 = 100)
PMS2 <- get_sidra(6445, 
                  #         variable = 1,
                  period = c("202001-202101"),
                  geo = "State"
) 

saveRDS(PMS2, "PMS2.rds")

# Índices de volume e de receita nominal de vendas de materiais de construção, por tipos de índice (2014 = 100)
PMC1 <- get_sidra(3415, 
                  #         variable = 1,
                  period = c("202001-202101"),
                  geo = "State"
) 

saveRDS(PMC1, "PMC1.rds")


# 3418 Índices de volume e de receita nominal de vendas no comércio varejista, por tipos de índice e atividades (2014 = 100)

PMC2 <- get_sidra(3418, 
                  #         variable = 1,
                  period = c("202001-202101"),
                  geo = "State"
)

saveRDS(PMC2, "PMC2.rds")

# 3653 	Produção Física Industrial, por seções e atividades industriais
PIM <-   get_sidra(3653, 
                   #         variable = 1,
                   period = c("202001-202101"),
                   geo = "State"
)

saveRDS(PIM, "PIM.rds")


# extraindo séries do BCB/SGS
precos <- BETSget(c(189,433,225,4390), from = "2010-01-01", to = "2021-01-31")
contas_publicas <- BETSget(c(13762,4503,5793), from = "2010-01-01", to = "2021-01-31")


# Acumular em 12 meses
data1  <- data.frame(precos)
#data  <- ts(data, start = c(2000,1), freq=12) 
fator <- 1+data1/100


acumulado <- (fator*lag(fator,1)*lag(fator,2)*lag(fator,3)*
                lag(fator,4)*lag(fator,5)*lag(fator,6)*lag(fator,7)*
                lag(fator,8)*lag(fator,9)*lag(fator,10)*lag(fator,11)
              -1)*100

colnames(acumulado) = colnames(data1)

acumulado <- data.frame(acumulado)
acumulado$data <- seq(as.Date("2010/01/01"), 
                      by = "month", 
                      length.out = length(acumulado$ts_189))


precos_acum <- data.frame(acumulado) %>% slice_tail(n = 13)


governo <- data.frame(contas_publicas)

governo$data <- seq(as.Date("2010/01/01"), 
                    by = "month", 
                    length.out = length(contas_publicas$ts_13762))

governo <- governo %>% slice_tail(n = 100)


saveRDS(precos_acum, "precos_acum.rds")
saveRDS(governo, "governo.rds")
