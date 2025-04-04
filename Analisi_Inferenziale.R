# Impostare la directory di lavoro e caricare il dataset
setwd("C:/Users/monte/OneDrive/My Drive/OneDrive/Desktop/Corso ProfessionAI/ProfessionAI_progetto_3/Progetto")
cartella_progetto <- getwd()
file <- "neonati.csv"
dati <- read.csv(file, header = TRUE)

# Struttura del dataset
str(dati)

# Sommario delle variabili
summary(dati)

# Selezione delle variabili numeriche
variabili_numeriche <- c('Anni.madre', 'N.gravidanze', 'Gestazione', 'Peso', 'Lunghezza', 'Cranio')

# Selezione delle variabili categoriali
variabili_categoriali <- c('Fumatrici', 'Tipo.parto', 'Ospedale', 'Sesso')

# Distribuzione di frequenze assolute e relative per le variabili categoriali
for (var in variabili_categoriali) {
  cat("Distribuzione di frequenze per la variabile", var, "\n")
  freq_abs <- table(dati[[var]])
  freq_rel <- prop.table(freq_abs)
  print(cbind(freq_abs, freq_rel))
}

# Matrice di correlazione per le variabili numeriche
cor_matrix <- cor(dati[, variabili_numeriche], use = "complete.obs")
print(cor_matrix)

#indice di corr di Spearman
cor_matrix_s <- cor(dati[, variabili_numeriche], use = "complete.obs", method = "spearman")
print(cor_matrix_s)

#install.packages("corrplot") 
library(corrplot)
corrplot(cor_matrix, method = "number", title = "Correlazione di Pearson", 
         tl.col = "black", title.col = "black", addCoef.col = "black", 
         title.position = "side", tl.srt = 90)
corrplot(cor_matrix_s, method = "number", title = "Correlazione di Spearman", 
         tl.col = "black", title.col = "black", addCoef.col = "black", 
         title.position = "side", tl.srt = 90)

# Caricamento della libreria ggplot2 per le visualizzazioni
library(ggplot2)

# Boxplot per il Peso del Neonato in base al Tipo di Parto
ggplot(dati, aes(x = Tipo.parto, y = Peso)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(x = "Tipo di Parto", y = "Peso del Neonato (grammi)", title = "Distribuzione del Peso del Neonato per Tipo di Parto") +
  theme_minimal()

# Boxplot per il Peso del Neonato in base al Sesso
ggplot(dati, aes(x = Sesso, y = Peso)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(x = "Sesso del Neonato", y = "Peso del Neonato (grammi)", title = "Distribuzione del Peso del Neonato per Sesso") +
  theme_minimal()

# Boxplot per il Peso del Neonato in base all'Ospedale
ggplot(dati, aes(x = Ospedale, y = Peso)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(x = "Ospedale", y = "Peso del Neonato (grammi)", title = "Distribuzione del Peso del Neonato per Ospedale") +
  theme_minimal()

# Scatter Plot per Peso del Neonato vs. Gestazione (con jitter)
ggplot(dati, aes(x = Gestazione, y = Peso)) +
  geom_jitter(alpha = 0.5, color = "blue", width = 0.5, height = 0) +
  labs(x = "Gestazione", y = "Peso del Neonato (grammi)", title = "Peso del Neonato vs. Gestazione") +
  theme_minimal()

# Distribuzione del Peso del Neonato in base allo Stato di Fumatrice della Madre
ggplot(dati, aes(x = factor(Fumatrici), y = Peso)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(x = "Madre Fumatrice (0=No, 1=Sì)", y = "Peso del Neonato (grammi)", title = "Distribuzione del Peso del Neonato per Stato di Fumatrice della Madre") +
  theme_minimal()

# Calcolo dell'indice di Gini per il peso dei neonati utilizzando la funzione gini.index
gini.index <- function(x){
  ni = table(x)
  fi = ni/length(x)
  fi2 = fi^2
  J = length(table(x))
  
  gini = 1-sum(fi2)
  gini.norm = gini/((J-1)/J)
  
  return(gini.norm)
}

# Calcolo dell'indice di Gini per il peso dei neonati
gini_peso <- gini.index(dati$Peso)

# Stampiamo l'indice di Gini
print(paste("Indice di Gini per il peso dei neonati:", round(gini_peso, 4)))


# Istogramma del peso dei neonati
ggplot(dati, aes(x = Peso)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  labs(title = "Distribuzione del peso dei neonati",
       x = "Peso (grammi)", y = "Frequenza") +
  theme_minimal()

# Istogramma della lunghezza dei neonati
ggplot(dati, aes(x = Lunghezza)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  labs(title = "Distribuzione della lunghezza dei neonati",
       x = "Lunghezza (mm)", y = "Frequenza") +
  theme_minimal()


# Calcolo delle medie del campione
media_peso_campione <- mean(dati$Peso)
media_lunghezza_campione <- mean(dati$Lunghezza)

# Test t per confrontare le medie del peso e della lunghezza con popolazione (trovata online)
t_test_peso <- t.test(dati$Peso, mu = 3300, conf.level = 0.99, alternative = "two.sided")
t_test_lunghezza <- t.test(dati$Lunghezza, mu = 500, conf.level = 0.99, alternative = "two.sided")

# Visualizzazione dei risultati
t_test_peso
t_test_lunghezza

# Test t a due campioni indipendenti per il peso
t_test_peso_sesso <- t.test(Peso ~ Sesso, data = dati, conf.level = 0.95, alternative = "two.sided")

# Test t a due campioni indipendenti per la lunghezza
t_test_lunghezza_sesso <- t.test(Lunghezza ~ Sesso, data = dati, conf.level = 0.95, alternative = "two.sided")

# Visualizzazione dei risultati
t_test_peso_sesso
t_test_lunghezza_sesso

# Conteggio delle frequenze di parti cesarei per ogni ospedale
tabella_contingenza <- table(dati$Tipo.parto, dati$Ospedale)

# Test del chi-quadro per l'indipendenza tra il tipo di parto e l'ospedale
test_chi2 <- chisq.test(tabella_contingenza)

# Visualizzazione dei risultati
test_chi2


#Studio coppie di variabili
# Selezione delle variabili numeriche
variabili_numeriche <- c('Anni.madre', 'N.gravidanze', 'Gestazione', 'Peso', 'Lunghezza', 'Cranio')

# Selezione delle variabili categoriali
variabili_categoriali <- c('Fumatrici', 'Tipo.parto', 'Ospedale', 'Sesso')

# Eseguire regressioni univariate per ogni variabile numerica rispetto al peso
risultati_regressioni <- list()

for (variabile in variabili_numeriche) {
  if (variabile != "Peso") {  # Escludere il peso stesso come variabile indipendente
    formula <- as.formula(paste("Peso ~", variabile))
    modello <- lm(formula, data = dati)
    risultati_regressioni[[variabile]] <- summary(modello)
  }
}

# Visualizzare i risultati delle regressioni univariate
for (variabile in names(risultati_regressioni)) {
  cat("Risultati per", variabile, ":\n")
  print(risultati_regressioni[[variabile]])
  cat("\n\n")
}

# Eseguire regressioni univariate per ogni variabile categoriale rispetto al peso
for (variabile in variabili_categoriali) {
  formula <- as.formula(paste("Peso ~", variabile))
  modello <- lm(formula, data = dati)
  risultati_regressioni[[variabile]] <- summary(modello)
}


# Prospetto riassuntivo delle regressioni univariate con p-value < 0.05
risultati_significativi <- list(
  Gestazione = risultati_regressioni[["Gestazione"]],
  Lunghezza = risultati_regressioni[["Lunghezza"]],
  Cranio = risultati_regressioni[["Cranio"]],
  Sesso = risultati_regressioni[["Sesso"]]
)

# Visualizzare il prospetto riassuntivo
cat("Prospetto riassuntivo delle regressioni univariate con p-value < 0.05:\n\n")
for (variabile in names(risultati_significativi)) {
  cat("Risultati per", variabile, ":\n")
  print(risultati_significativi[[variabile]])
  cat("\n\n")
}

# Installazione e caricamento del pacchetto GGally
#install.packages("GGally")
library(GGally)

# Creazione della matrice di scatterplot utilizzando ggpairs
ggpairs(dati[, variabili_numeriche],
        upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "points", combo = "facetdensity"),
        diag = list(continuous = "densityDiag"))

#REGRESSIONE LINEARE MULTIPLA

# Conversione delle variabili categoriche in fattori
dati$Fumatrici <- as.factor(dati$Fumatrici)
dati$Tipo.parto <- as.factor(dati$Tipo.parto)
dati$Ospedale <- as.factor(dati$Ospedale)
dati$Sesso <- as.factor(dati$Sesso)

# Creazione del modello di regressione lineare multipla
modello <- lm(Peso ~ ., data = dati)

# Visualizzazione del sommario del modello
summary(modello)


# Calcolo del VIF per verificare la multicollinearità
#install.packages("car")
library(car)
vif(modello)

# Installazione e caricamento del pacchetto MASS
#install.packages("MASS")
library(MASS)

# Selezione stepwise con AIC
modello_stepwise_aic <- stepAIC(modello, direction = "both", k = 2)
summary(modello_stepwise_aic)

# Selezione stepwise con BIC
modello_stepwise_bic <- stepAIC(modello, direction = "both", k = log(nrow(dati)))
summary(modello_stepwise_bic)

# Modello di regressione lineare con variabili di interazione
modello_interazione <- lm(Peso ~ N.gravidanze + Gestazione + Lunghezza + Cranio + Sesso +
                            Gestazione:Lunghezza + Gestazione:Cranio + Gestazione:Sesso + 
                            Lunghezza:Cranio + Lunghezza:Sesso + Cranio:Sesso, data = dati)

scope <- list(lower = ~ N.gravidanze + Gestazione + Lunghezza + Cranio + Sesso,
              upper = ~ N.gravidanze + Gestazione + Lunghezza + Cranio + Sesso +
                Gestazione:Lunghezza + Gestazione:Cranio + Gestazione:Sesso + 
                Lunghezza:Cranio + Lunghezza:Sesso + Cranio:Sesso)

# Selezione stepwise con BIC
modello_interazione_stepwise_bic <- stepAIC(modello_interazione, 
                                            direction = "both", 
                                            k = log(nrow(dati)), 
                                            scope = scope)
summary(modello_interazione_stepwise_bic)
  
# Confronto di BIC
BIC(modello_stepwise_bic, modello_interazione_stepwise_bic)

#Analisi dei residui modello_interazione
par(mfrow=c(2,2), mar=c(4,4,2,1))
plot(modello_interazione_stepwise_bic)

#install.packages("lmtest")
library(lmtest)
shapiro.test(residuals(modello_interazione_stepwise_bic))
plot(density(residuals(modello_interazione_stepwise_bic)))
lmtest::bptest(modello_interazione_stepwise_bic)
lmtest::dwtest(modello_interazione_stepwise_bic)

#leverage
lev<-hatvalues(modello_interazione_stepwise_bic)
plot(lev)
p<-sum(lev)
n<-length(lev)
soglia=2*p/n
abline(h=soglia,col=2)
lev[lev>soglia]

#outliers
plot(rstudent(modello_interazione_stepwise_bic))
abline(h=c(-2,2))
car::outlierTest(modello_interazione_stepwise_bic)

#distanza di cook
cook<-cooks.distance(modello_interazione_stepwise_bic)
plot(cook,ylim = c(0,1)) 


#rimozione variabile 1551 e analisi 
observation_1551 <- dati[1551, ]
print(observation_1551)

# Rimozione dell'osservazione numero 1551
dati_modificati <- dati[-1551, ]

modello_2 = lm(formula = Peso ~ N.gravidanze + Gestazione + Lunghezza + Cranio + 
     Sesso + Gestazione:Cranio, data = dati_modificati)
summary(modello_2)

#Analisi dei residui modello_interazione
par(mfrow=c(2,2), mar=c(4,4,2,1))
plot(modello_2)

#install.packages("lmtest")
library(lmtest)
shapiro.test(residuals(modello_2))
plot(density(residuals(modello_2)))
lmtest::bptest(modello_2)
lmtest::dwtest(modello_2)

#leverage
lev<-hatvalues(modello_2)
plot(lev)
p<-sum(lev)
n<-length(lev)
soglia=2*p/n
abline(h=soglia,col=2)
lev[lev>soglia]

#outliers
plot(rstudent(modello_2))
abline(h=c(-2,2))
car::outlierTest(modello_2)

#distanza di cook
cook<-cooks.distance(modello_2)
plot(cook,ylim = c(0,1)) 

#CALCOLO per testare modello:
N_gravidanze <- 3
Gestazione <- 39
Lunghezza <- 500
Cranio <- 350
SessoM <- 0
Gestazione_Cranio <- Gestazione * Cranio

# Coefficienti dal modello
Intercept <- 203.85870
coef_N_gravidanze <- 13.88876
coef_Gestazione <- -154.13058
coef_Lunghezza <- 11.24704
coef_Cranio <- -11.72645
coef_SessoM <- 71.83674
coef_Gestazione_Cranio <- 0.56621

# Calcolo del peso
Peso <- Intercept + (coef_N_gravidanze * N_gravidanze) + (coef_Gestazione * Gestazione) + 
  (coef_Lunghezza * Lunghezza) + (coef_Cranio * Cranio) + (coef_SessoM * SessoM) + 
  (coef_Gestazione_Cranio * Gestazione_Cranio)

print(Peso)

#grafici 
library(ggplot2)

dati_modificati$gestazione_cranio <- dati_modificati$Gestazione * dati_modificati$Cranio

ggplot(data=dati_modificati)+
  geom_point(aes(x=gestazione_cranio,
                 y=Peso,
                 col=Sesso),position = "jitter")+
  geom_smooth(aes(x=gestazione_cranio,
                  y=Peso,
                  col=Sesso),se=F,method = "lm")

ggplot(data=dati_modificati)+
  geom_point(aes(x=Cranio,
                 y=Peso,
                 col=Sesso),position = "jitter")+
  geom_smooth(aes(x=Cranio,
                  y=Peso,
                  col=Sesso),se=F,method = "lm")

#Prove
ggplot(data=dati_modificati)+
  geom_point(aes(x=Cranio,
                 y=Peso,
                 col=Sesso),position = "jitter")+
  geom_smooth(aes(x=Cranio,
                  y=Peso,
                  col=Sesso),se=F,method = "lm")









