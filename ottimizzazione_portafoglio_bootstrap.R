# Simone Bucciol – Bachelor’s Thesis Project “Financial Portfolio Optimization: A Quantitative Approach Using R”
# All rights reserved.
# --- Italian version below ---

# ======================================================================
# =                 FINANCIAL PORTFOLIO ANALYSIS                      =
# ======================================================================

# ------ ENVIRONMENT PREPARATION ------
# Remove all variables from the working environment to avoid conflicts
rm(list = ls())

# Execute garbage collection to free unused memory
gc()

# Close all graphic devices previously opened
while (dev.cur() > 1) {
  dev.off()
}

# ------ LIBRARY LOADING ------
library(PerformanceAnalytics)   # Used for return calculation and financial performance analysis
library(IntroCompFinR)          # Provides functions for efficient portfolio calculation
library(corrplot)               # Allows graphic visualization of correlation matrices
library(xts)                    # Manages extended time series (eXtensible Time Series)
library(boot)                   # Implements bootstrap methods for statistical analysis
library(ggplot2)                # Creates advanced graphics, used to visualize bootstrap normal distributions

# ------ BASE PARAMETERS ------
# Set the risk-free rate to 2.5% annually
r.f <- 0.025

# ------ DATA LOADING ------
# Automatically specify the path of the directory containing CSV files with financial data
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
data_dir <- file.path(script_dir, "dati")

# Get the complete list of CSV files in the specified directory
file_list <- list.files(data_dir, pattern = "*.csv", full.names = TRUE)

# Load data from CSV files into a list of dataframes efficiently
data_list <- lapply(file_list, function(file) {
  temp_data <- read.csv(file, stringsAsFactors = FALSE)
  if ("Close" %in% colnames(temp_data)) {
    temp_data$Date <- as.Date(temp_data$Date)
    return(data.frame(Date = temp_data$Date, Close = temp_data$Close))
  } else {
    warning(paste("Close column not found in", basename(file)))
    return(NULL)
  }
})

# Filter any NULL elements from the list (invalid files)
data_list <- Filter(Negate(is.null), data_list)

if (length(data_list) > 0) {
  # Merge all dataframes using the 'Date' column as join key
  combined_data <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), data_list)
  
  # Convert the dataframe to an xts object (structure for time series)
  prices_xts <- xts(combined_data[, -1], order.by = combined_data$Date)
  
  # Rename columns using original file names (without .csv extension)
  colnames(prices_xts) <- gsub("\\.csv$", "", basename(file_list))
}

# ------ ANALYSIS PERIOD DEFINITION ------
# Select the analysis period from 2009 to 2024
smpl <- "2009-01-01::2024-12-11"
prices_xts = prices_xts[smpl]

# ------ RETURN CALCULATION ------
# Calculate daily logarithmic returns and remove rows with missing values
returns_xts <- na.omit(Return.calculate(prices_xts, method = "log"))

# ------ PORTFOLIO PARAMETER CALCULATION ------
# Calculate expected annualized returns by multiplying by 252 (trading days in a year)
muhat <- 252 * colMeans(returns_xts)

# Calculate the annualized covariance matrix between returns
covhat <- 252 * cov(returns_xts)

# ------ STATISTICS PRINTING ------
# Display expected annualized returns for each security
cat("\nThe expected annualized returns of the securities that make up the portfolio are:\n"); print(muhat)

# Display annualized standard deviations for each security
cat("\nThe expected annualized standard deviations of the securities that make up the portfolio are:\n"); print(sqrt(diag(covhat)))

# Create a graph of the correlation matrix between securities
corrplot.mixed(cov2cor(covhat), upper = "ellipse")

# ------ EFFICIENT PORTFOLIO CALCULATION ------
# Calculate the global minimum variance portfolio allowing short sales
gmin.port <- globalMin.portfolio(muhat, covhat)

# Calculate the tangency portfolio (maximum return/risk ratio) with short sales
tan.port <- tangency.portfolio(muhat, covhat, r.f)

# Calculate the global minimum variance portfolio without short sales
gmin.port.ns <- globalMin.portfolio(muhat, covhat, shorts = FALSE)

# Calculate the tangency portfolio without short sales
tan.port.ns <- tangency.portfolio(muhat, covhat, r.f, shorts = FALSE)

# Display statistics of calculated optimal portfolios
cat("\nStatistics for the tangency portfolio (point P, figure 1.3), with short selling:\n");
print(tan.port)
cat("\nStatistics for the tangency portfolio (point P, figure 1.3), without short selling:\n");
print(tan.port.ns)
cat("\nStatistics for the minimum variance portfolio ('GMP', point G, figure 1.3), with short selling:\n");
print(gmin.port)
cat("\nStatistics for the minimum variance portfolio ('GMP', point G, figure 1.3), without short selling:\n");
print(gmin.port.ns)

# ------ EFFICIENT FRONTIER VISUALIZATION ------
# Calculate the efficient frontier with short sales (60 portfolios between alpha.min and alpha.max)
ef <- efficient.frontier(muhat, covhat, alpha.min = -0.5, alpha.max = 1.5, nport = 60)

# Calculate the efficient frontier without short sales (60 portfolios)
ef.ns <- efficient.frontier(muhat, covhat, alpha.min = 0, alpha.max = 1, nport = 60, shorts = FALSE)

# Configure graphic parameters to improve readability
par(mar = c(5, 5, 4, 2) + 0.1)  # Increase margins for labels
par(mgp = c(3, 1, 0))  # Configure position of axis labels

# Create an initial empty graph with professional labels
plot(NA, NA,
     xlim = c(0, max(ef$sd, ef.ns$sd) * 1.1),
     ylim = c(0, max(ef$er, ef.ns$er) * 1.1),
     xlab = expression(bold("Risk (Standard Deviation) " ~ sigma[p])),
     ylab = expression(bold("Expected Return " ~ E[R[p]])),
     main = expression(bold("Efficient Frontier and Capital Market Line")),
     cex.lab = 1.2, cex.axis = 1.1, cex.main = 1.4,
     las = 1,  # Horizontal labels on axes
     xaxs = "i", yaxs = "i")  # Remove extra space on axes

# Add a light grid to facilitate graph reading
grid(col = "gray90", lty = "dotted")

# Draw the efficient frontier with short sales in blue
lines(ef$sd, ef$er, type = "l", col = "dodgerblue3", lwd = 2.5)

# Draw the efficient frontier without short sales in red
lines(ef.ns$sd, ef.ns$er, type = "l", col = "firebrick3", lwd = 2.5)

# Add the point representing the risk-free rate
points(0, r.f, pch = 19, col = "darkgreen", cex = 1.2)
text(0, r.f, labels = expression(r[f]), pos = 4, cex = 1.1, col = "darkgreen")

# Calculate and draw the Capital Market Line (CML) for the tangency portfolio with short sales
sr.tan <- (tan.port$er - r.f) / tan.port$sd
abline(a = r.f, b = sr.tan, col = "seagreen", lwd = 2, lty = 1)

# Calculate and draw the Capital Market Line (CML) for the tangency portfolio without short sales
sr.tan.ns <- (tan.port.ns$er - r.f) / tan.port.ns$sd
abline(a = r.f, b = sr.tan.ns, col = "seagreen", lwd = 2, lty = 2)

# Add points representing tangency and minimum variance portfolios
points(tan.port$sd, tan.port$er, pch = 19, col = "black", cex = 1.2, bg = "dodgerblue3")
points(tan.port.ns$sd, tan.port.ns$er, pch = 19, col = "black", cex = 1.2, bg = "firebrick3")
points(gmin.port$sd, gmin.port$er, pch = 19, col = "black", cex = 1.2)
points(gmin.port.ns$sd, gmin.port.ns$er, pch = 19, col = "black", cex = 1.2)

# Calculate the display range to correctly position labels
x_range <- diff(par("usr")[1:2])
y_range <- diff(par("usr")[3:4])

# Position portfolio labels strategically to avoid overlaps
# Label for tangency portfolio with short sales
text(tan.port$sd + x_range * 0.02 , tan.port$er,
     "Tangency Portfolio\n(with short selling)",
     cex = 0.9, adj = 0)

# Label for tangency portfolio without short sales
text(tan.port.ns$sd + x_range * 0.02, tan.port.ns$er,
     "Tangency Portfolio\n(without short selling)",
     cex = 0.9, adj = 0)

# Label for minimum variance portfolio with short sales
text(gmin.port$sd - 0.035, gmin.port$er - y_range *0.05,
     "Minimum Variance Portfolio\n(with short selling)",
     cex = 0.9, adj = 0.5)

# Label for minimum variance portfolio without short sales
text(gmin.port.ns$sd + 0.045, gmin.port.ns$er + y_range * 0.05,
     "Minimum Variance Portfolio\n(without short selling)",
     cex = 0.9, adj = 0.5)

# Add points representing individual securities
points(sqrt(diag(covhat)), muhat, pch = 20, col = "gray50", cex = 0.8)
text(sqrt(diag(covhat)), muhat, labels = rownames(covhat), pos = 4, cex = 0.7, col = "gray30")

# Add legend with explanation of graphic elements
legend("topleft",
       legend = c("E.F. (with short)",
                  "E.F. (without short)",
                  "CML (with short)",
                  "CML (without short)"),
       col = c("dodgerblue3", "firebrick3", "seagreen", "seagreen"),
       lty = c(1, 1, 1, 2),
       lwd = c(2.5, 2.5, 2, 2),
       cex = 0.5,
       bg = "white",
       box.col = "gray70",
       inset = c(0.01, 0.01),  # Bring legend closer to edges
       x.intersp = 0.4,        # Reduce horizontal space between elements
       y.intersp = 0.8)        # Reduce vertical space between rows

# Add border to the graph to delimit its boundaries
box(lwd = 1.5)

# Add information about Sharpe indices below the graph
mtext(paste("Sharpe Ratio (with short selling):", round(sr.tan, 3)),
      side = 1, line = 3.5, adj = 0, cex = 0.9)
mtext(paste("Sharpe Ratio (without short selling):", round(sr.tan.ns, 3)),
      side = 1, line = 4.5, adj = 0, cex = 0.9)

# Display Sharpe ratio values also in the console
cat("\nEstimated Sharpe Ratio (Tangency Portfolio with short selling):", round(sr.tan, 4), "\n")
cat("Estimated Sharpe Ratio (Tangency Portfolio without short selling):", round(sr.tan.ns, 4), "\n")

# ------ PORTFOLIO RETURN CALCULATION ------
# Calculate daily percentage returns of each security
asset_returns <- na.omit(diff(prices_xts) / prices_xts[-nrow(prices_xts),])

# Extract optimal weights from tangency portfolio without short sales
weights_ns <- as.numeric(tan.port.ns$weights)

# Extract optimal weights from tangency portfolio with short sales
weights <- as.numeric(tan.port$weights)

# Calculate daily returns of portfolio without short sales
# Perform matrix-vector multiplication between returns and weights
portfolio_returns_ns <- asset_returns %*% weights_ns

# Calculate daily returns of portfolio with short sales
portfolio_returns <- asset_returns %*% weights

# Convert results to simple vectors for subsequent analysis
returns_tan_ns <- as.numeric(portfolio_returns_ns)
returns_tan <- as.numeric(portfolio_returns)

# ------ BOOTSTRAP FOR SHARPE RATIO ------
# Define a function to calculate Sharpe ratio on bootstrap samples
sharpe_bootstrap <- function(returns, indices) {
  sample_returns <- returns[indices]
  return(((mean(sample_returns)*252) - r.f) / (sd(sample_returns)*sqrt(252)))
}

# ------ BOOTSTRAP OF BOTH PORTFOLIOS ------
# Apply bootstrap to returns of both portfolios for Sharpe ratio variance estimation (10,000 resamplings)
boot_results <- sapply(list(returns_tan, returns_tan_ns), function(returns) {
  boot(returns, statistic = sharpe_bootstrap, R = 10000)
}, simplify = FALSE)

# ------ BOOTSTRAP RESULTS ANALYSIS ------
# Check if Sharpe ratios are statistically different from zero
for (i in 1:2) {
  # Identify portfolio type based on index
  x <- if (i == 1) "Short selling" else "No Short selling"
  # Calculate 99% confidence interval with percentile method
  ci <- boot.ci(boot_results[[i]], conf = 0.99, type = "perc")$percent[4:5]
  
  if (ci[1] < 0 & ci[2] > 0) {
    # If confidence interval includes zero, the index is not statistically significant
    print(paste("The beta coefficient for portfolios", x,
                "is not significant because zero is contained in the confidence interval, which equals:"))
    print(ci)
  } else {
    # If confidence interval does not include zero, the index is statistically significant
    print(paste("The beta coefficient for portfolios", x,
                "is significant because zero is not contained in the confidence interval, which equals:"))
    print(ci)
  }
}

# Check if the point estimator of Sharpe ratio falls within the bootstrap confidence interval
for (i in 1:2) {
  x <- if (i == 1) "Short selling" else "No Short selling"
  # Select appropriate estimator based on portfolio type
  estimator <- if (i == 1) sr.tan else sr.tan.ns
  # Calculate 99% confidence interval
  ci <- boot.ci(boot_results[[i]], conf = 0.99, type = "perc")$percent[4:5]
  
  if (estimator >= ci[1] & estimator <= ci[2]) {
    # If estimator is in interval, confirm estimate robustness
    print(paste("The Sharpe Ratio estimator, for portfolios", x,
                ", calculated from data equals", if(i == 1) sr.tan else sr.tan.ns,
                ", and is included in the confidence interval."))
  } else {
    # If estimator is outside interval, signal potential problems
    print(paste("The Sharpe Ratio estimator, for portfolios", x,
                ", calculated from data equals ", if(i == 1) sr.tan else sr.tan.ns,
                "and is not included in the confidence interval"))
  }
  
  print(paste("Confidence interval: [", ci[1], "-", ci[2],"]"))
  print("-----------------------------------")
}

# ------ NORMALITY ANALYSIS OF BOOTSTRAP ESTIMATES ------
# Configure graphic layout as 2x2 grid
par(mfrow = c(2, 2))

# Create graphs for both portfolios
for (i in 1:2) {
  # Identify portfolio type
  portfolio_type <- if (i == 1) "Short selling" else "No Short selling"
  
  # Create histogram of bootstrap estimates with overlaid normal density curve
  hist(boot_results[[i]]$t,
       freq = FALSE,
       col = "lightblue",
       border = "black",
       main = paste("Bootstrap -", portfolio_type),
       xlab = "Sharpe Ratio",
       ylab = "Density")
  
  # Add theoretical normal distribution curve based on mean and sd of estimates
  curve(dnorm(x, mean = mean(boot_results[[i]]$t), sd = sd(boot_results[[i]]$t)),
        add = TRUE, col = "red", lwd = 2)
  
  # Create line plot of bootstrap estimates to visualize their variability
  plot(boot_results[[i]]$t,
       type = "l",
       col = "blue",
       main = paste("SR Bootstrap -", portfolio_type),
       xlab = "Resamplings",
       ylab = "SR Values")
}

# Graphic analysis of normality of Sharpe Ratio bootstrap estimates
# Loop to create histograms and Q-Q plots for both portfolios
for (i in 1:2) {
  # Identify portfolio type
  portfolio_type <- if (i == 1) "Short selling" else "No Short selling"
  
  # Extract bootstrap estimates for current iteration
  bootstrap_data <- boot_results[[i]]$t
  
  # Create histogram with overlaid normal density curve
  hist(bootstrap_data,
       freq = FALSE,
       col = "lightblue",
       border = "black",
       main = paste("Bootstrap Distribution -", portfolio_type),
       xlab = "Sharpe Ratio",
       ylab = "Density")
  
  # Add theoretical normal density curve
  curve(dnorm(x, mean = mean(bootstrap_data), sd = sd(bootstrap_data)),
        add = TRUE, col = "red", lwd = 2)
  
  # Create Q-Q plot to visually verify normality of estimates
  qqnorm(bootstrap_data,
         main = paste("Q-Q Plot -", portfolio_type),
         pch = 19)  # Solid points
  qqline(bootstrap_data, col = "red", lwd = 2)  # Reference line
}

# Restore default graphic layout
par(mfrow = c(1, 1))

# ------ PREPARATION FOR STATISTICAL TEST ------
# Prepare data for statistical test on difference between the two Sharpe ratios
# Ensure both series have the same length
min_length <- min(length(returns_tan), length(returns_tan_ns))
returns_combined <- cbind(returns_tan[1:min_length], returns_tan_ns[1:min_length])

# ------ STATISTICAL TEST FOR SHARPE RATIO DIFFERENCE ------
# Calculate sample size for both portfolios
n1 = n2 = length(returns_xts[smpl][,1])

# Calculate standard deviations of bootstrap estimates
s1 <- sd(boot_results[[1]]$t)  # Standard deviation of estimates with short sales
s2 <- sd(boot_results[[2]]$t)  # Standard deviation of estimates without short sales

# Calculate standard error of difference between Sharpe ratios
se <- sqrt((s1^2 / n1) + (s2^2 / n2))

# Calculate t-statistic for difference test
t_stat <- (sr.tan - sr.tan.ns) / se

# Display t-statistic value
print(t_stat)

# Calculate degrees of freedom using Welch-Satterthwaite formula for different variances
df <- ((s1^2 / n1 + s2^2 / n2)^2) / ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1))

# Calculate p-value for two-tailed test
p_value <- 2 * (1 - pt(abs(t_stat), df))

# ------ RESULTS INTERPRETATION ------
# Check statistical significance of difference between the two Sharpe ratios
if (p_value < 0.01) {
  print("The difference between the two Sharpe Ratios is statistically significant with a significance level of 99%.")
} else {
  print("The difference between the two Sharpe Ratios is NOT statistically significant with a significance level of 99%.")
}


































































































# ======================================================================
# =                 ANALISI DI PORTAFOGLIO FINANZIARIO                 =
# ======================================================================

# ------ PREPARAZIONE AMBIENTE ------
# Rimuove tutte le variabili dall'ambiente di lavoro per evitare conflitti
rm(list = ls())
# Esegue la garbage collection per liberare memoria non utilizzata
gc()
# Chiude tutti i dispositivi grafici aperti in precedenza
while (dev.cur() > 1) {
  dev.off()
}

# ------ CARICAMENTO LIBRERIE ------
library(PerformanceAnalytics)   # Utilizzata per calcolo rendimenti e analisi delle performance finanziarie
library(IntroCompFinR)          # Fornisce funzioni per il calcolo dei portafogli efficienti
library(corrplot)               # Permette la visualizzazione grafica delle matrici di correlazione
library(xts)                    # Gestisce serie temporali estese (eXtensible Time Series)
library(boot)                   # Implementa metodi di bootstrap per analisi statistiche
library(ggplot2)                # Crea grafici avanzati, usato per visualizzare distribuzioni normali bootstrap

# ------ PARAMETRI DI BASE ------
# Imposta il tasso di rendimento privo di rischio al 2.5% annuo
r.f <- 0.025

# ------ CARICAMENTO DATI ------
# Specifica in automatico il percorso della directory contenente i file CSV con i dati finanziari
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
data_dir <- file.path(script_dir, "dati")

# Ottiene l'elenco completo dei file CSV nella directory specificata
file_list <- list.files(data_dir, pattern = "*.csv", full.names = TRUE)
# Carica i dati dai file CSV in una lista di dataframe in modo efficiente
data_list <- lapply(file_list, function(file) {
  temp_data <- read.csv(file, stringsAsFactors = FALSE)
  if ("Close" %in% colnames(temp_data)) {
    temp_data$Date <- as.Date(temp_data$Date)
    return(data.frame(Date = temp_data$Date, Close = temp_data$Close))
  } else {
    warning(paste("Close column not found in", basename(file)))
    return(NULL)
  }
})
# Filtra eventuali elementi NULL dalla lista (file non validi)
data_list <- Filter(Negate(is.null), data_list)

if (length(data_list) > 0) {
  # Unisce tutti i dataframe usando la colonna 'Date' come chiave di join
  combined_data <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), data_list)
  
  # Converte il dataframe in un oggetto xts (struttura per serie temporali)
  prices_xts <- xts(combined_data[, -1], order.by = combined_data$Date)
  
  # Rinomina le colonne usando i nomi dei file originali (senza estensione .csv)
  colnames(prices_xts) <- gsub("\\.csv$", "", basename(file_list))
}

# ------ DEFINIZIONE DEL PERIODO DI ANALISI ------
# Seleziona il periodo di analisi dal 2009 al 2024
smpl <- "2009-01-01::2024-12-11"
prices_xts = prices_xts[smpl]

# ------ CALCOLO DEI RENDIMENTI ------
# Calcola i rendimenti logaritmici giornalieri ed elimina le righe con valori mancanti
returns_xts <- na.omit(Return.calculate(prices_xts, method = "log"))

# ------ CALCOLO PARAMETRI DEL PORTAFOGLIO ------
# Calcola i rendimenti attesi annualizzati moltiplicando per 252 (giorni di negoziazione in un anno)
muhat <- 252 * colMeans(returns_xts)
# Calcola la matrice di covarianza annualizzata tra i rendimenti
covhat <- 252 * cov(returns_xts)

# ------ STAMPA DELLE STATISTICHE ------
# Visualizza i rendimenti attesi annualizzati per ciascun titolo
cat("\nI rendimenti attesi annualizzati dei titoli che compongono il portafoglio sono:\n"); print(muhat) #", round(muhat, 8), "
# Visualizza le deviazioni standard annualizzate per ciascun titolo
cat("\nLa deviazione standard attesa annualizzati dei titoli che compongono il portafoglio sono:\n"); print(sqrt(diag(covhat))) #", round(muhat, 8), "
# Crea un grafico della matrice di correlazione tra i titoli
corrplot.mixed(cov2cor(covhat), upper = "ellipse")

# ------ CALCOLO PORTAFOGLI EFFICIENTI ------
# Calcola il portafoglio a minima varianza globale permettendo vendite allo scoperto
gmin.port <- globalMin.portfolio(muhat, covhat)
# Calcola il portafoglio tangente (massimo rapporto rendimento/rischio) con vendite allo scoperto
tan.port <- tangency.portfolio(muhat, covhat, r.f)
# Calcola il portafoglio a minima varianza globale senza vendite allo scoperto
gmin.port.ns <- globalMin.portfolio(muhat, covhat, shorts = FALSE)
# Calcola il portafoglio tangente senza vendite allo scoperto
tan.port.ns <- tangency.portfolio(muhat, covhat, r.f, shorts = FALSE)

# Visualizza le statistiche dei portafogli ottimali calcolati
cat("\nStatistiche per il portafoglio tangente (punto P, figura 1.3), in presenta di short selling:\n");print(tan.port)
cat("\nStatistiche per il portafoglio tangente (punto P, figura 1.3), in assenza di short selling:\n");print(tan.port.ns)
cat("\nStatistiche per il portafoglio a minima varianza ('GMP',punto G, figura 1.3), in presenza di short selling:\n");print(gmin.port)
cat("\nStatistiche per il portafoglio a minima varianza ('GMP',punto G, figura 1.3), in assenza di short selling:\n");print(gmin.port.ns)

# ------ VISUALIZZAZIONE DELLA FRONTIERA EFFICIENTE ------
# Calcola la frontiera efficiente con vendite allo scoperto (60 portafogli tra alpha.min e alpha.max)
ef <- efficient.frontier(muhat, covhat, alpha.min = -0.5, alpha.max = 1.5, nport = 60)
# Calcola la frontiera efficiente senza vendite allo scoperto (60 portafogli)
ef.ns <- efficient.frontier(muhat, covhat, alpha.min = 0, alpha.max = 1, nport = 60, shorts = FALSE)

# Configura i parametri grafici per migliorare la leggibilità
par(mar = c(5, 5, 4, 2) + 0.1)  # Aumenta i margini per le etichette
par(mgp = c(3, 1, 0))  # Configura la posizione delle etichette degli assi

# Crea un grafico iniziale vuoto con etichette professionali
plot(NA, NA, 
     xlim = c(0, max(ef$sd, ef.ns$sd) * 1.1),
     ylim = c(0, max(ef$er, ef.ns$er) * 1.1),
     xlab = expression(bold("Rischio (Deviazione Standard) " ~ sigma[p])),
     ylab = expression(bold("Rendimento Atteso " ~ E[R[p]])),
     main = expression(bold("Frontiera Efficiente e Capital Market Line")),
     cex.lab = 1.2, cex.axis = 1.1, cex.main = 1.4,
     las = 1,  # Etichette orizzontali sugli assi
     xaxs = "i", yaxs = "i")  # Rimuove lo spazio extra agli assi

# Aggiunge una griglia leggera per facilitare la lettura del grafico
grid(col = "gray90", lty = "dotted")

# Disegna la frontiera efficiente con vendite allo scoperto in blu
lines(ef$sd, ef$er, type = "l", col = "dodgerblue3", lwd = 2.5)
# Disegna la frontiera efficiente senza vendite allo scoperto in rosso
lines(ef.ns$sd, ef.ns$er, type = "l", col = "firebrick3", lwd = 2.5)

# Aggiunge il punto rappresentante il tasso privo di rischio
points(0, r.f, pch = 19, col = "darkgreen", cex = 1.2)
text(0, r.f, labels = expression(r[f]), pos = 4, cex = 1.1, col = "darkgreen")

# Calcola e disegna la Capital Market Line (CML) per il portafoglio tangente con vendite allo scoperto
sr.tan <- (tan.port$er - r.f) / tan.port$sd
abline(a = r.f, b = sr.tan, col = "seagreen", lwd = 2, lty = 1)

# Calcola e disegna la Capital Market Line (CML) per il portafoglio tangente senza vendite allo scoperto
sr.tan.ns <- (tan.port.ns$er - r.f) / tan.port.ns$sd
abline(a = r.f, b = sr.tan.ns, col = "seagreen", lwd = 2, lty = 2)

# Aggiunge i punti rappresentanti i portafogli tangenti e a minima varianza
points(tan.port$sd, tan.port$er, pch = 19, col = "black", cex = 1.2, bg = "dodgerblue3")
points(tan.port.ns$sd, tan.port.ns$er, pch = 19, col = "black", cex = 1.2, bg = "firebrick3")
points(gmin.port$sd, gmin.port$er, pch = 19, col = "black", cex = 1.2)
points(gmin.port.ns$sd, gmin.port.ns$er, pch = 19, col = "black", cex = 1.2)

# Calcola l'intervallo di visualizzazione per posizionare correttamente le etichette
x_range <- diff(par("usr")[1:2])
y_range <- diff(par("usr")[3:4])

# Posiziona le etichette dei portafogli in modo strategico per evitare sovrapposizioni
# Etichetta per il portafoglio tangente con vendite allo scoperto
text(tan.port$sd + x_range * 0.02 , tan.port$er, 
     "Portafoglio Tangente\n(con short selling)",
     cex = 0.9, adj = 0)

# Etichetta per il portafoglio tangente senza vendite allo scoperto
text(tan.port.ns$sd + x_range * 0.02, tan.port.ns$er, 
     "Portafoglio Tangente\n(senza short selling)",
     cex = 0.9, adj = 0)

# Etichetta per il portafoglio a minima varianza con vendite allo scoperto
text(gmin.port$sd - 0.035, gmin.port$er - y_range *0.05, 
     "Portafoglio Minima Varianza\n(con short selling)",
     cex = 0.9, adj = 0.5)

# Etichetta per il portafoglio a minima varianza senza vendite allo scoperto
text(gmin.port.ns$sd + 0.045, gmin.port.ns$er + y_range * 0.05, 
     "Portafoglio Minima Varianza\n(senza short selling)",
     cex = 0.9, adj = 0.5)

# Aggiunge i punti rappresentanti i singoli titoli
points(sqrt(diag(covhat)), muhat, pch = 20, col = "gray50", cex = 0.8)
text(sqrt(diag(covhat)), muhat, labels = rownames(covhat), pos = 4, cex = 0.7, col = "gray30")

# Aggiunge la legenda con spiegazione degli elementi grafici
legend("topleft", 
       legend = c("F.E. (con short)", 
                  "F.E. (senza short)", 
                  "CML (con short)", 
                  "CML (senza short)"),
       col = c("dodgerblue3", "firebrick3", "seagreen", "seagreen"),
       lty = c(1, 1, 1, 2),
       lwd = c(2.5, 2.5, 2, 2),
       cex = 0.5,
       bg = "white",
       box.col = "gray70",
       inset = c(0.01, 0.01),  # Avvicina la legenda ai bordi
       x.intersp = 0.4,        # Riduce spazio orizzontale tra elementi
       y.intersp = 0.8)        # Riduce spazio verticale tra righe

# Aggiunge un bordo al grafico per delimitarne i confini
box(lwd = 1.5)

# Aggiunge informazioni sugli indici di Sharpe sotto il grafico
mtext(paste("Sharpe Ratio (con short selling):", round(sr.tan, 3)),
      side = 1, line = 3.5, adj = 0, cex = 0.9)
mtext(paste("Sharpe Ratio (senza short selling):", round(sr.tan.ns, 3)),
      side = 1, line = 4.5, adj = 0, cex = 0.9)

# Visualizza i valori degli indici di Sharpe anche nella console
cat("\nSharpe Ratio stimato (Portafoglio Tangente con short selling):", round(sr.tan, 4), "\n")
cat("Sharpe Ratio stimato (Portafoglio Tangente senza short selling):", round(sr.tan.ns, 4), "\n")

# ------ CALCOLO RENDIMENTI PORTAFOGLIO ------
# Calcola i rendimenti percentuali giornalieri di ciascun titolo
asset_returns <- na.omit(diff(prices_xts) / prices_xts[-nrow(prices_xts),])

# Estrae i pesi ottimali dal portafoglio tangente senza vendite allo scoperto
weights_ns <- as.numeric(tan.port.ns$weights)
# Estrae i pesi ottimali dal portafoglio tangente con vendite allo scoperto
weights <- as.numeric(tan.port$weights)

# Calcola i rendimenti giornalieri del portafoglio senza vendite allo scoperto
# Effettua una moltiplicazione matrice-vettore tra rendimenti e pesi
portfolio_returns_ns <- asset_returns %*% weights_ns

# Calcola i rendimenti giornalieri del portafoglio con vendite allo scoperto
portfolio_returns <- asset_returns %*% weights

# Converte i risultati in vettori semplici per analisi successive
returns_tan_ns <- as.numeric(portfolio_returns_ns)
returns_tan <- as.numeric(portfolio_returns)

# ------ BOOTSTRAP PER LO SHARPE RATIO ------
# Definisce una funzione per calcolare lo Sharpe ratio su campioni bootstrap
sharpe_bootstrap <- function(returns, indices) {
  sample_returns <- returns[indices]
  return(((mean(sample_returns)*252) - r.f) / (sd(sample_returns)*sqrt(252)))
}

# ------ BOOTSTRAP DEI DUE PORTAFOGLI ------
# Applica il bootstrap ai rendimenti di entrambi i portafogli per la stima della varianza dello Sharpe ratio (10,000 rimcampionamenti)
boot_results <- sapply(list(returns_tan, returns_tan_ns), function(returns) {
  boot(returns, statistic = sharpe_bootstrap, R = 10000)
}, simplify = FALSE)

# ------ ANALISI DEI RISULTATI BOOTSTRAP ------
# Verifica se gli indici di Sharpe sono statisticamente diversi da zero
for (i in 1:2) {
  # Identifica il tipo di portafoglio in base all'indice
  x <- if (i == 1) "Short selling" else "No Short selling"
  # Calcola intervallo di confidenza al 99% con metodo percentile
  ci <- boot.ci(boot_results[[i]], conf = 0.99, type = "perc")$percent[4:5]
  
  if (ci[1] < 0 & ci[2] > 0) {
    # Se l'intervallo di confidenza include zero, l'indice non è statisticamente significativo
    print(paste("Il coefficiente beta per i portafogli", x,
                "non è significativo perché lo zero è contenuto nell'intervallo di confidenza e, questo risulta pari a:"))
    print(ci)
  } else {
    # Se l'intervallo di confidenza non include zero, l'indice è statisticamente significativo
    print(paste("Il coefficiente beta per i portafogli", x,
                "è significativo perché lo zero non è contenuto nell'intervallo di confidenza e, questo risulta pari a:"))
    print(ci)
  }
}

# Verifica se lo stimatore puntuale dello Sharpe ratio cade nell'intervallo di confidenza bootstrap
for (i in 1:2) {
  x <- if (i == 1) "Short selling" else "No Short selling"
  # Seleziona lo stimatore appropriato in base al tipo di portafoglio
  estimator <- if (i == 1) sr.tan else sr.tan.ns
  # Calcola intervallo di confidenza al 99%
  ci <- boot.ci(boot_results[[i]], conf = 0.99, type = "perc")$percent[4:5]
  
  if (estimator >= ci[1] & estimator <= ci[2]) {
    # Se lo stimatore è nell'intervallo, conferma la robustezza della stima
    print(paste("Lo stimatore dello Sharpe Ratio, per i portafogli", x,",calcolato dai dati è pari a", if(i == 1) sr.tan else sr.tan.ns,
                ",ed è compreso nell'intervallo di confidenza."))
  } else {
    # Se lo stimatore è fuori dall'intervallo, segnala potenziali problemi
    print(paste("Lo stimatore dello Sharpe Ratio, per i portafogli", x,",calcolato dai dati è pari a ", if(i == 1) sr.tan else sr.tan.ns,
                "e non è compreso nell'intervallo di confidenza"))
  }
  
  print(paste("Intervallo di confidenza: [", ci[1], "-", ci[2],"]"))
  print("-----------------------------------")
}

# ------ ANALISI DELLA NORMALITÀ DELLE STIME BOOTSTRAP ------
# Configura layout grafico come griglia 2x2
par(mfrow = c(2, 2))

# Crea grafici per entrambi i portafogli
for (i in 1:2) {
  # Identifica il tipo di portafoglio
  portfolio_type <- if (i == 1) "Short selling" else "No Short selling"
  
  # Crea istogramma delle stime bootstrap con sovrapposta curva di densità normale
  hist(boot_results[[i]]$t, 
       freq = FALSE,
       col = "lightblue", 
       border = "black",
       main = paste("Bootstrap -", portfolio_type),
       xlab = "Ratio di Sharpe",
       ylab = "Densità")
  
  # Aggiunge curva della distribuzione normale teorica basata sulla media e sd delle stime
  curve(dnorm(x, mean = mean(boot_results[[i]]$t), sd = sd(boot_results[[i]]$t)), 
        add = TRUE, col = "red", lwd = 2)
  
  # Crea grafico a linee delle stime bootstrap per visualizzare la loro variabilità
  plot(boot_results[[i]]$t, 
       type = "l", 
       col = "blue", 
       main = paste("SR Bootstrap -", portfolio_type),
       xlab = "Rimostrazioni", 
       ylab = "Valori del SR")
}

# Analisi grafica della normalità delle stime bootstrap dello Sharpe Ratio
# Loop per creare istogrammi e Q-Q plot per entrambi i portafogli
for (i in 1:2) {
  # Identifica il tipo di portafoglio
  portfolio_type <- if (i == 1) "Short selling" else "No Short selling"
  
  # Estrae le stime bootstrap per l'iterazione corrente
  bootstrap_data <- boot_results[[i]]$t
  
  # Crea istogramma con sovrapposta curva di densità normale
  hist(bootstrap_data, 
       freq = FALSE,
       col = "lightblue", 
       border = "black",
       main = paste("Distribuzione Bootstrap -", portfolio_type),
       xlab = "Ratio di Sharpe",
       ylab = "Densità")
  
  # Aggiunge curva di densità normale teorica
  curve(dnorm(x, mean = mean(bootstrap_data), sd = sd(bootstrap_data)), 
        add = TRUE, col = "red", lwd = 2)
  
  # Crea Q-Q plot per verificare visivamente la normalità delle stime
  qqnorm(bootstrap_data, 
         main = paste("Q-Q Plot -", portfolio_type),
         pch = 19)  # Punti solidi
  qqline(bootstrap_data, col = "red", lwd = 2)  # Linea di riferimento
}

# Ripristina il layout grafico predefinito
par(mfrow = c(1, 1))

# ------ PREPARAZIONE PER TEST STATISTICO ------
# Prepara i dati per il test statistico sulla differenza tra i due Sharpe ratio
# Assicura che entrambe le serie abbiano la stessa lunghezza
min_length <- min(length(returns_tan), length(returns_tan_ns))
returns_combined <- cbind(returns_tan[1:min_length], returns_tan_ns[1:min_length])

# ------ TEST STATISTICO PER LA DIFFERENZA DEGLI SHARPE RATIO ------
# Calcola la dimensione del campione per entrambi i portafogli
n1 = n2 = length(returns_xts[smpl][,1])
# Calcola le deviazioni standard delle stime bootstrap
s1 <- sd(boot_results[[1]]$t)  # Deviazione standard delle stime con vendite allo scoperto
s2 <- sd(boot_results[[2]]$t)  # Deviazione standard delle stime senza vendite allo scoperto
# Calcola l'errore standard della differenza tra gli Sharpe ratio
se <- sqrt((s1^2 / n1) + (s2^2 / n2))
# Calcola la statistica t per il test sulla differenza
t_stat <- (sr.tan - sr.tan.ns) / se
# Visualizza il valore della statistica t
print(t_stat)

# Calcola i gradi di libertà usando la formula di Welch-Satterthwaite per varianze diverse
df <- ((s1^2 / n1 + s2^2 / n2)^2) / ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1))
# Calcola il p-value per il test a due code
p_value <- 2 * (1 - pt(abs(t_stat), df))

# ------ INTERPRETAZIONE DEI RISULTATI ------
# Verifica la significatività statistica della differenza tra i due Sharpe ratio
if (p_value < 0.01) {
  print("La differenza tra i due Sharpe Ratio è statisticamente significativa con un livello di significità pari al 99%.")
} else {
  print("La differenza tra i due Sharpe Ratio NON è statisticamente significativa con un livello di significità pari al 99%..")
}