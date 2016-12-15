library(shiny)
set.seed(20161214)

ui <- fluidPage(
  titlePanel("Tareas"),
  sidebarLayout(
    sidebarPanel(
      #h4('Tarea 1 Funcion Inversa'),
      #h6(''),
      #
    h6('')
      
    ),
    mainPanel(
      
      h2("Tarea1: Función Inversa"),
      h4('Variables de Entrada'),
      fluidRow(
        column(5, sliderInput("lambda", "Lambda", min=0.0001, max=10, value=1)),
        column(5, sliderInput("nsim", "Números Aleatorios", min=1, max=5000, value=1000))
      ),
      h4('Salida'),
      plotOutput("grafica"),
      verbatimTextOutput("chiTest"),
      h2("Tarea2: Integración numérica"),
      p("#Se puede poner cualquier función"),
      p("f = function(x) x^2 + 1   "),
      p("curve(f(x), -2,2, ylim=c(0, 5))"),
      p("#Sólo para visualizar se agregan puntos aleatorios a la gráfica:"),
      p("points(runif(1000, -2, 2), runif(1000, 0, 5))"),
      p("#Número de simulaciones Monte Carlo:"),
      p("N = 1000"),
      p("#Evalúo la función y reviso qué puntos caen ahí"),
      p("sum(f(runif(N, -2, 2)) > runif(N, 0, 5))/N *(4*5)"),
      p("f(runif(1, -2, 2)) > runif(1, 0, 5)"),
      p(" a <- runif(1,-2,2)"),
      p(" f(a) #imprimiendo el resultado de la integración"),
      p("b <- runif(1,0,5)"),
      h2("Tarea3: MCMC"),
      p("##############################################################################################"),
      p("######Aquí se están creando los datos, suponiendo una relación lineal con algo de ruido######"),
      p("##############################################################################################"),
      p("verdaderoA <- 5"),
      p("verdaderoB<- 0"),
      p("verdaderaSD <- 10"),
      p("tamanio_muestra <- 31"),
      p("x <- (-(tamanio_muestra-1)/2):((tamanio_muestra-1)/2)"),
      p("y <-  verdaderoA * x + verdaderoB+ rnorm(n=tamanio_muestra,mean=0,sd=verdaderaSD) #Errores normales"),
      p("#Graficando la información"),
      p("plot(x,y, main='Datos de prueba')"),
      p("########################################"),
      p("#Obteniendo la función de verosimilitud#"),
      p("########################################"),
      p("#Modelo lineal y = b + a*x + N(0,sd), entonces tomamos los parámetros (a,b,sd)"),
      p("verosimilitud <- function(param){"),
      p("  a = param[1]"),
      p("  b = param[2]"),
      p("  sd = param[3]"),
      p("  prediccion = a*x + b"),
      p("  verosim_individuales= dnorm(y, mean = prediccion, sd = sd, log = T)"),
      p("  sumll = sum(verosim_individuales)"),
      p("  return(sumll)   "),
      p("}"),
      p("######################################################"),
      p("#Definiendo la prior para cada uno de los parámetros##"),
      p("######################################################"),
      p("prior <- function(param){"),
      p("  a = param[1]"),
      p("  b = param[2]"),
      p("  sd = param[3]"),
      p("  aprior = dunif(a, min=0, max=10, log = T)"),
      p("  bprior = dnorm(b, sd = 5, log = T)"),
      p("  sdprior = dunif(sd, min=0, max=30, log = T) #errores"),
      p("  return(aprior+bprior+sdprior)"),
      p("}"),
      p("######################################################"),
      p("##Posterior: Prior*verosimilitud######################"),
      p("######################################################"),
      p("#Se suma por la ley de logaritmos#"),
      p("posterior <- function(param){"),
      p("  return (verosimilitud(param) + prior(param))"),
      p("}"),
      p("########################################################"),
      p("###########MCMC: simulando la distribución posterior#########"),
      p("########################################################"),
      p("######Algoritmo de Metrópolis"),
      p("funcion_propuesta <- function(param){"),
      p("  return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))"),
      p("}"),
      p("correr_metropolis_MCMC <- function(valor_inicial, iteraciones){"),
      p("  cadena = array(dim = c(iteraciones+1,3))"),
      p("  cadena[1,] = valor_inicial"),
      p("  for (i in 1:iteraciones){"),
      p("    proposal = funcion_propuesta(cadena[i,])"),
      p("    "),
      p("    probabilidad = exp(posterior(proposal) - posterior(cadena[i,])) #p1/p2 = exp[log(p1)-log(p2)"),
      p("    if (runif(1) < probabilidad){"),
      p("      cadena[i+1,] = proposal"),
      p("    }else{"),
      p("      cadena[i+1,] = cadena[i,]"),
      p("    }"),
      p("  }"),
      p("  return(cadena)"),
      p("}"),
      p("valor_inicial = c(4,0,10)"),
      p("cadena = correr_metropolis_MCMC(valor_inicial, 10000)"),
      p("calentamiento = 5000"),
      p("aceptacion = 1-mean(duplicated(cadena[-(1:calentamiento),]))"),
      p("####Información de resumen####"),
      p("### Summary: #######################"),
      p("par(mfrow = c(2,3))"),
      p("hist(cadena[-(1:calentamiento),1],nclass=30, , main='Posterior de a', xlab='Valor verdadero = línea roja' )"),
      p("abline(v = mean(cadena[-(1:calentamiento),1]))"),
      p("abline(v = verdaderoA, col='red')"),
      p("hist(cadena[-(1:calentamiento),2],nclass=30, main='Posterior of b', xlab='Valor verdadero = línea roja')"),
      p("abline(v = mean(cadena[-(1:calentamiento),2]))"),
      p("abline(v = verdaderoB, col='red' )"),
      p("hist(cadena[-(1:calentamiento),3],nclass=30, main='Posterior de sd', xlab='Valor verdadero = línea roja')"),
      p("abline(v = mean(cadena[-(1:calentamiento),3]) )"),
      p("abline(v = verdaderaSD, col='red')"),
      p("plot(cadena[-(1:calentamiento),1], type = 'l', xlab='Valor verdadero = línea roja' , main = 'Valores de la cadena de a', )"),
      p("abline(h = verdaderoA, col='red')"),
      p("plot(cadena[-(1:calentamiento),2], type = 'l', xlab='Valor verdadero = línea roja' , main = 'Valores de la cadena de b', )"),
      p("abline(h = trueB, col='red')"),
      p("plot(cadena[-(1:calentamiento),3], type = 'l', xlab='Valor verdadero = línea roja' , main = 'Valores de la cadena de sd', )"),
      p("abline(h = verdaderaSD, col='red' )"),
      p("#Muestra las estimaciones de los parámetros y la cadena de Markov de los parámetros"),
      p("summary(lm(y~x))")
      
    )
  )
)


server <- function(input, output) {
  data <- reactive({
    rinv <- function(u, lambda) {return(-log(1-u)/lambda)}
    u <- runif(input$nsim)
    x <- rinv(u, input$lambda)
  })
  
  chiTest <- reactive({
    breaks <- c(seq(0,10, by=1))
    o <- table(cut(data(), breaks = breaks))
    p <- diff(pexp(breaks))
    chisq.test(o, p=p, rescale.p = T)
  })
  
  data_exp <- reactive({
    disexp <- rexp(input$nsim, rate=input$lambda)
  })
  output$grafica <- renderPlot({
    hist(data(), main = 'Distribución', ylab = 'Frecuencias', xlab = 'Datos', col = 'steelBlue', 
         border = 'gray')
  })
  output$chiTest <- renderPrint({
    chiTest()
  })
  
  
}


shinyApp(ui = ui, server = server)

