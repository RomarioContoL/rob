#' Assignment-Expansion method
#'
#' @param z vector with the levels of the factor
#'
#' @return order of experimentation with bias and number of level changes in balance
#'
#' @examples
#' z<-c(2,2,2,2,2,2)
#' runorder(z)
#' z<-c(4,3,2,3,2)
#' runorder(z)
#' z<-c(3,3,2,4)
#' runorder(z)
#'
#' @importFrom FMC gen.level
#' @importFrom minimalRSD num.ch
#'
#' @export
runorder<- function (z)
{
  #Ordenamientos con mínimo número de cambios de nivel y en equilibrio (según el caso)
  run22 <- matrix(c(-1, -1, 1, 1, -1, 1, 1, -1),nrow = 4,byrow = T)
  run23 <- matrix(c(-1, 1, 1, -1, -1, -1, 1, 1, 1, 0, -1, 0),nrow = 6,byrow = T)
  run24 <- matrix(c(1, -1, 1, 1, -1, 1, -1, -1, -1, -2, -1, 2, 1, 2, 1, -2),nrow = 8,byrow = T)
  run25 <- matrix(c(-1, -1, -1, 1, 1, 1, 1, -2, 1, -1, 1, 2, -1, 2, -1, -2, -1, 0, 1, 0),nrow = 10,byrow = T)
  run26 <- matrix(c(1, -1, 1, -2, 1, 3, -1, 3, -1, -2, -1, -1, -1, 1, -1, 2, -1, -3, 1, -3, 1, 2, 1, 1),nrow = 12,byrow = T)

  run32 <- matrix(c(0, 1, 1, -1, -1, -1, -1, 1, 1, 1, 0, -1),nrow = 6,byrow = T)
  run33 <- matrix(c(1, -1, -1, -1, -1, 1, 0, 1, 1, 1, 1, 0, -1, 0, 0, 0, 0, -1),nrow = 9,byrow = T)
  run34 <- matrix(c(0, -2, 0, 1, 1, 1, -1, 1, -1, -2, 1, -2, 1, 2, -1, 2, 0, 2, 0, -1, -1, -1, 1, -1),nrow = 12,byrow = T)
  run35 <- matrix(c(-1, -1, 0, -1, 0, 2, 0, 0, 0, -2, 0, 1, -1, 1, 1, 1, 1, 2, 1, -1, 1, 0, 1, -2, -1, -2, -1, 2, -1, 0),nrow = 15,byrow = T)
  run36 <- matrix(c(-1, -2, -1, 3, -1, -3, 0, -3, 0, 3, 1, 3, 1, 2, 1, -2, 1, -3, 1, -1, 1, 1, 0, 1, 0, 2, -1, 2, -1, 1, -1, -1, 0, -1, 0, -2),nrow = 18,byrow = T)

  run42 <- matrix(c(-1, -1, 1, -1, 1, 1, -1, 1, 2, 1, -2, 1, -2, -1, 2, -1),nrow = 8,byrow = T)
  run43 <- matrix(c(1, 0, 1, 1, 1, -1, -2, -1, -2, 1, -2, 0, -1, 0, 2, 0, 2, -1, 2, 1, -1, 1, -1, -1),nrow = 12,byrow = T)
  run44 <- matrix(c(1, 2, 1, -2, -2, -2, -2, 2, -1, 2, 2, 2, 2, -1, 2, -2, -1, -2, -1, 1, -1, -1, 1, -1, -2, -1, -2, 1, 2, 1, 1, 1),nrow = 16,byrow = T)
  run45 <- matrix(c(-1, 1, 1, 1, -2, 1, -2, -2, -2, -1, -1, -1, 2, -1, 1, -1, 1, 2, 2, 2, 2, 0, 2, 1, 2, -2, 1, -2, 1, 0, -2, 0, -2, 2, -1, 2, -1, -2, -1, 0),nrow = 20,byrow = T)
  run46 <- matrix(c(-2, 3, -2, -1, -2, -3, 2, -3, 2, 3, 2, 1, 2, -1, 1, -1, 1, -2, 1, 3, -1, 3, -1, 2, -1, 1, -1, -1, -1, -2, -1, -3, 1, -3, 1, 2, 1, 1, -2, 1, -2, -2, -2, 2, 2, 2, 2, -2),nrow = 24,byrow = T)

  run52 <- matrix(c(2, 1, -2, 1, -2, -1, 2, -1, -1, -1, 1, -1, 1, 1, -1, 1, 0, 1, 0, -1),nrow = 10,byrow = T)
  run53 <- matrix(c(-2, 0, -2, 1, -2, -1, 2, -1, 2, 0, 2, 1, 1, 1, 1, -1, 1, 0, 0, 0, 0, -1, 0, 1, -1, 1, -1, -1, -1, 0),nrow = 15,byrow = T)
  run54 <- matrix(c(2, 1, 2, -2, -1, -2, -2, -2, -2, -1, -2, 2, -1, 2, 1, 2, 2, 2, 2, -1, 1, -1, 1, 1, -2, 1, -1, 1, -1, -1, 0, -1, 0, 1, 0, 2, 0, -2, 1, -2),nrow = 20,byrow = T)
  run55 <- matrix(c(-2, 2, 0, 2, 2, 2, 2, 0, 0, 0, 0, -1, 0, -2, -2, -2, -2, -1, -2, 0, 1, 0, 1, 2, 1, -1, 2, -1, 2, -2, 1, -2, -1, -2, -1, 2, -1, -1, -1, 0, -1, 1, -2, 1, 1, 1, 2, 1, 0, 1),nrow = 25,byrow = T)
  run56 <- matrix(c(-1, -3, -1, 3, -1, 2, -1, -2, -1, 1, 0, 1, 0, -3, 2, -3, 2, 3, 2, 2, 0, 2, 1, 2, 1, -3, 1, 1, 1, -1, -1, -1, 2, -1, 2, 1, -2, 1, -2, -2, -2, -3, -2, 2, -2, 3, -2, -1, 0, -1, 0, -2, 2, -2, 1, -2, 1, 3, 0, 3),nrow = 30,byrow = T)

  run62 <- matrix(c(3, -1, -1, -1, -2, -1, -2, 1, -1, 1, 2, 1, 3, 1, 1, 1, -3, 1, -3, -1, 2, -1, 1, -1),nrow = 12,byrow = T)
  run63 <- matrix(c(-1, 1, 1, 1, 3, 1, 3, -1, -1, -1, -2, -1, 1, -1, -3, -1, -3, 1, -3, 0, 3, 0, -1, 0, 2, 0, 1, 0, -2, 0, -2, 1, 2, 1, 2, -1),nrow = 18,byrow = T)
  run64 <- matrix(c(-1, 1, 2, 1, 2, -2, 2, 2, 2, -1, 1, -1, 1, -2, 1, 2, -2, 2, -2, -1, -2, -2, -3, -2, -3, -1, -3, 2, -3, 1, -2, 1, 1, 1, 3, 1, 3, -1, -1, -1, -1, 2, -1, -2, 3, -2, 3, 2),nrow = 24,byrow = T)
  run65 <- matrix(c(-1, -1, -2, -1, 1, -1, -3, -1, -3, 1, 3, 1, 3, 2, 2, 2, 2, -2, 2, -1, 2, 0, -3, 0, -2, 0, 1, 0, 1, 1, -1, 1, -1, -2, 1, -2, 1, 2, -3, 2, -2, 2, -1, 2, -1, 0, 3, 0, 3, -1, 3, -2, -3, -2, -2, -2, -2, 1, 2, 1),nrow = 30,byrow = T)
  run66 <- matrix(c(-3, -1, -3, 1, 1, 1, 1, -3, 1, -1, 3, -1, -1, -1, -1, 1, 2, 1, -2, 1, 3, 1, 3, 2, -2, 2, -1, 2, -1, -2, -2, -2, 1, -2, 1, 2, -3, 2, -3, -2, 3, -2, 2, -2, 2, 3, 2, -1, 2, 2, 2, -3, -2, -3, -2, -1, -2, 3, 1, 3, -1, 3, -3, 3, 3, 3, 3, -3, -1, -3, -3, -3),nrow = 36,byrow = T)

  run2_3 <- matrix(c(-1, 1, -1, 1, -1, -1, 1, -1, 1, 1, 1, 1, -1, 1, 1, -1, -1, 1, -1, -1, -1, 1, 1, -1),nrow = 8,byrow = T)
  run2_4 <- matrix(c(-1, 1, 1, 1, -1, 1, -1, 1, 1, 1, -1, -1, 1, -1, -1, -1, 1, -1, -1, 1, 1, -1, 1, 1, -1, -1, 1, 1, -1, -1, 1, -1, 1, -1, 1, -1, 1, 1, 1, -1, -1, 1, 1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, 1, 1, -1, 1, 1, 1, 1, 1),nrow = 16,byrow = T)
  run2_5 <- matrix(c(1, 1, 1, -1, -1, 1, -1, 1, -1, -1, -1, -1, 1, -1, -1, -1, -1, 1, 1, -1, -1, -1, -1, 1, -1, -1, 1, -1, 1, -1, 1, 1, -1, 1, -1, 1, 1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 1, -1, 1, 1, -1, 1, -1, 1, 1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 1, -1, -1, 1, -1, 1, 1, -1, 1, -1, 1, 1, 1, 1, -1, 1, -1, 1, 1, -1, 1, -1, 1, -1, -1, 1, -1, -1, -1, -1, 1, 1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 1, -1, 1, 1, 1, 1, 1, 1, 1, 1, -1, 1, 1, 1, 1, -1, 1, 1, 1, -1, 1, 1, 1, 1, -1, 1, -1, 1, 1, -1, 1, -1, -1, 1, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, -1, -1, -1, -1, 1, 1, -1, -1),nrow = 32,byrow = T)

  runs <- list(run22 = run22, run23 = run23, run24 = run24, run25 = run25,run26 = run26,
               run32 = run32, run33 = run33, run34 = run34, run35 = run35,run36 = run36,
               run42 = run42, run43 = run43, run44 = run44, run45 = run45,run46 = run46,
               run52 = run52, run53 = run53, run54 = run54, run55 = run55,run56 = run56,
               run62 = run62, run63 = run63, run64 = run64, run65 = run65,run66 = run66,
               run222 = run2_3, run2222 = run2_4, run22222 = run2_5)


  # clave para la matriz dependiendo del número de elementos iguales a 2
  if (length(z) == 3 && all(z[1:3]==2)) {
    key <- paste0("run", paste(z[1:3], collapse = ""))
  } else if (length(z) > 3 && all(z[1:3]==2) && z[4]!=2) {
    key <- paste0("run", paste(z[1:3], collapse = ""))
  } else if (length(z) == 4 && all(z[1:4]==2)) {
    key <- paste0("run", paste(z[1:4], collapse = ""))
  } else if (length(z) > 4 && all(z[1:4]==2) && z[5]!=2) {
    key <- paste0("run", paste(z[1:4], collapse = ""))
  } else if (length(z) == 5 && all(z[1:5]==2)) {
    key <- paste0("run", paste(z[1:5], collapse = ""))
  } else if (length(z) > 5 && all(z[1:5]==2)) {
    key <- paste0("run", paste(z[1:5], collapse = ""))
  } else {
    # En caso de que no haya tres, cuatro, o cinco elementos iguales a 2
    key <- paste0("run", paste(z[1:2], collapse = ""))
  }

  run <- runs[[key]]

  if (length(z) < 2) {
    print("The number of levels must be greater than or equal to 2")
  }
  else if (any(z[1:2] < 2 | z[1:2] > 6)){
    print("ERROR: the levels of the first two factors must be an integer between 2 and 6.")
  }

  else if (length(z) == 2 || length(z) < 6 &&all(z==2)) {
    d2 <- minimalRSD::num.ch(run)
    f <- seq(1:nrow(run))
    m <- run*f
    s <- apply(m, 2, sum)
    print(list(Run_Order = run, Time_Count = s, Changes_byFactor = d2[[1]],
               Total_Changes = d2[[2]]))
  }

  else if ((max(z%%1) == 0) && (min(z) >= 2) && ncol(run)==3) {
    n <- length(z)
    des <- vector("list", (n-ncol(run)+1))
    des[[1]] <- run
    if (n > 2) {
      for (i in 3:(n-1)) {
        x <- gen.level(z[i+1])
        y <- z[i+1]
        des[[i-1]] <- adcol(x,y,z[c(1:(i+1))], des[[(i - 2)]])
      }
    }
    d1 <- des[[n-2]]
    d2 <- minimalRSD::num.ch(d1)
    f <- seq(1:nrow(d1))
    m <- d1*f
    s <- apply(m, 2, sum)
    print(list(Run_Order = run, Time_Count = s, Changes_byFactor = d2[[1]],
               Total_Changes = d2[[2]]))
  }

  else if ((max(z%%1) == 0) && (min(z) >= 2) && ncol(run)==4) {
    n <- length(z)
    des <- vector("list", (n-ncol(run)+1))
    des[[1]] <- run
    if (n > 2) {
      for (i in 4:(n-1)) {
        x <- gen.level(z[i+1])
        y <- z[i+1]
        des[[i-2]] <- adcol(x,y,z[c(1:(i+1))], des[[(i - 3)]])
      }
    }
    d1 <- des[[n-3]]
    d2 <- minimalRSD::num.ch(d1)
    f <- seq(1:nrow(d1))
    m <- d1*f
    s <- apply(m, 2, sum)
    print(list(Run_Order = run, Time_Count = s, Changes_byFactor = d2[[1]],
               Total_Changes = d2[[2]]))
  }

  else if ((max(z%%1) == 0) && (min(z) >= 2) && ncol(run)==5) {
    n <- length(z)
    des <- vector("list", (n-ncol(run)+1))
    des[[1]] <- run
    if (n > 2) {
      for (i in 5:(n-1)) {
        x <- gen.level(z[i+1])
        y <- z[i+1]
        des[[i-3]] <- adcol(x,y,z[c(1:(i+1))], des[[(i - 4)]])
      }
    }
    d1 <- des[[n-4]]
    d2 <- minimalRSD::num.ch(d1)
    f <- seq(1:nrow(d1))
    m <- d1*f
    s <- apply(m, 2, sum)
    print(list(Run_Order = d1, Time_Count = s, Changes_byFactor = d2[[1]],
               Total_Changes = d2[[2]]))
  }

  else if ((max(z%%1) == 0) && (min(z) >= 2)) {
    n <- length(z)
    des <- vector("list", (n-1))
    des[[1]] <- run
    if (n > 2) {
      for (i in 2:(n-1)) {
        x <- gen.level(z[i+1])
        y <- z[i+1]
        des[[i]] <- adcol(x,y,z[c(1:(i+1))], des[[(i - 1)]])
      }
    }
    d1 <- des[[n-1]]
    d2 <- minimalRSD::num.ch(d1)
    f <- seq(1:nrow(d1))
    m <- d1*f
    s <- apply(m, 2, sum)
    print(list(Run_Order = d1, Time_Count = s, Changes_byFactor = d2[[1]],
               Total_Changes = d2[[2]]))
  }

  else{
    print("ERROR: levels should have integers greater than or equal to 2")
  }
}
