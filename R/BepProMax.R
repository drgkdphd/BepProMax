#' Title: To calculate Break-even Points and Profit maximizing output Levels
#'
#' @param datafile
#'
#' @returns
#' @export
#'
#' @examples
  BepProMax <- function(datafile) {
  # Estimate Total Revenue (TR) and Total Cost (TC) models
  TR_model <- lm(TotRev ~ Q + I(Q^2), data = datafile)
  TC_model <- lm(TotCos ~ Q + I(Q^2), data = datafile)

  print(summary(TR_model))
  print(summary(TC_model))

  # Extract coefficients from TR and TC models
  tr_intercept <- coef(TR_model)[1]
  tr_slope <- coef(TR_model)[2]
  tr_slope_squared <- coef(TR_model)[3]

  tc_intercept <- coef(TC_model)[1]
  tc_slope <- coef(TC_model)[2]
  tc_slope_squared <- coef(TC_model)[3]

  # TR function
  tr_function <- function(Q) {
    tr_intercept + tr_slope * Q + tr_slope_squared * Q^2
  }

  # TC function
  tc_function <- function(Q) {
    tc_intercept + tc_slope * Q + tc_slope_squared * Q^2
  }

  # Define the Break even function (TR = TC)
  Break_Even <- function(Q) {
    (tr_intercept + tr_slope * Q + tr_slope_squared * Q^2) -
      (tc_intercept + tc_slope * Q + tc_slope_squared * Q^2)
  }

  # Generate a sequence of Q values
  Q_values <- seq(0, 30, by = 0.1)

  # Calculate the corresponding Break-Even values
  BE_values <- Break_Even(Q_values)

  # Find the indices where the sign changes
  sign_change_indices <- which(BE_values * c(BE_values[-1], 0) < 0)

  # Extract the corresponding Q values
  q_star1 <- Q_values[sign_change_indices[1]]
  q_star2 <- Q_values[sign_change_indices[2]]

  # Define the profit function (TR - TC)
  profit_function <- function(Q) {
    tr_intercept + tr_slope * Q + tr_slope_squared * Q^2 -
      (tc_intercept + tc_slope * Q + tc_slope_squared * Q^2)
  }
  # Calculate the derivative of the profit function (MR - MC)
  profit_derivative <- function(Q) {
    (tr_slope + 2 * tr_slope_squared * Q) - (tc_slope + 2 * tc_slope_squared * Q)
  }
  # Define the equation to solve
  equation <- function(Q) {
    profit_derivative(Q)
  }
  # Solve the equation for Q
  q_star3 <- uniroot(equation, c(0, 30))$root

  # Evaluate the Maximum profit function at  equilibrium Quantity - q_star3
  Maxprofit <- profit_function(q_star3)


  # Calculate the second-order derivative of the profit function
  pt_qq <- function(Q) {
    2 * tr_slope_squared - 2 * tc_slope_squared
  }
  # Evaluate the second-order derivative at q_star
  pt_qqv <- pt_qq(q_star3)

  # Check the second-order condition for maximization
  if (pt_qqv < 0) {
    print("The second-order condition for maximization is satisfied.")
  } else {
    print("The second-order condition for maximization is not satisfied.")
  }
  # Create a table with Q values and corresponding TR, TC, and TP values
  table <- tibble(Q = seq(0, 30, 0.1))
  table <- table %>%
    mutate(
      TR = tr_function(Q),
      TC = tc_function(Q),
      TP = profit_function(Q)
    )

  # Plot TR, TC, and TP curves
  p <- table %>%
    pivot_longer(-Q, names_to = "Curve", values_to = "Value") %>%
    ggplot(aes(x = Q, y = Value, color = Curve)) +
    geom_line() +
    geom_vline(xintercept = q_star1, linetype = "dashed") +
    geom_vline(xintercept = q_star2, linetype = "dashed") +
    geom_vline(xintercept = q_star3, linetype = "dashed") +
    ggtitle("Total Revenue, Total Cost, and Total Profit") +
    theme_economist()

  print(p)

  # Derive the marginal revenue function MR_expr
  MR_expr <- Deriv(~ tr_c + tr_s1 * Q + tr_s2 * Q^2, "Q")

  # Derive the marginal cost function MC_expr
  MC_expr <- Deriv(~ tc_c + tc_s1 * Q + tc_s2 * Q^2, "Q")

  #Define the MR and MC functions

  MR <- function(Q){eval(MR_expr, envir = list(Q = Q, tr_c = tr_intercept, tr_s1 = tr_slope, tr_s2 = tr_slope_squared))}
  MC <- function(Q){eval(MC_expr, envir = list(Q = Q, tc_c = tc_intercept, tc_s1 = tc_slope, tc_s2 = tc_slope_squared))}

  # Calculate Equilibrium Quantity

  Q_1 <- uniroot(function(Q) MR(Q) - MC(Q), c(0, 30))$root

  #Calculate at Equilibrium Quantity

  TR_1 <- tr_function(Q_1)
  TC_1 <- tc_function(Q_1)
  MR_1 <- MR(Q_1)
  MC_1 <- MC(Q_1)
  AR_1 <- TR_1/Q_1
  AC_1 <- TC_1/Q_1
  profit_1 <- TR_1 - TC_1

  #Create a sequence of quantities
  Q <- seq(0,Q_1*1.5, by = 1)

  #Calculate the values for each curve
  TR_values <- sapply(Q, function(x) tr_function(x))
  TC_values <- sapply(Q, function(x) tc_function(x))
  MR_values <- sapply(Q, function(x) MR(x))
  MC_values <- sapply(Q, function(x) MC(x))
  AR_values <- TR_values/Q
  AC_values <- TC_values/Q
  profit_values <- TR_values - TC_values

  #Create a data frame

  df <- data.frame(Q, TR = TR_values, TC = TC_values, AR = AR_values, AC = AC_values, Profit = profit_values, MR = MR_values, MC = MC_values)

  #1. Plot the curves
  print(ggplot(df, aes(x = Q)) +
          geom_line(aes(y = TR, color = "TR")) +
          geom_line(aes(y = TC, color = "TC")) +
          geom_line(aes(y = MR, color = "MR")) +
          geom_line(aes(y = MC, color = "MC")) +
          geom_line(aes(y = AR, color = "AR")) +
          geom_line(aes(y = AC, color = "AC")) +
          geom_line(aes(y = Profit, color = "Profit")) +
          geom_vline(xintercept = Q_1, color = "red", linetype = "dashed") +
          labs(title = "TR, TC, AR, AC, MR, MC, and Profit Curves", x = "Quantity", y = "Value") +
          scale_color_manual(values = c("TR" = "blue", "TC" = "red", "MR" = "green", "MC" = "purple", "AR" = "orange", "AC" = "brown", "Profit" = "darkgreen")) +theme_economist())


  # 2 Plot the curves
  print(
    ggplot(df, aes(x = Q)) +
      geom_line(aes(y = TR, color = "TR")) +
      geom_line(aes(y = TC, color = "TC")) +
      geom_line(aes(y = Profit, color = "Profit")) +
      geom_vline(xintercept = Q_1, color = "red", linetype = "dashed") +
      labs(title = "TR, TC, and Profit Curves", x = "Quantity", y = "Value") +
      scale_color_manual(values = c("TR" = "blue", "TC" = "red",  "Profit" =
                                      "darkgreen")) + theme_economist()
  )

  # 3 Plot the curves
  print(
    ggplot(df, aes(x = Q)) +
      geom_line(aes(y = MR, color = "MR")) +
      geom_line(aes(y = MC, color = "MC")) +
      geom_line(aes(y = Profit, color = "Profit")) +						      geom_vline(xintercept = Q_1, color = "red", linetype = "dashed") +			      labs(title = "MR, MC, and Profit Curves", x = "Quantity", y = "Value") +		      scale_color_manual(values = c("MR" = "blue", "MC" = "red",  "Profit" = "darkgreen")) + theme_economist()
  )

  #Create matrices to store the results
  Summary_of_Results <- matrix(c(
    round(q_star1, 4),
    round(q_star2, 4),
    round(q_star3, 4),
    round(TR_1, 4),
    round(TC_1, 4),
    round(MR_1, 4),
    round(MC_1,4),
    round(AR_1, 4),
    round(AC_1, 4),
    round(profit_1,4)),
    nrow = 10,ncol = 1,
    dimnames = list(c(
      "Break_Even_Point_1                       =",
      "Break_Even_Point_2                       =",
      "Equilibrium Quantity/Optimum_Output      =",
      "Total Revenue at Equilibrium Quantity    =",
      "Total Cost at Equilibrium Quantity       =",
      "Marginal Revenue at Equilibrium Quantity =",
      "Marginal Cost at Equilibrium Quantity    =",
      "Average Revenue(or)Price at Equilibrium Quantity =",
      "Average Cost at Equilibrium Quantity     =",
      "Maximum Profit at Equilibrium Quantity   ="), c("Value")))

  #Return the Summary of results
  return(Summary_of_Results)
}
