#### Translation Function ----

Translate <- function(df, var, values_from, values_to) {
  # Change the values of a specific variable (val) from values_from to values_to
  df |> mutate(!!var := case_when( 
    .data[[var]] %in% values_from ~ values_to[match(.data[[var]], values_from)], # Replace values in the specific column "var" to values_to
    TRUE ~ as.character(.data[[var]]) # Keep other values unchanged
    ),
    !!var := as_factor(.data[[var]])  # Convert back to factor after translation
  )
}
  