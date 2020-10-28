# Units file

# Greek letters
units <- list(
  mu = "μ",
  sigma = "σ",
  # Units
  CM = "cm",
  G = "g",
  HA = "ha",
  DEG = "\U00B0",
  # Superscript Operators
  NEG = "\U207B",
  POS = "\U207A",

  # Superscript numbers
  ZERO = "\U2070",
  ONE = "\U00B9",
  TWO = "\U00B2",
  THREE = "\U00B3"
  # # Usefull combinations
  # PER_HA <- paste(HA, NEG, ONE, sep = ""),
  # PER_CM3 <- paste(CM, NEG, THREE, sep = "")
)


arn::export(units)
