# cruzDisplaySymbolProp for CruzPlot; modified version of display.codes()
#   Creates a display of options for symbols, colors, line types, typefaces, and fonts

cruzDisplaySymbolProp <- reactive({
  oldpar <- par(mar = rep(1, 4), family = "sans")
  plot(c(0, 1), c(0, 28), type = "n", axes = FALSE, bty = "n")

  # Top labels
  text(c(0.05, 0.32, 0.7, 0.95), rep(26.5, 4), c("Symbols", "Colors", "Line Types", "Widths"), cex = 1.4)
  #text(c(0.05,0.3,0.7,0.95),rep(26.5,4),c("(pch= )","(col= )","(lty= )","(lwd= )"),cex=1.3)

  # Symbols
  for(i in 0:20) {
    text(0.02, 25 - (i * 1.25), i, cex = 1)
    points(.08, 25 - (i * 1.25), pch = i, cex = 1.8)
  }

  # Colors used in CruzPlot
  for(i in 1:length(symbol.col)) {
    text(0.33, 26.15 - (i + .25), symbol.col[i], cex = 1, pos = 2)
    points(.35, 26.25 - (i + .25), pch = 15, cex = 2.1, col = symbol.col.code[i])
  }
  points(0.35,  (26.25 - (length(symbol.col) + .25)), pch = 0, cex = 2.0)					# puts box around white
  text(0.20, 17.3, "Color", srt = 90, cex = 1.2)

  # Grayscale palette
   palette(gray(0:5 / 5))
  for(i in 1:6) {
    text(0.33, 6.2 - (i + .2), symbol.col.gray[i], cex = 1, pos = 2)
    points(.35, 6.3 - (i + .2), pch = 15, cex = 2.1, col = symbol.col.code.gray[i])
  }
  points(0.35, 0.11, pch = 0, cex = 2.0)					# puts box around white
  text(0.20, 2.7, "Gray Scale", srt = 90, cex = 1.2)

  # Line types
  for(i in 1:6) {
    text(0.55, 25 - i, i, cex = 1.1)
    lines(c(0.6, 0.7, 0.85), c(25 - i, 26 - i, 26 - i), lty = i)}
  # line widths
  for(i in 1:6) {
    lines(c(0.9, 1), c(26 - i, 26 - i), lwd = i)
  }

  # Typefaces
  text(0.75, 14, "Fonts", cex = 1.4)
  for(i in 1:3) {
    text(0.75, 15.5 - 2.3 * (i * 1.5), names(font.family)[i], cex = 1.2, adj = 0.5, family = font.family.vals[i])
    text(0.75, 14.6 - 2.3 * (i * 1.5), paste(LETTERS, collapse = ""), cex = 1, adj = 0.5, family = font.family.vals[i])
    text(0.75, 13.7 - 2.3 * (i * 1.5), paste(c(letters, " ", 0:9), collapse = ""), cex = 1, adj = 0.5, family = font.family.vals[i])
  }

  palette("default")
  par(oldpar)
})
