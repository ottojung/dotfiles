Config {
    font = "xft:monospace-15",
    bgColor = "#000000",
    fgColor = "#ffffff",
    commands = [
        Run Date "%a %b %_d %l:%M" "date" 10,
        Run StdinReader
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "%StdinReader% }{ <fc=#FFFFCC>%date%</fc>"
}
