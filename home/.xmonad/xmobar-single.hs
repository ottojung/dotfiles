Config {
    font = "xft:monospace-15",
    position = Static { xpos = 0 , ypos = 0, width = 1920, height = 25 },
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
