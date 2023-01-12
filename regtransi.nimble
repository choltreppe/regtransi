# Package

version       = "0.1.0"
author        = "Joel Lienhard"
description   = "A interpreter for register transfer commands"
license       = "MIT"
srcDir        = "src"
bin           = @["regtransi"]


# Dependencies

requires "nim >= 1.6.6"
requires "parlexgen >= 0.1.1"
requires "cligen >= 1.5.37"
