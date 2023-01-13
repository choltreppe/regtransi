import std/[sequtils, strutils, strformat, tables, options, sugar]
import fusion/matching
import parlexgen
import cligen

type Val = BiggestInt
const MaxWordSize = sizeof Val

type
  LocationKind = enum locConst, locReg, locMem
  Location = object
    case kind: LocationKind
    of locConst: val: Val
    else: loc: string

  BinOp = enum opAdd="+", opSub="-", opMul="*", opDiv="/", opMod="%", opAnd="&", opOr="|", opXor="^", opShl="<<", opShr=">>"
  UnOp = enum opTransfer, opNot="~"

  CompOp = enum compEq="==", compNe="!=", compLt="<", compLe="<=", compGt=">", compGe=">="

  ExpKind = enum expBin, expUn
  Exp = object
    case kind: ExpKind
    of expUn:
      unOp: UnOp
      loc: Location
    of expBin:
      binOp: BinOp
      lhs, rhs: Location

  DebugOutKind = enum debugReg, debugMem
  DebugOut = object
    case kind: DebugOutKind
    of debugReg: name: string
    of debugMem:
      a,b: Option[Val]

  CmdKind = enum cmdAssgn, cmdJump, cmdDebug
  Cmd[T: string|int] = object
    case kind: CmdKind
    of cmdAssgn:
      res: Location
      exp: Exp
    of cmdJump:
      goto: T
      case withCond: bool
      of true:
        cond: (Exp, CompOp, Exp)
        gotoElse: T
      else: discard
    of cmdDebug:
      name: string
      debug: seq[DebugOut]

  Prog = seq[seq[Cmd[int]]]

func `$`(loc: Location): string =
  case loc.kind
  of locConst: $loc.val
  of locReg: loc.loc
  of locMem: fmt"M[{loc.loc}]"

func `$`(exp: Exp): string =
  case exp.kind
  of expUn:
    if exp.unOp == opTransfer: $exp.loc
    else: fmt"{exp.unOp}{exp.loc}"
  of expBin:
    fmt"{exp.lhs} {exp.binOp} {exp.rhs}"

func `$`(d: DebugOut): string = 
  case d.kind
  of debugReg: d.name
  of debugMem:
    func `$`(o: Option[Val]): string =
      if Some(@v) ?= o: $v
      else: ""
    "[" & $d.a & ".." & $d.b & "]"

func `$`(prog: Prog): string =
  for cmds in prog:
    result.add cmds.mapIt(
      case it.kind
      of cmdAssgn: fmt"{it.res} <- {it.exp}"
      of cmdJump:
        if it.withCond:
          fmt"goto if {it.cond} == 0 then {it.goto} else {it.gotoElse}"
        else:
          fmt"goto {it.goto}"
      of cmdDebug:
        "!" & it.name & " " & it.debug.mapIt($it).join(", ")
    ).join(": ")
    result.add ";\n"


proc parse(code: File): Prog =
  
  type
    TokenKind = enum
      IDENT, LIT, UOP, BOP, COMP,
      SEMIC=";", COLOMN=":", ARROW="<-", BRACL="[", BRACR="]", EMARK="!", COMMA=",", DDOT="..",
      GOTO, IF, THEN, ELSE
    Token = object
      case kind: TokenKind
      of IDENT: name: string
      of LIT: val: Val
      of UOP: unOp: UnOp
      of BOP: binOp: BinOp
      of COMP: comp: CompOp
      else: discard
      pos: int

  func escape(s: string): string =
    for c in s:
      result.add '\\'
      result.add c

  makeLexer lexCmd[Token]:
    (escape $opNot):
      Token(kind: UOP, unOp: opNot, pos: pos)

    for o in BinOp:
      (escape $o):
        Token(kind: BOP, binOp: o, pos: pos)

    for o in CompOp:
      (escape $o):
        Token(kind: COMP, comp: o, pos: pos)

    for k in SEMIC .. DDOT:
      (escape $k):
        Token(kind: k, pos: pos)

    for k in GOTO..ELSE:
      (($k).toLower & r"\s"):
        Token(kind: k, pos: pos)

    r"[0-9]+":
      Token(kind: LIT, val: parseBiggestInt(match), pos: pos)

    r"[a-zA-Z_][a-zA-Z0-9_]*":
      Token(kind: IDENT, name: match, pos: pos)

    r"\s+": continue

  var
    prog: seq[seq[Cmd[string]]]
    labels: Table[string, int]

  type ParseCmdError = ref object of CatchableError
    pos: int

  template compileError(m = "parsing error") {.dirty.} =
    raise ParseCmdError(
      msg: m,
      pos:
        if Some(@t) ?= token: t.pos
        else: 0
    )

  makeParser parseCmd[Token]:
    prog[seq[seq[Cmd[string]]]]:
      try:
        (cmds, SEMIC, prog): s[0] & s[2]
        (cmds, SEMIC): @[s[0]]
        cmds: @[s[0]]
      except: compileError()

    cmds[seq[Cmd[string]]]:
      try:
        (cmd, COLOMN, cmds): s[0] & s[2]
        cmd: @[s[0]]
      except: compileError()

    cmd[Cmd[string]]:
      try:
        (loc, ARROW, exp): Cmd[string](kind: cmdAssgn, res: s[0], exp: s[2])
      except: compileError("invalid transfer command")
      
      try:
        (GOTO, gotoBody): s[1]
      except: compileError("invalid goto command")
      
      try:
        (EMARK, IDENT, debugs): Cmd[string](kind: cmdDebug, name: s[1].name, debug: s[2])
      except: compileError("invalid debug breakpoint declaration")
      

    gotoBody[Cmd[string]]:
      try:
        IDENT: Cmd[string](kind: cmdJump, withCond: false, goto: s[0].name)

        (IF, exp, COMP, exp, THEN, ifTail):
          Cmd[string](
            kind:     cmdJump,
            withCond: true,
            cond:     (s[1], s[2].comp, s[3]),
            goto:     s[5][0],
            gotoElse: s[5][1]
          )

      except: compileError("invalid conditional jump")

    ifTail[(string, string)]:
      try:
        IDENT: (s[0].name, "")
        (IDENT, ELSE, IDENT): (s[0].name, s[2].name)

      except: compileError("expect label name")

    loc[Location]:
      try:
        LIT:                   Location(kind: locConst, val: s[0].val)
        IDENT:                 Location(kind: locReg, loc: s[0].name)
        (BRACL, IDENT, BRACR): Location(kind: locMem, loc: s[1].name)

      except: compileError("expected constant, register or memmory access with []")


    exp[Exp]:
      try:
        loc:             Exp(kind: expUn, unOp: opTransfer, loc: s[0])
        (UOP, loc):      Exp(kind: expUn, unOp: s[0].unOp,  loc: s[1])
        (loc, BOP, loc): Exp(kind: expBin, binOp: s[1].binOp, lhs: s[0], rhs: s[2])

      except: compileError("invalid expression")


    debugs[seq[DebugOut]]:
      try:
        (debug, COMMA, debugs): s[0] & s[2]
        debug: @[s[0]]

      except: compileError("expect register or memmory section")

    debug[DebugOut]:
      try:
        IDENT: DebugOut(kind: debugReg, name: s[0].name)
        (BRACL, BRACR):       DebugOut(kind: debugMem)
        (BRACL, DDOT, BRACR): DebugOut(kind: debugMem)
        (BRACL, LIT, DDOT, BRACR): DebugOut(kind: debugMem, a: some(s[1].val))
        (BRACL, DDOT, LIT, BRACR): DebugOut(kind: debugMem, b: some(s[2].val))
        (BRACL, LIT, DDOT, LIT, BRACR): DebugOut(kind: debugMem, a: some(s[1].val), b: some(s[3].val))

      except: compileError("expect register or memmory section")

  var errors = ""
  proc addError(msg: string) = errors &= "Error: " & msg & "\n"

  var i = 1
  for line in code.lines:
    var hashPos = line.find('#')
    if hashPos < 0: hashPos = len(line)
    let line = line[0 ..< hashPos]
    let lineStriped = line.strip
    if len(lineStriped) != 0:
      if lineStriped.startsWith('@'):
        let label = lineStriped[1..^1]
        if label in labels:
          addError fmt"{(i, 0)}: double definition of label '{label}'"
        labels[label] = len(prog)
      else:
        try: prog &= parseCmd(line, lexCmd)
        except LexingError as e:
          addError fmt"{(i, e.pos)}: unexpected charachter '{line[e.pos]}'"
        except ParseCmdError as e:
          addError fmt"{(i, e.pos)}: {e.msg}"
    inc i
  
  for i, cmds in prog:
    proc labelAddr(label: string): int =
      if label == "": i + 1
      else:
        if label notin labels:
          addError fmt"label '{label}' not defined"
        labels[label]

    result.add: collect:
      for cmd in cmds:
        case cmd.kind
        of cmdJump:
          var newCmd = Cmd[int](kind: cmdJump, withCond: cmd.withCond, goto: labelAddr(cmd.goto))
          if cmd.withCond:
            newCmd.cond = cmd.cond
            newCmd.gotoElse = labelAddr(cmd.gotoElse)
          newCmd
        
        of cmdAssgn: Cmd[int](kind: cmdAssgn, res: cmd.res, exp: cmd.exp)
        of cmdDebug: Cmd[int](kind: cmdDebug, name: cmd.name, debug: cmd.debug)

  if errors != "":
    echo errors
    quit 1


proc execute(prog: Prog, mem: string, wordSize: range[1 .. MaxWordSize], steps: bool): string =
  var
    mem = mem
    regs: Table[string, Val]
    pc = 0


  proc memSection(pos: Val): Slice[Val] =
    if pos < 0:
      echo fmt"Error: trying to access negative address {pos}"
      quit 1
    result.a = pos * wordSize
    result.b = result.a + wordSize - 1

  proc getMem(section: Slice[Val]): string =
    if section.a > high(mem):
      '\0'.repeat(wordSize)
    elif high(mem) in section:
      mem[section.a .. ^1] & '\0'.repeat(section.b - high(mem))
    else:
      mem[section]

  proc getMem(pos: Val): string =
    getMem(memSection(pos))

  proc setMem(pos: Val, data: string) =
    assert len(data) == wordSize
    let section = memSection(pos)
    if len(mem) < section.a:
      mem = mem & '\0'.repeat(section.a - len(mem)) & data
    elif len(mem) <= section.b:
      mem = mem[0 ..< section.a] & data
    else:
      mem = mem[0 ..< section.a] & data & mem[section.b + 1 .. ^1]
    

  proc asBytes(s: string): int64 =
    assert len(s) == wordSize
    var w: array[sizeof(int64), byte]
    for i, c in s:
      w[wordSize - i - 1] = byte(ord(c))
    # negative number 1 padding
    if (w[wordSize - 1] and 128) > 0:
      for i in wordSize ..< len(w):
        w[i] = 255
    cast[int64](w)

  proc asString(n: int64): string =
    let w = cast[array[sizeof(int64), byte]](n)
    for b in w[0 ..< wordSize]:
      result = char(b) & result


  proc get(loc: Location): Val =
    case loc.kind
    of locConst: loc.val
    of locReg: regs.getOrDefault(loc.loc)
    of locMem: getMem(regs.getOrDefault(loc.loc)).asBytes

  proc put(loc: Location, data: Val) =
    case loc.kind
    of locConst: assert false
    of locReg: regs[loc.loc] = data
    of locMem: setMem(regs.getOrDefault(loc.loc), data.asString)


  proc eval(exp: Exp): Val =
    case exp.kind
    of expUn:
      case exp.unOp
      of opTransfer: get(exp.loc)
      of opNot: not get(exp.loc)
    of expBin:
      let l = get(exp.lhs)
      let r = get(exp.rhs)
      case exp.binOp
      of opAdd: l + r
      of opSub: l - r
      of opMul: l * r
      of opDiv: l div r
      of opMod: l mod r
      of opAnd: l and r
      of opOr:  l or r
      of opXor: l xor r
      of opShl: l shl r
      of opShr: l shr r
      

  while pc < len(prog):
    var assigns = newSeq[(Location, int64)]()
    var didJump = false

    for cmd in prog[pc]:
      case cmd.kind
      of cmdAssgn:
        assigns &= (cmd.res, eval(cmd.exp))

      of cmdJump:
        pc =
          if cmd.withCond:
            let a = eval(cmd.cond[0])
            let b = eval(cmd.cond[2])
            if (
              case cmd.cond[1]
              of compEq: a == b
              of compNe: a != b
              of compLt: a <  b
              of compLe: a <= b
              of compGt: a >  b
              of compGe: a >= b
            ):
              cmd.goto
            else: cmd.gotoElse
          else: cmd.goto
        didJump = true

      of cmdDebug:
        func formatByte(c: char): string =
          fmt"{ord(c):0>2X} "
        echo cmd.name, ":"
        let indent = "  "
        for debug in cmd.debug:
          var dump = ""
          dump &= fmt"{indent}{debug}: "
          case debug.kind
          of debugReg:
            dump &= regs.getOrDefault(debug.name).asString.map(formatByte).join
          of debugMem:
            let
              a = if Some(@a) ?= debug.a: a * wordSize
                  else: 0
              b = if Some(@b) ?= debug.b: (b+1) * wordSize - 1
                  else: high(mem)
            for (i, c) in getMem(a..b).pairs:
              if i mod 4 == 0: dump &= "\n" & indent
              dump &= formatByte(c)
          echo dump, "\n"
        if steps: discard readLine(stdin)

    for (res, val) in assigns:
      put(res, val)

    if not didJump:
      inc pc

  mem


proc run(
  prog: string,
  input = "",
  output = "",
  wordSize: range[1..MaxWordSize] = min(4, MaxWordSize),
  steps = false
) =
  ## run a RT program
  let mem = if input == "": ""
            else: readFile(input)
  let res = execute(parse(open(prog)), mem, wordSize, steps)
  if output != "":
    writeFile(output, res)

proc assamble(prog: string, output = "") =
  ## resolve labels into absolute positions and remove comments / blank lines
  let code = $parse(open(prog))
  if output == "": echo code
  else: writeFile(output, code)

dispatchMulti(
  [run, help={
    "prog"  : "file containing the code to execute",
    "input" : "file containing initial state of RAM",
    "output": "file to write RAM to at end of execution",
    "steps" : "enable steps, so execution stops at every debug output. continue by pressing enter"
  }],
  [assamble, help={
    "prog"   : "file containing the code to assamble",
    "output" : "file to write assembled code to. if not given, assembled code is printed to console"
  }]
)