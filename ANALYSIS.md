# PSL (Portable Standard Lisp) - Technical Analysis

A complete Lisp interpreter and compiler system implementing the Standard Lisp dialect.

## Architecture Overview

### Components

| Component | Purpose |
|-----------|---------|
| **Interpreter** (`lisp`) | Interactive Lisp execution |
| **Compiler** (`lispc`) | Compiles Lisp to C to native executable |
| **Build Tools** (`cr1`, `cr2`, `crc`, `cri`) | Generate C from Lisp sources |

### Directory Structure

```
PSL/
├── Source files (.c, .l, .lsp)
│   ├── Interpreter/Runtime: lisp-fn.l, lisp-zfn.l, yylex.l, big-n.l
│   ├── Compiler: compiler.lsp, lap.lsp
│   ├── Build Tools: cr1.c, cr2.c, crc.c, cri.c, sizes.c
│   └── Configuration: flags.l, types.l, hd.l, fnames.l, sysid.l, errors.l, zfnames.l
├── manual/
│   ├── lispman.pdf/tex - Implementation documentation
│   └── sl.pdf/tex - Standard Lisp specification
├── tests/
│   ├── test.lsp - Basic tests
│   └── BigNTest.lsp - Big number tests
├── Makefile
└── LICENSE.txt (BSD-like)
```

## Data Representation

### Type System (types.l)

All Lisp values are represented as pointers to structures with a type tag byte:

| Type Code | Name | Structure | Description |
|-----------|------|-----------|-------------|
| 0 | `Tpair` | `Xcar`, `Xcdr` | Cons cell (list node) |
| 1 | `Tid` | `Xvalue`, `Xproplist`, `Xpname`, `Xhashlink` | Symbol/identifier |
| 2 | `Tstring` | `Xstrelement` | String (pointer to char array) |
| 3 | `Tinteger` | `Xintval` (long) | Small integer |
| 4 | `Tbig` | `Xintval`, `Xcdr` | Big integer cell |
| 5 | `Tfloating` | `Xfloval` (double) | Floating point |
| 6 | `Tvector` | `Xupbv`, `Xvectelts`, `Xused` | Vector/array |
| 7 | `Tfpointer` | `Xargno`, `Xfnc` | Function pointer |
| 8 | `Terrmsg` | `Xerrorno`, `Xerrormsg` | Error message |
| 11 | `Tforwardadr` | `Xforwardaddress` | GC forwarding pointer |

### Symbol Structure (ID)

Symbols have:
- `Xtype` - Type tag (1 for Tid)
- `Xattr` - Attribute flags (or bit fields if BITF enabled)
- `Xhashlink` - Hash chain for oblist lookup
- `Xvalue` - Current value binding
- `Xproplist` - Property list
- `Xpname` - Print name (C string)

Attribute flags (when not using bit fields):
- Bit 0: `isinheap` - Allocated in heap
- Bit 1: `isglobal` - Global variable
- Bit 2: `isfluid` - Fluid (dynamically scoped) variable
- Bit 3: `isfunction` - Has function binding
- Bit 4: `isinoblist` - In oblist (interned)
- Bit 5: `isdclfluid` - Declared fluid

### Big Number Representation

Big integers use a linked list of cells, each holding up to 8 decimal digits:
- `BASE = 100000000` (10^8)
- First cell holds sign and length: positive length = positive number
- Subsequent cells hold digits in little-endian order (least significant first)

Example: -123456789012345 is stored as:
```
[-2] -> [89012345] -> [1234567]
```

### Memory Constants

```c
#define PAGESIZE    12000    /* Memory page size */
#define TXSIZE       121     /* Text buffer size */
#define DZ             8     /* Decimal digits per big number cell */
#define BASE   100000000L    /* 10^DZ */
#define BN    2147483647L    /* Max positive long int */
```

## Build System

### Build Process Flow

```
┌─────────────────────────────────────────────────────────────┐
│                        cr1 (cr1.c)                          │
│  Reads: flags.l, types.l, hd.l, fnames.l, sysid.l,         │
│         errors.l, lisp-fn.l                                 │
│  Writes: lisp1.c                                            │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                        cr2 (cr2.c)                          │
│  Reads: flags.l, types.l, hd.l, zfnames.l, yylex.l,        │
│         big-n.l, lisp-zfn.l                                 │
│  Writes: lisp2.c                                            │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                     GCC Compilation                         │
│  lisp1.o + lisp2.o → lisp executable                       │
└─────────────────────────────────────────────────────────────┘
```

### Generated Files

| Source | Generated | Contains |
|--------|-----------|----------|
| cr1 | lisp1.c | Symbol table, function registry, core Lisp functions |
| cr2 | lisp2.c | Auxiliary functions, tokenizer, big number arithmetic, GC |
| crc | lispc*.c | Compiled Lisp function modules |
| cri | LISP-INI | Initialization expressions |

### Configuration Files

| File | Purpose |
|------|---------|
| `flags.l` | Compiler flags (`#define BITF 0`, etc.) |
| `types.l` | Type definitions, structures, macros, includes |
| `hd.l` | Global variable declarations |
| `fnames.l` | Built-in function name/arity registry |
| `zfnames.l` | Auxiliary C function prototypes |
| `sysid.l` | System identifier definitions |
| `errors.l` | Error message table |

### Build Commands

```bash
make lisp      # Build interpreter
make lispc     # Build compiler
make clean     # Remove generated files
```

## Lexical Analyzer (yylex.l)

### Token Types Returned

| Return Value | Token Type |
|--------------|------------|
| `Tstring` | String literal `"..."` |
| `Tid` | Identifier/symbol |
| `Tfloating` | Floating point number |
| `Tinteger` | Small integer (≤8 digits) |
| `Tbig` | Big integer (>8 digits) |
| `Tfpointer` | Function pointer `#octal#` |
| `EOF` | End of input |

### Input Buffer Management

```c
#define INPUTBUFLEN 300
char input[INPUTBUFLEN];
char *curpos;   /* Current parse position */
char *lastpos;  /* Start of current token */
```

The `input_buf_save_table` allows saving/restoring input state when switching files (for `rds` function).

### Number Parsing

- `signflag` controls whether leading +/- is allowed
- Numbers with >8 digits automatically become big integers
- Floating point: `digits.digits[E[+-]digits]`
- Function pointers: `#octal#` format

## Core Functions (lisp-fn.l)

### Function Categories

**Arithmetic**: `plus`, `minus`, `times`, `quotient`, `remainder`, `add1`, `sub1`, `minus` (unary), `abs`, `max`, `min`

**Comparison**: `lessp`, `greaterp`, `leq`, `geq`, `eqn`, `neq`, `zerop`, `onep`, `minusp`

**List Operations**: `car`, `cdr`, `cons`, `list`, `append`, `reverse`, `length`, `nth`, `nthcdr`, `last`, `member`, `assoc`, `subst`, `copy`

**Composites**: `caar`, `cadr`, `cdar`, `cddr`, `caaar`, ... (all combinations)

**Type Predicates**: `atom`, `null`, `numberp`, `fixp`, `floatp`, `bigp`, `idp`, `pairp`, `stringp`, `vectorp`, `codep`

**Type Conversion**: `fix`, `float`, `id2string`, `string2id`, `intern`, `gensym`

**Control Flow**: `cond`, `prog`, `go`, `return`, `progn`, `prog1`, `prog2`

**Higher-Order**: `map`, `mapc`, `mapcar`, `mapcan`, `maplist`, `mapcon`, `apply`, `funcall`

**I/O**: `print`, `prin1`, `prin2`, `terpri`, `read`, `readch`, `open`, `close`, `rds`, `wrs`

**Property Lists**: `get`, `put`, `remprop`, `getd`, `putd`, `remd`

**Special**: `eval`, `set`, `setq`, `quote`, `function`, `de`, `df`, `dm`, `dn`

### Register Usage

Global registers for expression evaluation:
```c
PSEXP reg1, reg2, reg3;  /* General purpose */
PSEXP local0;            /* Local storage */
```

### Stack Operations (lisp-zfn.l)

```c
void kalloc(unsigned n);     /* Allocate n stack slots */
void kpop(unsigned n);       /* Pop n slots */
#define kset(i, x)           /* Set slot i to x */
#define local(i)             /* Get slot i */
```

## Memory Management (lisp-zfn.l)

### Garbage Collection

Uses a copying collector with forwarding pointers:

1. **Mark phase**: Traverse from roots, copy live objects to new space
2. **Forward**: Leave forwarding pointer (`Tforwardadr`) at old location
3. **Update**: Fix all pointers to use new locations
4. **Swap**: Exchange spaces

### Memory Allocation

```c
PSEXP zalloc(int tp);        /* Allocate object of type tp */
PPAGE zgetpage(int sz);      /* Get memory page of size sz */
void zinitpages(void);       /* Initialize page system */
```

### Page System

Memory organized into pages of `PAGESIZE` bytes. Different object types may use different page pools.

## Big Number Arithmetic (big-n.l)

### Operations

| Function | Operation |
|----------|-----------|
| `zmultiply()` | reg1 × reg2 → reg1 |
| `zdivision(p)` | Division with mode p (0=remainder, 1=quotient, 2=both) |
| `zaddsub(s)` | Addition (s=1) or subtraction (s=-1) |
| `zexpt()` | Exponentiation |
| `zbig2float()` | Convert big to double |
| `znormalize()` | Normalize result (demote to integer if small enough) |

### Arithmetic Arrays

```c
long *arit1, *arit2;         /* Working arrays */
unsigned arit1sz, arit2sz;   /* Array sizes */
void resize(long **p, unsigned *n, int l);  /* Resize if needed */
```

### Multiplication Algorithm

Uses schoolbook multiplication with BASE=10^8:
- Each cell holds 8 decimal digits
- Intermediate results use `umultiply()` for cell×cell
- Carries propagate through cells

## Error Handling

### Error Function

```c
void zerror(unsigned n, PSEXP a1, PSEXP a2, PSEXP a3);
```

Error codes defined in `errors.l`. Common errors:
- 8: Lexer error
- 41: Arithmetic overflow
- 42: Too many open files
- 44: Input buffer overflow

## Compiler (compiler.lsp, lap.lsp)

### Compilation Pipeline

1. Parse Lisp source
2. Transform to intermediate representation
3. Generate C code via LAP (Lisp Assembly Program)
4. Output to compn*, compc*, compx* files

### Compiled Function Format

Functions compiled to C with:
- Argument handling via stack
- Direct calls to runtime primitives
- Type dispatch for polymorphic operations

## Key Features

- Standard Lisp dialect implementation
- Multi-precision integer arithmetic
- Copying garbage collector
- Lisp-to-C compiler for standalone executables
- File I/O with input buffer save/restore
- Property lists on symbols
- Fluid (dynamic) variable binding
- FEXPR and MACRO support

## Documentation

- `manual/lispman.pdf` - Implementation internals
- `manual/sl.pdf` - Standard Lisp specification

## License

BSD-like license requiring attribution to original copyright holders.
