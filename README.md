llint
------

# How to build
```bash
$ brew install haskell-platform
$ make
```

# How to use
```bash
./llint a.lua b.lua c.lua
```
```bash
find . -name "*.lua" | xargs ./llint
```

# Analysis
 - global definition
 - masking global definition by local definition
 - duplicated local definition
 - unused local variale
 - too long function
 - redundant step index in for block
 - redundant conditional block
 - inconsistent syntax in table literal
 - empty control block
 - constant conditional expression
 - unused variable
 - use of undefined variable
 - modification of index variable inside loop

# Supported Lua Version
5.1 
5.2 (module is not fully supported)

# License
MIT
