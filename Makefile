TARGET=llint
SRCS=Main.hs 
SRCS+=Language/Lua/AST.hs
SRCS+=Language/Lua/Symbol.hs
SRCS+=Language/Lua/Show.hs
SRCS+=Language/Lua/Type.hs
SRCS+=Language/Lua/Semantics.hs
SRCS+=Language/Lua/Parser.hs
SRCS+=Language/Lua/Lint/Lint.hs
SRCS+=Language/Lua/Lint/Report.hs
SRCS+=Language/Lua/Lint/Rules.hs
SRCS+=Language/Lua/Lint/Env.hs
OBJS=$(SRCS:%.hs=%.o) $(SRCS:%.hs=%.hi)

.PHONY: clean test

all: $(TARGET)

$(TARGET): $(SRCS)
	ghc --make -O2 -rtsopts -with-rtsopts="-K64m" -o $@ $^

clean:
	$(RM) $(OBJS) $(TARGET)

install: $(TARGET)
	cp $(TARGET) /usr/local/bin
