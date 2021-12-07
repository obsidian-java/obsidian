# SSA Construction

An implementation of the SSA Construction for high level IR

# Code structure

## Pre-processing

1. Labeling - mandating the labels at each while, break and continue statements. `src/main/scala/obsidian/lang/java/Label.scala`
2. Flattening - flatten nested / composit assignment statement into multiple simple assignment statements. `src/main/scala/obsidian/lang/java/Flatten.scala`
3. Desugaring ``src/main/scala/obsidian/lang/java/Desugar.scala`

## SSA Construction with Cytron's algorithm

1. Control flow graph construction `src/main/scala/obsidian/lang/java/CFG.scala`
2. SSA construction `src/main/scala/obsidian/lang/java/SSACFG.scala` and `src/main/scala/obsidian/lang/java/CFG.scala`


## SSA Construction (a declarative approach described in the submitted paper)

1. SSA construction `src/main/scala/obsidian/lang/java/SSADL.scala`


# Test

Test cases can be found in the respective modules in `src/test/scala/obsidian/lang/java/`.

```
sbt test
```