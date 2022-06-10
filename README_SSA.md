# SSA Construction

An implementation of the SSA Construction for high level IR

# Code structure

## Pre-processing

1. Labeling - mandating the labels at each while, break and continue statements. `src/main/scala/obsidian/lang/java/Label.scala`
2. Flattening - flatten nested / composit assignment statement into multiple simple assignment statements. `src/main/scala/obsidian/lang/java/Flatten.scala`
3. Desugaring ``src/main/scala/obsidian/lang/java/Desugar.scala`

## SSA Construction (a declarative approach described in the submitted paper)

1. SSA construction `src/main/scala/obsidian/lang/java/MinSSA.scala`
2. There is a minor difference in the implemntation compared to the version presented in the submitted paper, i.e. an SSA while statement consists two phi join clauses, one at the entry, the other at the exit. The exit phi is introduced for the support of continue and break statements in the future release. In this version, the exit phi copies the assignment from the entry phi. 


# Test

Test cases can be found in the respective modules in `src/test/scala/obsidian/lang/java/`.

```
sbt test
```