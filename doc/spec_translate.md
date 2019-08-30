# Translation of the specification to IR

## Related files

- [ir/translate.rkt](../ir/translate.rkt)
- [ir/inst.rkt](../ir/inst.rkt)

## The algorithm

For an specification passed to the translate function, we first analyze the function using `find-missing-var`(see [spec_analyze.md](spec_analyze.md)) to decide if we need to generate action code. If there's any reference to the output stream, we should generate action code.

Then we would generate the input instructions, the compiler analyze the whole specification and find the maximum number of nested `prev`s for each input stream. Then translate them to `input` instructions.

Then for an instruction, if it refers to the output stream, we directly generate `ret-action` instruction, or we do the case analysis.

- `bind`: Generate `intro`/`intro-const` instruction to introduce the required inputs, then generate compute instructions to compute the value and bind it to the stream.
- `return`: First decide if we need `intro/intro-const` instructions, then generate `return` instruction.
- `if`: First decide if we need `intro/intro-const` instructions, then generate `filter` instruction. 
- `if-else`: First decide if we need `intro/intro-const` instructions, then generate `partition` instruction.
- `custom`: Generate `custom` instruction.
- `split`: First try to restructure the bindings, move all the constant bindings to the body. Then generate `split` actions. The body is translated by `translate-imperative`, which just find the `new-stream` or `empty-stream` inside the imperative part and translate.

After that, we would generate the final instruction for the results.

- If we are generating action code
  - If there's only one return action, replace it to `scan` instruction
  - Or we generate `merge-action` instruction
- Or
  - If there's only one return, directly use it as the result
  - Or we generate `merge` instruction.
