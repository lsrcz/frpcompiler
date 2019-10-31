# Mutation Generation

For mutation analysis, some mutation operators are defined based on the syntax of the spec language.

## `js-expr`

The mutations could be done on any sub-expression of the whole `js-expr`. All `js-expr`s in the whold program could be mutated.

- Remove `prev` specifier. `(prev a) -> a`.
- Add `prev` specifier. `a -> (prev a)`.
- Change the value to a variable collected. `a -> b` where `b` is a variable available at the mutation position.

## `if`

- Remove the condition. `(if a branch) -> branch`.
- Flip the condition. `(if a branch) -> (if (not a) branch)`.

## `if-else`

- Remove the condition. `(if-else a then-branch else-branch) -> then-branch`.
- Flip the condition. `(if-else a then-branch else-branch) -> (if-else a else-branch then-branch)`.
- Remove one branch. `(if-else a then-branch else-branch) -> (if a then-branch)`.

## `if-multi`

- Mutate the mapping. `(if-multi (a b) branch mapping) -> (if-multi (a b) branch mapping')`.

## `if-else-multi`

- Mutate the mapping. `(if-else-multi (a b) then-branch else-branch mapping) -> (if-else-multi (a b) then-branch else-branch mapping')`.

## `case-multi`

- Mutate the mapping. `(case-multi (a b) (branch1 branch2) mapping) -> (case-multi (a b) (branch1 branch2) mapping1)`.

## `new-stream`

- Change to `empty-stream`.

## `new-stream-initial`

- Change to `empty-stream`.
- Change to `new-stream-seed`.

## `new-stream-seed`

- Change to `empty-stream`.
- Change to `new-stream-initial`.



