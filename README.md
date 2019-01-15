# AQaml

AQaml (AnQou ocAML compiler) is yet another tiny self-hosted OCaml compiler
with an also tiny standard library.

## What is AQaml?

- AQaml is a self-hosted OCaml compiler i.e. AQaml can compile the source code of itself.
- AQaml has basic arithmetic operations, basic control expressions,
string, lists, tuples, variants, pattern matching,
recursive functions, closures, mutual recursion, references, etc.
- For now AQaml **doesn't** have currying, (complete) module system,
GC, type inference, class and many other features that are very important
but aren't needed to complete self-hosting.

## To Build and Test

Just `make`, which requires an OCaml compiler e.g. `ocamlopt`.
The output `aqaml` is the executable.

To test the AQaml's code, run `make test`,
or you can run `make self_test` or `make selfself_test`
to test the AQaml compiler (`_selfself_aqaml`)
compiled by the AQaml compiler (`_self_aqaml`)
compiled by the AQaml compiler (`aqaml`)
compiled by the official OCaml compiler (`ocamlopt`).

## FAQ

Frequently (, where #samples = 1,) asked questions.

The number of people who ask questions about AQaml is too small to decrease,
so I won't write this FAQ seriously.

### How to pronounce AQaml?

As you like. Though I call it like 'Ah-Caml', this is just an example.

### Why did you write AQaml?

Because it's fun.

### Why did you do it in actual?

Because it's a lot of fun.

### How did you make AQaml? Is there any trick?

Yarudake.

### Why have you written so many 'TODO:' in the AQaml's code?

Because AQaml is eien ni koujityuu.

## What is the license of AQaml?

MIT.

## I found a bug. Can I contribute to AQaml?

Yes of course! And I'm glad to hear that you built AQaml and ran it to find that bug.
Thank you very much. You can contribute to AQaml for example
by creating a pull request or an issue. I will be happy if
you attach a minimal example code to reproduce that bug.

## What is 'Naruto'?

It's a nerimono.

## What is 'NarutoNaruto'?

It's a nerimononerimono.

