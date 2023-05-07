# To-Do

## Internals

- [x] Cursor-relative constructions break down if the origin line has expanded unexpectedly
- [x] `Punct`s are not validated
- [ ] `Punct` rules are not enforced during normalization
- [ ] Make raw stream behavior more predictable (don't wrap everything in groups, for example)
- [ ] Diagnostic spans for `rote!` are oftentimes incorrect
- [ ] Comments are not yet supported
- [ ] Identifiers should be equated through normalization
- [ ] Multiline strings break everything
- [ ] Implementation of various format-adjacent systems isn't unified
- [ ] Cow implementation is suboptimal
- [ ] Normalization routine performance is suboptimal
- [ ] We need unicode support in literals
- [ ] We need support for comments
- [ ] Quoted tab violations need to be warned against
- [ ] Invariants and general whitespace semantics are not specified or validated

## API

- [x] Quoting is very verbose and needs automated conversions
- [ ] Token stream insertion methods could be useful
- [ ] `rote!` should support more ways of quoting that don't require a surrounding block
- [ ] Literals need a way to be constructed and numeric literals need a simplified decoding interface.
- [ ] Modules should be reorganized and a prelude for quoting should be exposed

## Features

- [ ] Create a system to show white-spaces and token trees for debugging purposes
- [ ] Implement a parser for tokens
- [ ] Implement a parser for directives
