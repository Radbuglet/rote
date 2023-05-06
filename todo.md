# To-Do

## Internals

- [ ] Multiline strings break everything
- [ ] `Punct`s are not validated
- [ ] Invariants are not specified or validated
- [ ] Implementation of various format-adjacent systems isn't unified
- [x] Cursor-relative constructions break down if the origin line has expanded unexpectedly
- [ ] Cow implementation is suboptimal
- [ ] We need unicode support in literals
- [ ] We need support for comments
- [ ] Quoted tab violations need to be warned against

## API

- [ ] Quoting is very verbose and needs automated conversions
- [ ] Token stream insertion methods could be useful
- [ ] `rote!` should support more ways of quoting that don't require a surrounding block
- [ ] Literals need a way to be constructed and numeric literals need a simplified decoding interface.
- [ ] Modules should be reorganized and a prelude for quoting should be exposed

## Features

- [ ] Create a system to show white-spaces and token trees for debugging purposes
- [ ] Implement a parser for tokens
- [ ] Implement a parser for directives
