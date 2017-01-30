It's mainly based on a tutorial from its author: https://mrkkrp.github.io/megaparsec/tutorials/parsing-simple-imperative-language.html

Still some issues/open questions:
- empty branch causes parsing error or tc error
- if we want to save comments in AST; megaparsec itself doesn't seem to support it out of the box
...

Test using `stack build --fast && stack exec parser`.
