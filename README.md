# haskell-jusText

This is a haskell clone of the python [jusText](https://github.com/miso-belica/jusText) project. It is useful for removing boiler plate content from HTML pages leaving just the main content. jusText applies certain heuristics to identify the main content of the page. You can read more about it in the [thesis work](https://is.muni.cz/th/45523/fi_d/phdthesis.pdf) done by Jan Pomik´alek.

# Building

```
  stack install
  haskell-jusText <htmlFile> <stopwordsFile>
```

Stopword files for different languages are available in the [original repo](https://github.com/miso-belica/jusText/tree/dev/justext/stoplists).