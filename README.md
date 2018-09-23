This package provides the replacement for the `show` and `print` functions which escape "non-printable" characters including every character with the ASCII code greater than 127. It is intended to be used with GHCi's `-interactive-print` option:

```
GHCi> :set -interactive-print=UnescapingPrint.uprint
GHCi> "Съешь ещё этих мягких французских булок, да выпей чаю"
"Съешь ещё этих мягких французских булок, да выпей чаю"
GHCi> "Vogt Nyx: »Büß du ja zwölf Qirsch, Kämpe!«"
"Vogt Nyx: »Büß du ja zwölf Qirsch, Kämpe!«"
GHCi> "Dès Noël où un zéphyr haï me vêt de glaçons würmiens je dîne d’exquis rôtis de bœuf au kir à l’aÿ d’âge mûr & cætera"
"Dès Noël où un zéphyr haï me vêt de glaçons würmiens je dîne d’exquis rôtis de bœuf au kir à l’aÿ d’âge mûr & cætera"
```

This approach works for (almost) any type with `Show` instance:
```
GHCi> (Just "Привет, ", 'м','и','р') 
(Just "Привет, ",'м','и','р')
```

This change could be made persistent via `$HOME/.ghci` file.