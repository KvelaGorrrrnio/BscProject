## I første omgang
Interaktivt interface
Test til parser og fortolker
(A) Tilføj positioner til alle AST-elementer
Nogle sanity checks:
  - Kun 1 entry og 1 exit.
    Vi skal skanne AST for entry og bevæge os til den givne blok før fortolkning
  - Bliver alle labels refereret?
  - Refereres nogle labels, som ikke eksisterer?
Overvej - og måske implementer - stadig følgende ting:
  - Måske skal AST også repræsenteres ved et Map i stedet for; altså [(Label,Block)]. På den måde kan man fortolke den enkelte blok og så bruge lookup i mappet for at gå til næste blok. Således behøves relative positioner ikke.
  - RL tjek from assertions
  - Type tjek
  - Optimering
  - Generer VarTab fra TypeTab
  - Lidt i samme boldgade; Vil vi tillade at fortolke programmet step-by-step, så man kan se tilstanden til hver en tid? - Og hvordan vil man i så fald gøre det?

!! Log til SRL virker ikke, da vi bruger show til statements

## Gør ting fælles
- invertStmt (Inversion.hs)
- Error
- HandleArgs.hs (giv programnavnet som argument?
- Main.hs ? grundlæggende forskel er navnene (SRL/RL)
- exec (statements) og eval (expressions) ?

## BESLUTNINGER
VarTab:
  - Type: HashMap
  - Bygges på forhånd: antag, at alle variable er bundet fra start

Log:
  - Egen datastruktur
  - Til JSON
  - Blot for exec og error indtil videre

Type:
  - Unknown type + unification
  - Build VarTab with default values from TypeTab
  - Terminerer ved første fejl

CLI: (Tjek CLI-thoughts.txt)
  - Interp
  - Invert interp
  - translate between srl/rl
  - (optimering)

checkFrom cur nfrom = -- for fi: evaluer exp og tjek om cur stemmer overens med nfrom
                      -- for from: blot tjek om de stemmer overns
                      -- entry: giv fejl.

## Når RL er done
Overfør RL til SRL - bør være ret easy
Overvej at strippe result state, så det ikke indeholder zero-bindings.

Gør Expression, Vartab fælles


## Optimeringer

Sequential single-variable assignment
Sequential swaps

(Propagation (inlining ved udelukkende interp))
One-to-one block removal
If a = a l1 l2 => Goto l1 etc.

ConstantFolding
Expression Compression (a < b = true) (not not a) ((e)) etc.
Redundant paranthesis
Remove skips
