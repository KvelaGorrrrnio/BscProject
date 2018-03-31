-- When changing anything;
--   AST types
--   Reversion
--   toString
--   evaluation/interpretation

## I første omgang
Interaktivt interface
Test til parser og fortolker
Tilføj positioner til alle AST-elementer
Kombiner det nuværende program med en State- og Reader-monade for henholdsvis AST og LabTab
Refaktorer RL-fortolkeren - måske bruge et Map til VarTab?
Brug type classes til at gøre Value-funktioner generiske: https://stackoverflow.com/questions/33983905/how-to-write-a-function-returns-either-integer-or-bool-based-on-a-user-defined-d
Nogle sanity checks:
  - Kun 1 entry og 1 exit
  - Enten skal programmet starte med en entry og slutte med en exit,
    eller vi skal skanne AST for entry og bevæge os til den givne blok før fortolkning
  - Bliver alle labels refereret?
  - Refereres nogle labels, som ikke eksisterer?
Overvej - og måske implementer - stadig følgende ting:
  - Måske skal AST også repræsenteres ved et Map i stedet for; altså [(Label,Block)]. På den måde kan man fortolke den enkelte blok og så bruge lookup i mappet for at gå til næste blok. Således behøves relative positioner ikke.
  - if (e) (b) else if (e) (b) else (b) - og tilsvarende
    fi (e) (b) else if (e) (b) else (b) - rent syntaktisk sukker og er ikke nødvendigt.
  - Måske et enkelt pas, som genererer en VarTab af de eksisterende variable med en default value til hver af dem afhængig af typen.
    Så kan den genererede VarTab fodres til fortolkeren. Fortolkeren kan i så fald fungere på normal vis - ikke noget med at inferere typerne ved runtime.
  - Logging? - Jeg har allerede implementeret et udkast, hvor vores primære state er (String,VarTab) i stedet for blot VarTab.
    Man kan således blot fylde meddelelseer på tilstanden løbende og så skrive det til en fil til sidst. Dette betyder dog, at hvis vi rammer en runtime error,
    så skal log-strengen overføres til fejlmeddelelsen, så loggen stadig kan skrives til en fil. Det ville næsten være bedre at have loggen i et separat state, så vi få den ud uanset om der er error eller ej.
  - Lidt i samme boldgade; Vil vi tillade at fortolke programmet step-by-step, så man kan se tilstanden til hver en tid? - Og hvordan vil man i så fald gøre det?


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
