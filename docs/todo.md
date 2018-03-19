Del op i filer
Flet RL og parser sammen
RL skal ændres til at bruge State-monaden
Interaktivt interface
Test til parser og fortolker
Understøttelse af patenteser og flere operatorer til RL

Ordn SRL med nyt AST parser
Overvej at strippe result state, så det ikke indeholder zero-bindings.


Mappestruktur:
src
  rl.hs  (interface)
  srl.hs (interface)
  RL
    interp.hs
    ast.hs
    parser.hs
  SRL
    interp.hs
    ast.hs
    parser.hs
  common
    *expression*
    *vartab*
