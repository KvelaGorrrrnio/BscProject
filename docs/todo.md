Del op i filer
Flet RL og parser sammen
RL skal ændres til at bruge StateT-med-Except(Except InterpErr)-monaden
Interaktivt interface
Test til parser og fortolker
Understøttelse af patenteser og flere operatorer til RL
Ændr alle '.ToString' til instance af Show - eller ikke (indentering?)
Tilføj positioner til alle AST-elementer

Ordn SRL med nyt AST parser
Overvej at strippe result state, så det ikke indeholder zero-bindings.

Definer syntax highlighting


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
