-- When changing anything;
--   AST types
--   Reversion
--   toString
--   evaluation/interpretation

-- Split up in statements and expressions
-- TODO:
-- Maybe errors should be so too?
-- Expression errors:
--    type errors when applying operator
--    division by zero
--    division has rest
-- Instruction errors:
--    variable being assigned different type than expression
--    condition is not bool
--    assigned variable occurs in expression

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

Gør Expression, Vartab fælles


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




OBS!!!!
Enten SKAL rl-filer starte med en entry-block og slutte med en exit blok,

ellers skal vi lave et ekstra tjek inden vi starter programmet, så vi er sikre på at starte i entry-blokken.

Desuden må vi ikke tillade mere end 1 exit
