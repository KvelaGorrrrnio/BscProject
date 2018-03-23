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
    Interp.hs
    AST.hs
    Parser.hs
  SRL
    Interp.hs
    Ast.hs
    Parser.hs
  common
    *expression*
    *vartab*

Optimeringer:

- Sequential single-variable assignment
- Sequential swaps

- (Propagation (inlining ved udelukkende interp))
- One-to-one block removal
- If a = a l1 l2 => Goto l1 etc.

- ConstantFolding
- Expression Compression (a < b = true) (not not a)
- Redundant paranthesis
- Remove skips


Block sts
optim Block [] = Block [Skip]
optim Block sts = remove all skips => if []: optim (Block adas)
