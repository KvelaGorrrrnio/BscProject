cd ../..
target=report/chapters/appendices/hsfiles.tex
commonfiles=$(find src/src/Common -type f -name *.hs | sort)
srlfiles=$(find src/src/SRL -type f -name *.hs | sort)
rlfiles=$(find src/src/RL -type f -name *.hs | sort)
rm -f "$target"
touch "$target"

# Common
echo "\\section{Common}" >> $target
echo "\\label{app:interpreter_common}" >> $target

for f in $commonfiles; do
  name=$(basename "$f")
  label=${name/\./_}
  echo "\\subsection{Common/${name/_/\_}}" >> $target
  echo "\\label{app:Common_$label}" >> $target
  echo "\\lstinputlisting[language=Haskell]{../$f}" >> $target
done

# SRL
echo "\\section{SRL}" >> $target
echo "\\label{app:interpreter_srl}" >> $target

for f in $srlfiles; do
  name=$(basename "$f")
  label=${name/\./_}
  echo "\\subsection{SRL/${name/_/\_}}" >> $target
  echo "\\label{app:SRL_$label}" >> $target
  echo "\\lstinputlisting[language=Haskell]{../$f}" >> $target
done

# RL
echo "\\section{RL}" >> $target
echo "\\label{app:interpreter_rl}" >> $target

for f in $rlfiles; do
  name=$(basename "$f")
  label=${name/\./_}
  echo "\\subsection{RL/${name/_/\_}}" >> $target
  echo "\\label{app:RL_$label}" >> $target
  echo "\\lstinputlisting[language=Haskell]{../$f}" >> $target
done

