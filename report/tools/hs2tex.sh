cd ../..
target=report/chapters/appendices/hsfiles.tex
commonfiles=$(find src/src/Common -type f -name *.hs | sort)
srlfiles=$(find src/src/SRL -type f -name *.hs | sort)
rlfiles=$(find src/src/RL -type f -name *.hs | sort)
rm -f "$target"
touch "$target"

# Common
echo "\\subsection{Common Files}" >> $target
echo "\\label{app:Common_files}" >> $target

for f in $commonfiles; do
  name=$(basename "$f")
  label=${name/\./_}
  echo "\\subsubsection{Common/${name/_/\_}}" >> $target
  echo "\\label{app:Common_$label}" >> $target
  echo "\\lstinputlisting[language=Haskell]{../$f}" >> $target
done

# SRL
echo "\\subsection{SRL Files}" >> $target
echo "\\label{app:SRL_files}" >> $target

for f in $srlfiles; do
  name=$(basename "$f")
  label=${name/\./_}
  echo "\\subsubsection{SRL/${name/_/\_}}" >> $target
  echo "\\label{app:SRL_$label}" >> $target
  echo "\\lstinputlisting[language=Haskell]{../$f}" >> $target
done

# RL
echo "\\subsection{RL Files}" >> $target
echo "\\label{app:RL_files}" >> $target

for f in $rlfiles; do
  name=$(basename "$f")
  label=${name/\./_}
  echo "\\subsubsection{RL/${name/_/\_}}" >> $target
  echo "\\label{app:RL_$label}" >> $target
  echo "\\lstinputlisting[language=Haskell]{../$f}" >> $target
done

