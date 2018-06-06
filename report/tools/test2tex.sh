target=../chapters/appendices/testfiles.tex
srlfiles=$(find ../../src/test/suite -type f \( -name *.srl -o -name *.srl*.out \) | sort)
rlfiles=$(find ../../src/test/suite -type f \( -name *.rl -o -name *.rl*.out \) | sort)
suite_dir=../src/test/suite
rm -f $target
touch "$target"

# SRL
echo "\\section{SRL}" >> $target
echo "\\label{app:testing_srl}" >> $target

for f in $srlfiles; do
  name=$(basename "$f")
  label=${name/\./_}
  echo "\\subsection{${name/_/\_}}" >> $target
  echo "\\label{app:$label}" >> $target
  echo "\\lstinputlisting[language=rl]{${suite_dir}/${name}}" >> $target
done

# RL
echo "\\section{RL}" >> $target
echo "\\label{app:testing_rl}" >> $target

for f in $rlfiles; do
  name=$(basename "$f")
  label=${name/\./_}
  echo "\\subsection{${name/_/\_}}" >> $target
  echo "\\label{app:$label}" >> $target
  echo "\\lstinputlisting[language=rl]{${suite_dir}/${name}}" >> $target
done
