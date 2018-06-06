cd ../../
target=./report/chapters/appendices/webfiles.tex
serverfiles=$(find web/server -maxdepth 1 -type f -name *.js | sort)
clientfiles=$(find web/client/src -type f \( -name *.js -o -name *.jsx \) | sort)
rm -f $target
touch "$target"

# Server
echo "\\section{Server}" >> $target
echo "\\label{app:web_server}" >> $target

for f in $serverfiles; do
  name=${f#web/server/}
  label=$(echo $name | sed -e 's/[/.-]/_/g')
  echo "\\subsection{${name/_/\_}}" >> $target
  echo "\\label{app:server_$label}" >> $target
  echo "\\lstinputlisting[language=javascript]{../$f}" >> $target
done

# Client
echo "\\section{Client}" >> $target
echo "\\label{app:web_client}" >> $target

for f in $clientfiles; do
  name=${f#web/client/}
  label=$(echo $name | sed -e 's/[/.-]/_/g')
  echo "\\subsection{${name/_/\_}}" >> $target
  echo "\\label{app:client_$label}" >> $target
  echo "\\lstinputlisting[language=javascript]{../$f}" >> $target
done
