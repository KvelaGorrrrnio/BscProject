import React from 'react';
import { Controlled as CodeMirror } from 'react-codemirror2';

const cmoptions = {
  readOnly: false,
  lineNumbers: true,
  indentUnit: 2,
  tabSize: 2,
  smartIndent: true,
  theme: 'editor-theme'
}

export default (<div>
  <h2>Reversible Language</h2>

  <h3>Variables and types</h3>
  <b>RL</b> is an explicitly strongly typed language in which every program starts by declaring the variables that it uses.
  Variables can only be declared at the start of the program, before any statements.
  There are two types in <b>RL</b>: <i>int</i> describing integers; <i>list t</i> describing lists, where t either is a nested <i>list</i> type or an <i>int</i> type.
  Thus we see that <b>RL</b> supports multidimensional integer lists.
  An example of variable declarations:
  <CodeMirror value={'list list int a\nlist int b\nlist int c\nint d\nint e'} options={cmoptions} />

  A variable is known by its <i>id</i>. Lists support indexing on already defined indices (pushed via push or allocated via init).
  <i>id</i> describes by default an identifier along with its indices. If <i>id</i> is described as root id, no indices are allowed.
  Indices start at 0.
  Following sample program shows the syntax of indexing.
  <CodeMirror value={'// N = [[1, 2, 3], [4, 5, 6], [7, 8, 9]\n\nN[1][0] += 2\n// N = [[1, 2, 3], [6, 5, 6], [7, 8, 9]\n\nswap N[1][1] N[0][1]\n// N = [[1, 6, 3], [2, 5, 6], [7, 8, 9]'} options={cmoptions} />

  <h3>Structures</h3>
  An <b>RL</b> program is structured into blocks. Each block consists of 3 parts:
  <table>
    <tbody>
      <tr>
        <td>Come-From assertion</td>
        <td>A come-from assertion can be one of 3 things: <b>entry</b>, which defines the entry point of the program. Only one entry point is allowed and must be placed in the first block of the program; <b>from</b>, which describes the previous block; <b>fi</b>, which uses a conditional to determine which of two blocks execution came from.</td>
      </tr>
      <tr>
        <td>Statements</td>
        <td>Statements are described further down.</td>
      </tr>
      <tr>
        <td>Jump</td>
        <td>A jump part can be one of 3 things:<br/>
        &nbsp;&nbsp;<b>exit</b>, which defines the exit point of the program. Only one exit point is allowed, and is recommended to be placed in the last block of the program;<br/>
        &nbsp;&nbsp;<b>goto</b>, which describes the next block;<br/>
        &nbsp;&nbsp;<b>if</b>, which uses a conditional to determine which of two blocks execution should jump to.</td>
      </tr>
    </tbody>
  </table>

  A sample program utilizing all above described froms and jumps:
  <CodeMirror value={'int n\n\nzero: entry\ngoto fst\n\nfst: fi n=0 zero snd\n  n+=1\nif n=0 snd trd\n\nsnd: from fst\ngoto fst\n\ntrd: from fst\nexit'} options={cmoptions} />

  <h3>Statements</h3>
  <b>RL</b> supports following statements
  <table>
    <tbody>
      <tr>
        <td style={{ width: '200px' }}><i>id</i> &oplus;= <i>exp</i></td>
        <td>
          Updates the variable <i>id</i>, by using the &oplus; operator on the current value of <i>id</i> and the evaluated value of <i>exp</i>.
          &oplus; can be either be + (addition), - (subtraction), * (multiplication), / (division) or ^ (xor).
        </td>
      </tr>
      <tr>
        <td>push <i>id<sub>1</sub></i> <i>id<sub>2</sub></i></td>
        <td>
          Pushes <i>id<sub>1</sub></i> on top of <i>id<sub>2</sub></i>. If <i>id<sub>1</sub></i> has type <i>t</i>, then <i>id<sub>2</sub></i> must have a type of <i>list t</i>.
        </td>
      </tr>
      <tr>
        <td>pop <i>id<sub>1</sub></i> <i>id<sub>2</sub></i></td>
        <td>
          Pops <i>id<sub>1</sub></i> off the top of <i>id<sub>2</sub></i>. If <i>id<sub>1</sub></i> has type <i>t</i>, then <i>id<sub>2</sub></i> must have a type of <i>list t</i>.
        </td>
      </tr>
      <tr>
        <td>swap <i>id<sub>1</sub></i> <i>id<sub>2</sub></i></td>
        <td>
          Swaps the contents of <i>id<sub>1</sub></i> and <i>id<sub>2</sub></i>. Both ids must have same type.
        </td>
      </tr>
      <tr>
        <td>skip</td>
        <td>
          A no-operation statement. Shorthand notation is a single dot (.).
        </td>
      </tr>
      <tr>
        <td>init <i>id</i> [<i>s<sub>0</sub></i>, <i>s<sub>1</sub></i>, ...]</td>
        <td>
          init takes a root id (no indices) of a variable with a <i>list</i> type, and a list of sizes, where the list size should match the number of dimensions that <i>id</i> has.<br/>
          If this is the case, then these dimensions are pushed onto <i>id</i>.
          E.g. a variable with type <i>list list list int</i> should have size list with 3 integers.
          <i>id</i> must be empty when using init, otherwise a runtime error is thrown.
        </td>
      </tr>
      <tr>
        <td>free <i>id</i> [<i>s<sub>0</sub></i>, <i>s<sub>1</sub></i>, ...]</td>
        <td>
          free takes a root id (no indices) of a variable with a <i>list</i> type, and a list of sizes, where the listsize should match the number of dimensions that <i>id</i> has.<br/>
          <i>id</i> must have same dimensions as specified in the size list, and all entries must be empty (0 or empty list).
          If this is the case, then <i>id</i> becomes an empty list.
          E.g. a variable with type <i>list list list int</i> should have size list with 3 integers.
        </td>
      </tr>
    </tbody>
  </table>

  <h3>Expressions</h3>
  Following describes possible expressions:
  <table>
    <tbody>
      <tr>
        <td style={{ width: '130px' }}><i>exp</i> + <i>exp</i></td>
        <td style={{ width: '170px' }}><i>int</i> &rarr; <i>int</i> &rarr; <i>int</i></td>
        <td>Addition</td>
      </tr>
      <tr>
        <td><i>exp</i> - <i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i> &rarr; <i>int</i></td>
        <td>Subtraction</td>
      </tr>
      <tr>
        <td><i>exp</i> * <i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i> &rarr; <i>int</i></td>
        <td>Multiplication</td>
      </tr>
      <tr>
        <td><i>exp</i> / <i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i> &rarr; <i>int</i></td>
        <td>Division</td>
      </tr>
      <tr>
        <td><i>exp</i> ^ <i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i> &rarr; <i>int</i></td>
        <td>Xor</td>
      </tr>
      <tr>
        <td><i>exp</i> % <i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i> &rarr; <i>int</i></td>
        <td>Modulo</td>
      </tr>
      <tr>
        <td><i>exp</i> ** <i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i> &rarr; <i>int</i></td>
        <td>Power</td>
      </tr>
      <tr>
        <td><i>exp</i> = <i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i> &rarr; <i>int</i></td>
        <td>Equal</td>
      </tr>
      <tr>
        <td><i>exp</i> != <i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i> &rarr; <i>int</i></td>
        <td>Not equal</td>
      </tr>
      <tr>
        <td><i>exp</i> &lt; <i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i> &rarr; <i>int</i></td>
        <td>Lesser than</td>
      </tr>
      <tr>
        <td><i>exp</i> &lt;= <i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i> &rarr; <i>int</i></td>
        <td>Lesser than or equal</td>
      </tr>
      <tr>
        <td><i>exp</i> &gt; <i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i> &rarr; <i>int</i></td>
        <td>Greater than</td>
      </tr>
      <tr>
        <td><i>exp</i> &gt;= <i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i> &rarr; <i>int</i></td>
        <td>Greater than or equal</td>
      </tr>
      <tr>
        <td><i>exp</i> && <i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i> &rarr; <i>int</i></td>
        <td>Logic and</td>
      </tr>
      <tr>
        <td><i>exp</i> || <i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i> &rarr; <i>int</i></td>
        <td>Logic or</td>
      </tr>

      <tr>
        <td>-<i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i></td>
        <td>Negate</td>
      </tr>
      <tr>
        <td>~<i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i></td>
        <td>Sign</td>
      </tr>
      <tr>
        <td>not <i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i></td>
        <td>Logic not</td>
      </tr>
      <tr>
        <td>size <i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i></td>
        <td>Size of list</td>
      </tr>
      <tr>
        <td># <i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i></td>
        <td>Size of list</td>
      </tr>
      <tr>
        <td>empty <i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i></td>
        <td>Check is exp is either [] or 0.</td>
      </tr>
      <tr>
        <td>?<i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i></td>
        <td>Check is exp is either [] or 0.</td>
      </tr>
      <tr>
        <td>top <i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i></td>
        <td>Get top element on list.</td>
      </tr>
      <tr>
        <td>^<i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i></td>
        <td>Get top element on list.</td>
      </tr>
      <tr>
        <td>null <i>exp</i></td>
        <td><i>int</i> &rarr; <i>int</i></td>
        <td>Checks if all entries of a list or scalar is empty.</td>
      </tr>
      <tr>
        <td><i>id</i></td>
        <td><i>t</i></td>
        <td>Variable lookup</td>
      </tr>
      <tr>
        <td><i>int</i></td>
        <td><i>int</i></td>
        <td>Integer literal</td>
      </tr>
    </tbody>
  </table>
</div>);
