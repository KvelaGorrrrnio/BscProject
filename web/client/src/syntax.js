import * as CodeMirror from 'codemirror';

(function(mod) {
  mod(CodeMirror);
})(function(CodeMirror) {
"use strict";

  CodeMirror.defineMode("[s]rl", function(config, parserConfig) {
    const wsRE = /[ \t\v\f]/;
    const tpRE = /(int|list)/;
    const idRE = /[a-zA-Z][a-zA-Z0-9_']*/;
    const cmtRE = /\/\/.*/;
    const numRE = /[0-9]+/;
    const lblRE = /[0-9]+/;
    const alpRE = /[a-zA-Z]/;
    const opRE  = /[%?!~#=]|[+\-*/^<>]=?|&&|\|\|/;
    const notcmtperRE = /[^\*\/]/
    const cmtperRE = /\/\*|\*\//

    function token(stream, state) {

      const c = stream.peek();

      // block comment start
      if (c == '/') {
        if (stream.match(/\/\*/)) {
          state.blockComment++;
          return 'comment';
        } else if (state.blockComment > 0) {
          stream.next();
          return 'comment';
        }
      }

      // block comment end
      if (c == '*' && state.blockComment > 0) {
        if (stream.match(/\*\//)) {
          state.blockComment--;
        } else {
          stream.next();
        }
        return 'comment';
      }

      // In block comment
      if (state.blockComment > 0) {
          // Try to jump to next start or end
        while (stream.skipTo('*')) {
          // Check if begin
          stream.backUp(1);
          if (stream.peek() == '/') {
            break;
          }
          // Check if end
          stream.next();
          stream.next();
          if (stream.peek() == '/') {
            stream.backUp(1);
            break;
          }
        }
        if (stream.peek() != '*' && stream.peek() != '/') {
          stream.skipToEnd();
        }
        return 'comment';
      }

      // Eat whitespace
      if (stream.eatSpace()) {
        return null;
      }

      // String
      if (stream.match(/"[^"]*"/)) {
        return 'string'
      }

      // Comment
      if(stream.match(cmtRE)) {
        stream.skipToEnd();
        return 'comment';
      }

      // Brackets
      if (c == '(' || c == ')') {
        stream.next();
        return 'bracket';
      }

      if (numRE.test(c)) {
        stream.eatWhile(numRE);
        return 'number';
      }
      // Operators
      if (opRE.test(c)) {
        stream.match(opRE);
        return 'operator';
      }

      // Variables and stuff
      if (idRE.test(c)) {
        stream.match(idRE);
        if (stream.peek() == ':') {
          stream.next();
          return 'label';
        }
        return 'variable';
      }

      // Fallback
      stream.next();
    }

    const keywords = function() {
      var kws = {};
      function setGroup(t) {
        return function () {
          for (var i = 0; i < arguments.length; i++)
            kws[arguments[i]] = t;
        };
      }

      setGroup('type')('list', 'int', 'string');

      setGroup('keyword')(
        'swap', 'push', 'pop', 'skip', '\.',
        'init', 'free'
      );

      setGroup('flow')(
        'if', 'then', 'else', 'fi',
        'from', 'do', 'loop', 'until',
        'goto', 'entry', 'exit'
      );

      setGroup('operator')(
        '+', '-', '*', '/', '^', '%',
        '<', '>', '=', '!', '!=', '<=', '>=',
        '+=', '-=', '*=', '/=', '^=',
        '?', '#', '~'
      );

      // Exp operators
      setGroup('operator')(
        'top', 'size', 'null', 'not', 'and', 'or', 'empty'
      );

      return kws;

    }();

    return {
      startState: function() {
        return {
          blockComment: 0
        };
      },
      token: function(stream, state){
        const t = token(stream,state);
        const w = stream.current();
        return w in keywords && t != 'comment' ? keywords[w] : t;
      },
    };
  });
  CodeMirror.defineMIME("text/x-[s]srl", "[s]rl");
});

