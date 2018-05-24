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
    const symRE = new RegExp(idRE.source + '|' + '[+=\\-*/%^?!~#<>|&]+');

    const keywords = function() {
      var kws = {};
      function setGroup(t) {
        return function () {
          for (var i = 0; i < arguments.length; i++)
            kws[arguments[i]] = t;
        };
      }

      setGroup('def')('list', 'int');

      setGroup('keyword')(
        'swap', 'push', 'pop', 'skip', '\.',
        'init', 'free'
      );

      setGroup('flow')(
        'if', 'then', 'else', 'fi',
        'from', 'do', 'loop', 'until',
        'goto', 'entry', 'exit'
      );

      setGroup('builtin')(
        '+', '-', '*', '/', '^', '%',
        '<', '>', '=', '!', '!=', '<=', '>=',
        '+=', '-=', '*=', '/=', '^=',
        '?', '#', '~'
      );

      // Exp builtins
      setGroup('builtin')(
        'top', 'size', 'null', 'not', 'and', 'or', 'empty'
      );
     
      return kws;

    }();

    function token(stream, state) {

      const c = stream.peek();
      // Eat whitespace
      if (stream.eatWhile(wsRE)) {
        return null;
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

      // Variables and stuff
      if (symRE.test(c)) {
        stream.match(symRE);
        return 'variable';
      }

      // Fallback
      stream.next();
    }

    return {
      token: function(stream, state){
        const t = token(stream,state);
        const w = stream.current();
        return w in keywords ? keywords[w] : t;
      },
    };
  });
  CodeMirror.defineMIME("text/x-[s]srl", "[s]rl");
});

