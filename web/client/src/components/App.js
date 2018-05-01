import React, { Component } from 'react';
import { Controlled as CodeMirror } from 'react-codemirror2';
import 'codemirror/lib/codemirror.css';
import './App.scss'

class Select extends Component {
  constructor(props) {
    super(props);
    this.state = { value: 'srl' };
  }
  change(event) {
    this.setState({value: event.target.value});
    this.props.change(event.target.value);
  }
  render() {
      return(
         <div>
             <select id="lang" onChange={this.change.bind(this)} value={this.state.value}>
                <option value="srl">SRL</option>
                <option value="rl">RL</option>
             </select>
         </div>
      );
  }
}

export default class App extends Component {
  constructor(props) {
    super(props);
    this.state = { code: 'a += 1', language: 'srl' };
    this.editor = null;
  }

  run() {
    //console.log("editor:", this.editor)
    //console.log("lineinfo",this.editor.doc.addLineClass(0, "wrap", "redish"));
     fetch('http://localhost:3001/api/run/' + this.state.language, {
       method: 'POST',
       body: JSON.stringify(this.state),
       headers: {
         "Content-Type": "application/json"
       },
     })
       .then(res => res.json())
       .then(
        (result) => {
          if (result.type=='variable') {
            console.log("Final state:")
            result.table.map(o => { console.log(o.id, JSON.parse(o.value)) });
          } else if (result.type=='error') {
            this.editor.doc.addLineClass(parseInt(result.position.line)-1, "wrap", "redish");
            console.log(result.message);
          } else {
            console.error("Expected response of type 'variable', but got '"+result.type+"'.");
          }
         },
         (error) => {
           console.error(error);
       });
  }

  setEditor(editor) {
    this.editor = editor;
  }

  render() {
    const options = {
      lineNumbers: true
    }
    return (
      <div>
        <button className="redish" onClick={this.run.bind(this)}>Run</button>
        <Select change={(value) => {
          this.setState({ language: value });
        }}
        />
      <CodeMirror
        editorDidMount={this.setEditor.bind(this)}
        value={this.state.code}
        options={options}
        onBeforeChange={(editor, data, code) => {
          this.setState({code});
        }}
        onChange={(editor, value) => {}}
      />
      </div>
    );
  }
}

