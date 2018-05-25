import React from 'react';
import playIcon from '../images/icons/play.svg';
export default (<div>
  <h2>Run Mode</h2>
  <h3>Actions</h3>
  <b>Run</b> mode has one action <img className='icon inline' src={playIcon}/> <b>Run</b>, which executes the code in either the <b>RL</b> or <b>SRL</b> interpreter (defined by language choice in upper left corner).<br/>
  <h3>Result Window</h3>
  The <b>Result Window</b> contains a table displaying the end state of the program execution.
</div>);
