import React from 'react';
import playIcon from '../images/icons/play.svg';
export default (<div>
  <h2>Translate Mode</h2>
  <h3>Actions</h3>
  <b>Translate</b> mode has one action <img className='inline' src={playIcon}/> <b>Translate</b>, which translates the code through either the <b>RL</b> or <b>SRL</b> interpreter (defined by language choice in upper left corner).<br/>
  <h3>Result Window</h3>
  The <b>Result Window</b> contains a read-only code area with the translated program.
</div>);
