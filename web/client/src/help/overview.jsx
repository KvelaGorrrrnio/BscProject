import React from 'react';
import helpIcon from '../images/icons/help.svg';
import tempIcon from '../images/icons/template.svg';
import openIcon from '../images/icons/open.svg';
import saveIcon from '../images/icons/save.svg';
export default (<div>
  <h2>Help for RL and SRL Web Interface</h2>
  <h3>Languages</h3>
  This web application is an interactive interface for two reversible programming languages, <b>SRL</b> and <b>RL</b>.
  Common for both languages is that they are primarily explicitly strongly typed with dynamic checks.
  For more information on syntax and the languages, refer to their individual help pages.

  <h3>Windows</h3>
  <b>Windows</b> are in the web interface regions that have certain features.
  There are 3 main <b>Windows</b>:
    <table>
      <tbody>
        <tr>
          <td><b>Code Window</b></td>
          <td>Left Bottom</td>
          <td>
            The <b>Code Window</b> contains the code editor.<br/>
            When errors occur, these are displayed in the <b>Error Window</b>, which are visible at the bottom of the <b>Code Window</b>.<br/>
            The <b>Error Window</b> is only visible when errors are present.
          </td>
        </tr>
        <tr>
          <td><b>Result Window</b></td>
          <td>Right Bottom</td>
          <td>
            The <b>Result Window</b> contains the result of the mode actions on your <b>RL</b>/<b>SRL</b> script.<br/>
            See the different modes for which actions are available and what the corresponding results are.
          </td>
        </tr>
        <tr>
          <td><b>Toolbar</b></td>
          <td>Right Bottom</td>
          <td>
            The <b>Toolbar</b> contains actions altering the state of the interface.
            See the following section to learn about the actions available.
          </td>
        </tr>
      </tbody>
  </table>

  <b>Toolbar actions</b><br/>
  The actions of <b>Toolbar</b> are divided into 4 groups.
    <table>
      <tbody>
        <tr>
          <td>Language Selection</td>
          <td>Left</td>
          <td>
            This action group has two actions <b>SRL</b> and <b>RL</b>. The highlighted action is the current language selected, and defines whether the code is interpreted as <b>SRL</b> or <b>RL</b> code.
          </td>
        </tr>
        <tr>
          <td>Help & IO</td>
          <td>Left Center</td>
          <td>
            This action group has 4 actions:
              <img className='icon inline' src={helpIcon}/> <b>Help</b>, which shows this help dialogue;
              <img className='icon inline' src={tempIcon}/> <b>Template</b>, which reveals a dropdown menu with template programs for both <b>RL</b> and <b>SRL</b>;
              <img className='icon inline' src={openIcon}/> <b>Open</b>, which reveals a dialogue for opening files from the browser or importing the code from a file on the local computer.
              <img className='icon inline' src={saveIcon}/> <b>Save</b>, which reveals a dialogue for saving files to the browser, sharing the script with the code embeded into a link or exporting/downloading the code as a file to the local computer.
          </td>
        </tr>
        <tr>
          <td>Mode Actions</td>
          <td>Right Center</td>
          <td>
            This action group contains actions associated with the current mode.
            See the individual modes for descriptions on their actions.
          </td>
        </tr>
        <tr>
          <td>Mode Selection</td>
          <td>Right Center</td>
          <td>
            This action group handles selection of interpretation modes. There are 4 modes: <br/>
              &nbsp;&nbsp;<b>Run</b>, which executes the code;<br/>
              &nbsp;&nbsp;<b>Step</b>, which executes the code step-by-step;<br/>
              &nbsp;&nbsp;<b>Invert</b>, which computes the inverse program;<br/>
              &nbsp;&nbsp;<b>Translate</b>, which translates between <b>RL</b> and <b>SRL</b>.<br/>
            For more information, refer to the individual modes.
          </td>
        </tr>
      </tbody>
  </table>

</div>);
