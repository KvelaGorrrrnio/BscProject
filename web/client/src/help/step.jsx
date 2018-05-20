import React from 'react';
import playIcon from '../images/icons/play.svg';
import stepIcon from '../images/icons/step.svg';
import stepPrevIcon from '../images/icons/stepprev.svg';
import stepAllIcon from '../images/icons/stepall.svg';
import resetIcon from '../images/icons/reset.svg';
import stopIcon from '../images/icons/stop.svg';
export default (<div>
  <h2>Run Mode</h2>
  <h3>Actions</h3>
  <b>Step</b> mode has one initial action <img className='inline' src={playIcon}/> <b>Run</b>. This activates the <b>Step</b> mode, which has following actions:
  <table>
    <tbody>
      <tr>
        <td><img src={stepIcon}/></td>
        <td>Next Step</td>
        <td>Available in <b>Step</b> mode. Executes next statement in program.</td>
      </tr>
      <tr>
        <td><img src={stepPrevIcon}/></td>
        <td>Prev Step</td>
        <td>Available in <b>Step</b> mode. Reverse executes last statement in program.</td>
      </tr>
      <tr>
        <td><img src={stepAllIcon}/></td>
        <td>Step To End</td>
        <td>Available in <b>Step</b> mode. Executes all remaining statements in program.</td>
      </tr>
      <tr>
        <td><img src={resetIcon}/></td>
        <td>Reset Execution</td>
        <td>Available in <b>Step</b> mode. Reset execution state to before first statement.</td>
      </tr>
      <tr>
        <td><img src={stopIcon}/></td>
        <td>End Step</td>
        <td>Available in <b>Step</b> mode. Ends <b>Step</b> mode.</td>
      </tr>
    </tbody>
  </table>
  When in <b>Step</b> mode, language-selection, code-editing and storage-actions are disabled. Use <img className='inline' src={stopIcon} /> <b>End Step</b> to leave <b>Step</b> mode to re-enable these features.

  <h3>Result Window</h3>
  In the <b>Result Window</b> two windows are shown. The upper half is the current state of the executing program.
  The lower half shows a statement log, containing all executed statements. The top of the log is the most recent executed statement.<br/>
  If the last statement of the program is executed, either the last statement and an error is shown with red highlighting on failure or the last statement with green highlighting on success.<br/>
  Only runtime errors will be shown via the log. Parse and static errors will be shown in the <b>Error Window</b> (only visible when errors are present) below the <b>Code Window</b>.

</div>);
