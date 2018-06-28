# Interface
This web application is an interactive interface for two reversible programming languages, **SRL** and **RL**.
Common for both languages is that they are primarily explicitly strongly typed with dynamic checks.
For more information on syntax and the languages, refer to their individual help pages ([SRL](./languages/#srl)|[RL](./languages/#rl)).
This section explains the interface, which features and functionality is embedded into it, and how to use it.

## Windows

**Windows** are in the web interface regions that have certain features.
There are 3 main **Windows**:
  <table>
    <tbody>
      <tr>
        <td>**Code Window**</td>
        <td>Left Bottom</td>
        <td>
          The **Code Window** contains the code editor.<br/>
          When errors occur, these are displayed in the **Error Window**, which are visible at the bottom of the **Code Window**.<br/>
          The **Error Window** is only visible when errors are present.
        </td>
      </tr>
      <tr>
        <td>**Result Window**</td>
        <td>Right Bottom</td>
        <td>
          The **Result Window** contains the result of the mode actions on your **RL**/**SRL** script.<br/>
          See the different modes for which actions are available and what the corresponding results are.
        </td>
      </tr>
      <tr>
        <td>**Toolbar**</td>
        <td>Right Bottom</td>
        <td>
          The **Toolbar** contains actions altering the state of the interface.
          See the following section to learn about the actions available.
        </td>
      </tr>
    </tbody>
</table>

####Toolbar actions
The actions of **Toolbar** are divided into 4 groups.
  <table>
    <tbody>
      <tr>
        <td>Language Selection</td>
        <td>Left</td>
        <td>
          This action group has two actions **SRL** and **RL**. The highlighted action is the current language selected, and defines whether the code is interpreted as **SRL** or **RL** code.
        </td>
      </tr>
      <tr>
        <td>Help & IO</td>
        <td>Left Center</td>
        <td>
          This action group has 4 actions:  
            ![Themes](img/themes.svg) **Themes**, which alters the colour scheme of the interface;  
            ![Help](img/help.svg) **Help**, which shows this help dialogue;  
            ![Template](img/template.svg) **Template**, which reveals a dropdown menu with template programs for both **RL** and **SRL**;  
            ![Open](img/open.svg) **Open**, which reveals a dialogue for opening files from the browser or importing the code from a file on the local computer.  
            ![Save](img/save.svg) **Save**, which reveals a dialogue for saving files to the browser, sharing the script with the code embeded into a link or exporting/downloading the code as a file to the local computer.  
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
            &nbsp;&nbsp;**Run**, which executes the code;<br/>
            &nbsp;&nbsp;**Step**, which executes the code step-by-step;<br/>
            &nbsp;&nbsp;**Invert**, which computes the inverse program;<br/>
            &nbsp;&nbsp;**Translate**, which translates between **RL** and **SRL**.<br/>
          For more information, refer to the individual modes.
        </td>
      </tr>
    </tbody>
</table>

## Modes

### Run
**Run** mode has one action ![Run](img/play.svg) **Run**, which executes the code in either the **RL** or **SRL** interpreter (defined by language choice in upper left corner).

The **Result Window** contains a table displaying the end state of the program execution.

### Step

  **Step** mode has one initial action ![Run](img/play.svg) **Run**. This activates the **Step** mode, which has following actions:
  <table>
    <tbody>
      <tr>
        <td>![Next step](img/step.svg)</td>
        <td>Next Step</td>
        <td>Available in **Step** mode. Moves to the next step operation in program.</td>
      </tr>
      <tr>
        <td>![Prev step](img/stepprev.svg)</td>
        <td>Prev Step</td>
        <td>Available in **Step** mode. Moves to the previous step operation in program.</td>
      </tr>
      <tr>
        <td>![Step to End](img/stepall.svg)</td>
        <td>Step To End</td>
        <td>Available in **Step** mode. Moves to the end of the program.</td>
      </tr>
      <tr>
        <td>![Reset execution](img/reset.svg)</td>
        <td>Reset Execution</td>
        <td>Available in **Step** mode. Moves to the beginning of the program.</td>
      </tr>
      <tr>
        <td>![End step](img/stop.svg)</td>
        <td>End Step</td>
        <td>Available in **Step** mode. Ends **Step** mode.</td>
      </tr>
    </tbody>
  </table>

When in **Step** mode, language selection, code-editing and storage-actions are disabled. Use ![End step](img/stop.svg) **End Step** to leave **Step** mode to re-enable these features.

In the **Result Window** two windows are shown. The upper half is the current state of the executing program.
The lower half shows a step operation log containing all executed step operations. The top of the log is the most recent executed step operation.

If the last step operation of the program is executed, either the last step operation and an error is shown with red highlighting on failure or the last step operation with green highlighting on success.

  Only runtime errors will be shown via the log. Parse errors and static errors will be shown in the **Error Window** (only visible when errors are present) below the **Code Window**.

### Invert

**Invert** mode has one action ![Invert](img/play.svg) **Invert**, which inverts the code through either the **RL** or **SRL** interpreter (defined by language choice in upper left corner).

The **Result Window** contains a read-only code area with the inverted program.

### Translate
**Translate** mode has one action ![Translate](img/play.svg) **Translate**, which translates the code through either the **RL** or **SRL** interpreter (defined by language choice in upper left corner).

The **Result Window** contains a read-only code area with the translated program.

