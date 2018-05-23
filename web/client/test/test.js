import { configure } from 'enzyme';
import Adapter from 'enzyme-adapter-react-16';

configure({ adapter: new Adapter() });

import './components/Button.test';
import './components/Radio.test';
import './components/Dropdown.test';
