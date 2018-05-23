import React from 'react';
import { expect } from 'chai';
import sinon from 'sinon';
import { shallow } from 'enzyme';

import Dropdown from '../../src/components/Dropdown';

const dummyItems = [
  { index: 0, title: 'a' }
];

describe('Component: Dropdown', () => {

  it('renders without exploding', () => {
    const wrapper = shallow(<Dropdown/>);
    expect(wrapper).to.have.length(1);
  });

}); 
