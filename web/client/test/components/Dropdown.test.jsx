import React from 'react';
import { expect } from 'chai';
import sinon from 'sinon';
import { shallow } from 'enzyme';

import Dropdown from '../../src/components/Dropdown';

const dummyItems = [
  { type: 'item', title: 'a', },
  { type: 'item', title: 'b', onClick: sinon.spy() },
  { type: 'item', title: 'c', onClick: sinon.spy() },
  { type: 'item', title: 'd', onClick: sinon.spy() }
];

describe('Component: Dropdown', () => {

  it('renders without exploding', () => {
    const wrapper = shallow(<Dropdown/>);
    expect(wrapper).to.have.length(1);
  });

  it('Check default element', () => {
    const wrapper = shallow(<Dropdown/>);
    expect(wrapper.find('.dropdown li').get(0).props.children).to.equal('No items.');
  });

  it('click with onClick undefined', () => {
    const wrapper = shallow(<Dropdown items={dummyItems}/>);
    wrapper.find('.dropdown>ul').children().at(0).simulate('click');
  });

  it('click with onClick defined', () => {
    const wrapper = shallow(<Dropdown items={dummyItems}/>);
    wrapper.find('.dropdown>ul').children().at(1).simulate('click');
    expect(dummyItems[1].onClick.calledOnce).to.be.true;
  });

  it('disabled click with onClick undefined', () => {
    const wrapper = shallow(<Dropdown disabled={true} items={dummyItems}/>);
    wrapper.find('.dropdown>ul').children().at(0).simulate('click');
  });

  it('disabled click with onClick defined', () => {
    const wrapper = shallow(<Dropdown disabled={true} items={dummyItems}/>);
    wrapper.find('.dropdown>ul').children().at(1).simulate('click');
    expect(dummyItems[1].onClick.calledOnce).to.be.false;
  });

});
