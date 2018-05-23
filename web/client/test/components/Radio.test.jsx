import React from 'react';
import { expect } from 'chai';
import sinon from 'sinon';
import { shallow } from 'enzyme';

import Radio from '../../src/components/Radio';

const dummyItems = [
  { index: 0, title: 'a' },
  { index: 1, title: 'b' },
  { index: 2, title: 'c' }
]

describe('Component: Radio', () => {

  it('renders without exploding', () => {
    const wrapper = shallow(<Radio/>);
    expect(wrapper).to.have.length(1);
  });

  it('default element', () => {
    const wrapper = shallow(<Radio/>);
    expect(wrapper.find('li').get(0).props.children).to.equal('default');
    expect(wrapper.find('li').get(0).key).to.equal('0');
  });

  it('3 items', () => {
    const wrapper = shallow(<Radio items={dummyItems}/>);
    expect(wrapper.children()).to.have.length(3);
    for (var i=0; i<dummyItems.length; i++) {
      expect(wrapper.find('li').get(i).props.children).to.equal(dummyItems[i].title);
      expect(wrapper.find('li').get(i).key).to.equal(dummyItems[i].index.toString());
    }
  });

  it('click with onChange undefined', () => {
    const wrapper = shallow(<Radio items={dummyItems}/>);
    wrapper.find('li').at(1).simulate('click');
  });

  it('click with onChange defined', () => {
    const changeSpy = sinon.spy();
    const wrapper = shallow(<Radio onChange={changeSpy} items={dummyItems}/>);
    wrapper.find('li').at(1).simulate('click');
    expect(changeSpy.calledOnce).to.be.true;
  });

  it('disabled click with onChange undefined', () => {
    const wrapper = shallow(<Radio disabled={true} items={dummyItems}/>);
    wrapper.find('li').at(1).simulate('click');
  });

  it('disabled click with onChange defined', () => {
    const changeSpy = sinon.spy();
    const wrapper = shallow(<Radio disabled={true} onChange={changeSpy} items={dummyItems}/>);
    wrapper.find('li').at(1).simulate('click');
    expect(changeSpy.calledOnce).to.be.false;
  });

})
