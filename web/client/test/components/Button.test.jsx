import React from 'react';
import chai from 'chai';
import sinon from 'sinon';
import { shallow } from 'enzyme';

import Button from '../../src/components/Button';

const expect = chai.expect;

describe('Component: Button', () => {

  it('renders without exploding', () => {
    const wrapper = shallow(<Button/>);
    expect(wrapper).to.have.length(1);
  });

  it('button with 3 children', () => {
    const wrapper = shallow(<Button><img/><span></span><div></div></Button>);
    expect(wrapper.find('.button').children()).to.have.length(3);
  });

  it('click with onClick undefined', () => {
    const wrapper = shallow(<Button/>);
    wrapper.find('.button').simulate('click');
  });

  it('click with onClick defined', () => {
    const spyClick = sinon.spy();
    const wrapper = shallow(<Button onClick={spyClick}/>);
    wrapper.find('.button').simulate('click');
    expect(spyClick.calledOnce).to.be.true;
  });

  it('disabled click with onClick undefined', () => {
    const wrapper = shallow(<Button disabled={true} />);
    wrapper.find('.button').simulate('click');
  });

  it('disabled click with onClick defined', () => {
    const spyClick = sinon.spy();
    const wrapper = shallow(<Button disabled={true} onClick={spyClick}/>);
    wrapper.find('.button').simulate('click');
    expect(spyClick.calledOnce).to.be.false;
  });

})
